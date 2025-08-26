# 自定义函数：绘制 Kaplan–Meier 曲线，并将主图导出为 PowerPoint 文件
plot_km_stage <- function(data_subset, stage_title, pptx_path, var_name, time_var, status_var, legend_labels = NULL) {
  
  # 1️⃣ 构造生存分析公式（Surv对象 + 分组变量）
  formula_obj <- as.formula(paste0("Surv(", time_var, ", ", status_var, ") ~ ", var_name))
  group_factor <- as.factor(data_subset[[var_name]])  # 提取分组因子

  # 🧾 打印传入的时间和事件变量名，方便调试
  message("⏱ 使用的生存时间变量为：", time_var)
  message("💀 使用的生存事件变量为：", status_var)
  print(formula_obj)

  # 2️⃣ 拟合 Cox 回归模型，提取 HR（风险比）和置信区间
  cox_model <- coxph(formula_obj, data = data_subset)
  summary_model <- summary(cox_model)
  HR <- round(summary_model$coefficients[1, "exp(coef)"], 2)
  CI_lower <- round(summary_model$conf.int[ , "lower .95"], 2)
  CI_upper <- round(summary_model$conf.int[ , "upper .95"], 2)
  HR_label <- paste0("HR = ", HR, " [", CI_lower, "–", CI_upper, "]")

  # 3️⃣ 进行 Log-rank 检验，得到 P 值
  logrank <- survdiff(formula_obj, data = data_subset)
  pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
  pval_label <- paste0("Log-rank P = ", signif(pval, 3))

  # 4️⃣ 使用 survfit() 拟合 KM 曲线，并转换为 summary 数据框
  fit <- survfit(formula_obj, data = data_subset)
  d <- survminer::surv_summary(fit, data = data_subset)

  # 5️⃣ 使用 ggsurvplot_df() 绘制 KM 曲线（适配新版 survminer，避免报错）
  km_plot <- ggsurvplot_df(
    d,
    legend.labs = if (is.null(legend_labels)) levels(group_factor) else legend_labels,           # 图例标签
    legend = "none",  # 👈 关闭图例
    xlab = "Years After Surgery",                 # 横坐标标签
    ylab = "Survival Probability",                # 纵坐标标签
    break.x.by = 1,                               # 横轴刻度间隔
    break.y.by = 0.2,                             # 纵轴刻度间隔
    palette = c("black", "grey50"),               # 曲线颜色
    linetype = c("solid", "solid"),              # 曲线线型
    size = 1.0,
    censor = TRUE,
    censor.shape = "+",
    censor.size = 2,
    ggtheme = theme_bw() +
      theme(
        text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # ❌ 去除整体边框
        axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),  # ✅ 仅保留底部横轴
        axis.line.y.left   = element_line(color = "black", linewidth = 0.5),  # ✅ 仅保留左侧纵轴
        axis.line.x.top    = element_blank(),  # ❌ 去除顶部边线
        axis.line.y.right  = element_blank(),  # ❌ 去除右侧边线
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)
      )
  )

  # 6️⃣ 在主图上添加 HR、P 值 和 标题注释
  km_plot <- km_plot +
    annotate("text", x = 4, y = 0.9, label = HR_label, size = 5, family = "Times New Roman") +
    annotate("text", x = 4, y = 0.8, label = pval_label, size = 5, family = "Times New Roman") +
ggtitle(paste0(
  stage_title,
  " (n=", sum(!is.na(data_subset[[var_name]])), ")"
)) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Times New Roman", face = "bold"))

# 获取每组的样本数，把分组变量设为 factor，并指定 levels 顺序
# 将 0 / 1 映射为 legend_labels 中的名称
group_raw <- as.character(data_subset[[var_name]])
group_factor <- factor(ifelse(group_raw == "1", legend_labels[2],
                              ifelse(group_raw == "0", legend_labels[1], NA)),
                       levels = legend_labels)
# 🧪 调试输出：显示原始变量分布
message("🔍 原始分组变量（", var_name, "）值分布：")
print(table(data_subset[[var_name]], useNA = "always"))

# 🧪 调试输出：显示映射后 group_factor 的分布（看看有没有 NA）
message("🔍 映射后的分组标签分布：")
print(table(group_factor, useNA = "always"))

# 然后 table 会按 legend_labels 的顺序来统计
group_counts <- table(group_factor)

# 生成注释文字
group_labels <- paste0(legend_labels, " (n=", group_counts, ")")

# 添加到图中（位置可根据需要调整）
  km_plot <- km_plot +
      annotate("text", x = 1, y = 0.3, label = group_labels[1], size = 5, family = "Times New Roman", hjust = 0) +
      annotate("text", x = 1, y = 0.2, label = group_labels[2], size = 5, family = "Times New Roman", hjust = 0)

  # 7️⃣ 写入 PowerPoint 文件，仅输出主图（不含风险表）
  plot_main <- km_plot
  doc <- read_pptx()
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, dml(ggobj = plot_main), location = ph_location(left = 1, top = 1, width = 4, height = 4))
  print(doc, target = pptx_path)
  message("✅ PowerPoint saved at: ", pptx_path)

  # 返回 ggplot 对象（方便后续展示或另存）
  return(km_plot)
}
