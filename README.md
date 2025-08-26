# KMplot-survival-analysis
绘制 Kaplan–Meier 曲线，并将主图导出为 PowerPoint 文件
# Kaplan–Meier Plot Generator for R (KMplot)

This script defines a custom function `plot_km_stage()` to generate and export survival curves (Kaplan–Meier) with Cox regression HR and log-rank p-values.

## Features
- Customizable time/status variables
- Auto HR and P-value annotation
- PowerPoint export for publication
- Support multi-stage plotting

## Usage

```r
source("plot_km_stage.R")
km_stage1 <- plot_km_stage(
  data_subset = subset(Data, st == 1),
  stage_title = "Stage I",
  pptx_path = "StageI_KM.pptx",
  var_name = "MyVariable",
  time_var = "OS_years",
  status_var = "death_event",
  legend_labels = c("Negative", "Positive")
)
