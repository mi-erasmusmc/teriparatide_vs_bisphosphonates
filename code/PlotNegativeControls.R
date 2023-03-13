#!/usr/bin/env Rscript

# =================================
# Description:
# Input:
# Output:
# Depends:
# =================================

library(tidyverse)
library(EmpiricalCalibration)

args = commandArgs(trailingOnly = TRUE)
protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

if (analysis == "1095_custom_10") {
  stratification <- 5402
} else {
  stratification <- 5403
}

analType <- paste(protocol, estimand, analysis, sep = "_")

databases <- c("ccae", "mdcr",  "optum_extended_dod", "optum_ehr")
databaseLabels <- c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")
mappedOverallResults <- readRDS("data/raw/mappedOverallResults.rds")
overallNegativeControls <- readRDS("data/raw/mappedOverallResultsNegativeControls.rds")
plots <- list()

for (i in seq_along(databases)) {
  positiveResults <- mappedOverallResults %>%
    filter(
      database == databases[i],
      analysisType == analType,
      outcomeId == stratification
    ) %>%
    mutate(logRr = log(estimate))

  negativeControls <- overallNegativeControls %>%
    filter(
      database == databases[i],
      analysisType == analType,
    ) %>%
    mutate(logRr = log(estimate))

  null <- fitNull(
    logRr = negativeControls$logRr,
    seLogRr = negativeControls$seLogRr)

  plots[[i]] <- plotCalibrationEffect(
    null = null,
    logRrNegatives = negativeControls$logRr,
    seLogRrNegatives = negativeControls$seLogRr,
    logRrPositives = positiveResults$logRr,
    seLogRrPositives = positiveResults$seLogRr,
    xLabel = ""
  ) + ggtitle(databaseLabels[i]) + theme(
      plot.title = element_text(size = 38),
      axis.text.x = element_text(size = 22),
      axis.title.x  = element_text(size = 30),
      )
  if (i == 1) {
    plots[[i]] <- plots[[i]] +
      theme(axis.title.y = element_text(size = 30))
  } else {
    plots[[i]] <- plots[[i]] +
      theme(
        axis.title.y = element_blank()
      )
  }
}

# res <- gridExtra::grid.arrange(plot[[1]], plot[[2]], plot[[3]], nrow = 1)
res <- cowplot::plot_grid(
                  plots[[1]],
                  plots[[2]],
                  plots[[3]],
                  plots[[4]],
                  nrow = 1
                ) +
  cowplot::draw_label("Relative risk", x = .5, y = 0, vjust = -.5, size = 30)

fileName <- paste0(
  paste(
    "overallNcPlot",
    analType,
    sep = "_"
  ),
  ".tiff"
)
ggsave(
  file.path("figures", fileName),
  plot = res,
  dpi = 400,
  width = 700,
  height = 350,
  units = "mm",
  compression = "lzw+p"
)
