#!/usr/bin/env Rscript

# =================================
# Description:
# Input:
# Output:
# Depends:
# =================================

library(tidyverse)
library(EmpiricalCalibration)

databases <- c("ccae", "mdcr", "optum_extended_dod", "optum_ehr")
databaseLabels <- c("CCAE", "MDCR","Optum-DOD", "Optum-EHR")
mappedOverallResults <- readRDS("data/raw/mappedOverallResults.rds")
overallNegativeControls <- readRDS("data/raw/mappedOverallResultsNegativeControls.rds")
plot <- list()

for (i in seq_along(databases)) {
  positiveResults <- mappedOverallResults %>%
    filter(
      database == databases[i],
      analysisType == "itt_att_1095_gl",
      outcomeId == 101
    ) %>%
    mutate(logRr = log(estimate))
  
  negativeControls <- overallNegativeControls %>%
    filter(
      database == databases[i],
      analysisType == "itt_att_1095_gl",
    ) %>%
    mutate(logRr = log(estimate))
  
  null <- fitNull(
    logRr = negativeControls$logRr,
    seLogRr = negativeControls$seLogRr)
  
  plot[[i]] <- plotCalibrationEffect(
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
    plot[[i]] <- plot[[i]] +
      theme(axis.title.y = element_text(size = 30))
  } else {
    plot[[i]] <- plot[[i]] +
      theme(
        axis.title.y = element_blank()
      )
  }
}

# res <- gridExtra::grid.arrange(plot[[1]], plot[[2]], plot[[3]], nrow = 1)
res <- cowplot::plot_grid(
                  plot[[1]],
                  plot[[2]],
                  plot[[3]],
                  plot[[4]],
                  nrow = 1
                ) +
  cowplot::draw_label("Relative risk", x = .5, y = 0, vjust = -.5, size = 30)

ggsave(
  file.path("figures/OverallNcPlot.tiff"),
  plot = res,
  dpi = 600,
  width = 700,
  height = 350,
  units = "mm",
  compression = "lzw"
)
