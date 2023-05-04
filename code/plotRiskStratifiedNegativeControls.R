#!/usr/bin/env Rscript

# =================================
# Description:
# Input:
# Output:
# Depends:
# =================================

library(tidyverse)
library(EmpiricalCalibration)
library(gridExtra)

args = commandArgs(trailingOnly = TRUE)
protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

if (analysis == "1095_custom") {
  stratification <- 5402
} else {
  stratification <- 5403
}

analType <- paste(protocol, estimand, analysis, sep = "_")

  databases <- c("ccae", "mdcr",  "optum_extended_dod", "optum_ehr")

analysisLabels <- paste0(c("ccae", "mdcr",  "optum_extended_dod", "optum_ehr"), "_itt")
databaseLabels <- c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")
plots <- list()

ff <- function(database, file) {
  readRDS(
    file.path(
      "data/raw",
      database,
      "shiny",
      paste0(
        file,
        "_",
        database,
        ".rds"
      )
    )
  )
}

relativeResults <- lapply(
  analysisLabels,
  ff,
  file = "mappedOverallRelativeResults"
) %>%
  bind_rows()

negativeControls <- lapply(
  analysisLabels,
  ff,
  file = "negativeControls"
) %>%
  bind_rows() %>%
  na.omit() %>%
  mutate(logRr = log(estimate))

fitNullWrapper <- function(data) {
  EmpiricalCalibration::fitNull(
    logRr = data$logRr,
    seLogRr = data$seLogRr
  )
}


nullDistributions <- negativeControls %>%
  group_by(database, riskStratum) %>%
  nest() %>%
  mutate(
    null = purrr::map(
      .x = data,
      .f = fitNullWrapper
    )
  ) %>%
  select(-data)

riskStrata <- paste0("Q", 1:2)
plots <- list()

left.grob <- grid::textGrob(
  "Standard error",
  rot = 90,
  gp = grid::gpar(fontsize = 18)
)

bottom.grob <- list(
  NULL,
  grid::textGrob(
    "Relative risk",
    just = "center",
    gp = grid::gpar(fontsize = 18)
  )
)

if (analysis == "1095_custom") {
  risk <- c("Hip fracture risk below 3%", "Hip fracture risk above 3%")
} else {
  risk <- c("Lower risk", "Higher risk")
}


for (i in seq_along(riskStrata)) {
  ncPlots <- list()
  right.grob <- grid::textGrob(
    risk[i],
    rot = 270,
    gp = grid::gpar(fontsize = 18)
  )
  for (j in seq_along(databases)) {
    negativeControlsSubset <- negativeControls %>%
      filter(
        riskStratum == riskStrata[i],
        database == databases[j],
        stratOutcome == stratification
      )
    null <- nullDistributions %>%
      filter(
        riskStratum == riskStrata[i],
        database == databases[j]
      ) %>%
      pull(null)
    ncPlots[[j]] <- plotCalibrationEffect(
      null = null[[1]],
      logRrNegatives = negativeControlsSubset$logRr,
      seLogRrNegatives = negativeControlsSubset$seLogRr,
      # logRrPositives = positiveResults$logRr,
      # seLogRrPositives = positiveResults$seLogRr,
      xLabel = ""
    ) +
      ggtitle(databaseLabels[j]) +
      theme(
        axis.text = element_text(size = 5, color = "red"),
        axis.title  = element_blank()
      )
    if (i == 2) {
      ncPlots[[j]] <- ncPlots[[j]] +
        theme(
          plot.title = element_text(size = 20, color = "white")
        )
    } else {
      ncPlots[[j]] <- ncPlots[[j]] +
        theme(
          plot.title = element_text(size = 20)
        )
    }
  }
  pp <- cowplot::plot_grid(
    ncPlots[[1]],
    ncPlots[[2]],
    ncPlots[[3]],
    ncPlots[[4]],
    nrow = 1
  )
  plots[[i]] <- grid.arrange(
    arrangeGrob(pp, left = left.grob, right = right.grob, bottom = bottom.grob[[i]])
  )
}

res <- cowplot::plot_grid(plots[[1]], plots[[2]], ncol = 1)

fileName <- paste0(
  paste(
    "riskStratifiedNcPlot",
    analType,
    sep = "_"
  ),
  ".pdf"
)
pdf(file.path("figures", fileName), width = 10, height = 8)
res
dev.off()
