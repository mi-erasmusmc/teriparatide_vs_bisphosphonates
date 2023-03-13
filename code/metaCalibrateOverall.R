#!/usr/bin/env Rscript

# Description:
#     Generates meta-analytic estimates of negative control-calibrated estimates
#     for all outcomes
# Depends:
#     data/processed/calibrateOverallResults.rds
# Output:
#     data/processed/calibrateMetaOverall.rds

library(tidyverse)

args = commandArgs(trailingOnly = TRUE)

protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

analType <- paste(protocol, estimand, analysis, sep = "_")

fileDir <- file.path(
  "data/processed",
  paste0(
    paste(
      "calibrateOverallResults",
      analType,
      sep = "_"
    ),
    ".rds"
  )
)

calibrateOverallResults <- readRDS(fileDir)

metaAnalysis <- function(data) {
  metaRes <- meta::metagen(
    TE = data$logRr,
    seTE = data$seLogRr,
    sm = "HR",
    method.tau = "PM",
    studlab = data$database
  )

  res <- tibble(
    hr = exp(metaRes$TE.random),
    lower = exp(metaRes$lower.random),
    upper = exp(metaRes$upper.random)
  )

  return(res)

}

fileName <- paste0(
  paste(
    "metaCalibrateOverall",
    analType,
    sep = "_"
  ),
  ".rds"
)

calibrateOverallResults %>%
  group_by(outcome) %>%
  nest() %>%
  mutate(var = map(data, ~metaAnalysis(.x))) %>%
  unnest(cols = var) %>%
  select(-data) %>%
  saveRDS(
    file.path(
      "data/processed",
      fileName
    )
  )
