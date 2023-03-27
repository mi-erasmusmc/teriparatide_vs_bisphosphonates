#!/usr/bin/env Rscript

# ==============================================================================
# Description:
#   Calculates the negative control-calibrated hazard ratios per
#   database:outcome combination.
# Depends:
#   data/raw/negativeControls.rds
#   data/raw/mappedOverallRelativeResults.rds
# Output:
#   data/processed/calibrateRiskStratified.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

args = commandArgs(trailingOnly = TRUE)

protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

message(rep("=", 80))
message(crayon::bold("SETTINGS"))
message(paste0("args_protocol:  ", protocol))
message(paste0("args_estimand:  ", estimand))
message(paste0("args_analysis:  ", analysis))
message(rep("=", 80))

analType <- paste(protocol, estimand, analysis, sep = "_")

fileName <- paste0(
  paste(
    "calibrateRiskStratified",
    analType,
    sep = "_"
  ),
  ".rds"
)

negativeControls <- readRDS(
  "data/raw/negativeControls.rds"
) %>%
  dplyr::filter(
    analysisType %in% analType,
    database %in% args
  ) %>%
  dplyr::mutate(logRr = log(estimate))

relativeResults <- readRDS(
  "data/raw/mappedOverallRelativeResults.rds"
) %>%
  filter(
    analysisType %in% analType,
    database %in% args[-(1:3)]
  )

if (analysis == "1095_custom") {
  negativeControls <- negativeControls %>%
    filter(!(riskStratum == "Q2" & database == "ccae"))
}

negativeControls <- negativeControls %>%
  mutate(logRr = log(estimate))

systErrorModels <- negativeControls %>%
  filter(!is.na(seLogRr)) %>%
  group_by(analysisType, database, stratOutcome, riskStratum) %>%
  nest() %>%
  mutate(
    mod = map(
      data,
      ~EmpiricalCalibration::fitSystematicErrorModel(
        logRr     = .x$logRr,
        seLogRr   = .x$seLogRr,
        trueLogRr = rep(0, nrow(.x))
      )
    )
  ) %>%
  select(-data)

tibble(relativeResults) %>%
  mutate(estimate = log(estimate)) %>%
  inner_join(systErrorModels) %>%
  group_by(analysisType, database, stratOutcome, estOutcome, riskStratum) %>%
  nest() %>%
  mutate(
    pp = map(
      data,
      ~EmpiricalCalibration::calibrateConfidenceInterval(
        logRr   = .x$estimate,
        seLogRr = .x$seLogRr,
        model   = .x$mod[[1]]
      )
    )
  ) %>%
  unnest(pp) %>%
  select(-data) %>%
  saveRDS(
    file.path(
      "data/processed",
      fileName
    )
  )

message(
  crayon::green(
    paste(
      "\u2713 File saved at:",
      file.path("data/processed", fileName),
      "\n"
    )
  )
)
