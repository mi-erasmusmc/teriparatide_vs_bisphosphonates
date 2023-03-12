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

library(tidyverse)

args = commandArgs(trailingOnly = TRUE)

if (args[1] == "att") {
  analType <- c(
    "itt_att_1095_risk_quarters",
    "itt_att_1095_q_25_75",
    "itt_att_1095_gl"
  )
} else {
  analType <- c(
    "itt_1095_risk_quarters",
    "itt_1095_q_25_75",
    "itt_1095_gl"
  )
}

fileName <- paste0(
  paste(
    "calibrateRiskStratified",
    args[1],
    sep = "_"
  ),
  ".rds"
)

negativeControls <- readRDS(
  "data/raw/negativeControls.rds"
) %>%
  dplyr::filter(
    analysisType %in% analType,
    database %in% args[-1]
  ) %>%
  dplyr::mutate(logRr = log(estimate))

relativeResults <- readRDS(
  "data/raw/mappedOverallRelativeResults.rds"
) %>%
  filter(
    analysisType %in% analType,
    database %in% args[-1]
  ) %>%
  mutate(
    logRr = log(estimate)
  )



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
