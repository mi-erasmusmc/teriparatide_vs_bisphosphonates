#!/usr/bin/env Rscript

# Description:
#   Calculates the negative control-calibrated hazard ratios per database:outcome
#   combination.
# Depends: 
#   data/raw/mappedOverallResultsNegativeControls.rds
#   data/raw/mappedOverallResults.rds
# Output:
#   data/processed/calibrateOverallResults.rds

library(tidyverse)

args = commandArgs(trailingOnly = TRUE)

if (args[1] == "att") {
  analType <- "itt_att_1095_q_25_75"
} else {
  analType <- "itt_1095_q_25_75"
}

fileName <- paste0(
    paste(
        "calibrateOverallResults",
        args[1],
        sep = "_"
    ),
    ".rds"
)

overallNegativeControls <- readRDS(
  "data/raw/mappedOverallResultsNegativeControls.rds"
) %>%
  dplyr::filter(
    analysisType == analType,
    database %in% args[-1]
  ) %>%
  dplyr::mutate(logRr = log(estimate))

overallMappedOverallRelativeResults <- readRDS(
  "data/raw/mappedOverallResults.rds"
) %>%
  dplyr::filter(
    analysisType == analType,
    database %in% args[-1]
  ) %>%
  mutate(logRr = log(estimate))

mod <- overallNegativeControls %>%
  split(.$database) %>%
  purrr::map(
    ~EmpiricalCalibration::fitSystematicErrorModel(
      logRr = .x$logRr,
      seLogRr = .x$seLogRr,
      trueLogRr = rep(0, nrow(.x))
    )
  )


overallMappedOverallRelativeResults %>%
  split(list(.$database, .$outcomeId), sep = ":") %>%
  purrr::map_dfr(
    ~EmpiricalCalibration::calibrateConfidenceInterval(
      logRr = .x$logRr,
      seLogRr = .x$seLogRr,
      model = mod[[.x$database]]
    ),
    .id = "id"
  ) %>%
  mutate(
    hr = exp(logRr),
    lower = exp(logLb95Rr),
    upper = exp(logUb95Rr)
  ) %>%
  tidyr::separate(id, c("database", "outcome"), ":") %>%
  saveRDS(
    file.path(
      "data/processed",
      fileName
    )
  )
