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

protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

analType <- paste(protocol, estimand, analysis, sep = "_")


fileName <- paste0(
    paste(
        "calibrateOverallResults",
        analType,
        sep = "_"
    ),
    ".rds"
)

overallNegativeControls <- readRDS(
  "data/raw/mappedOverallResultsNegativeControls.rds"
) %>%
  dplyr::filter(
    analysisType == analType,
    database %in% args[-(1:3)]
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
