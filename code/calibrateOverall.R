#!/usr/bin/env Rscript

# Description:
#   Calculates the negative control-calibrated hazard ratios per database:outcome
#   combination.
# Depends:
#   data/raw/mappedOverallResultsNegativeControls.rds
#   data/raw/mappedOverallResults.rds
# Output:
#   data/processed/calibrateOverallResults.rds

suppressPackageStartupMessages({
  library(tidyverse)
})


args = commandArgs(trailingOnly = TRUE)

protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

message(rep("=", 80))
message(crayon::bold("SETTINGS"))
message(paste0("args_protocol:   ", protocol))
message(paste0("args_estimand:   ", estimand))
message(paste0("args_analysis:   ", analysis))
message(paste0("args_databases:  ", paste(args[-(1:3)], sep = ", ", collapse = ", ")))
message(rep("=", 80))

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
    database %in% args[-(1:3)]
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

message(
  crayon::green(
    paste(
      "\u2713 File saved at:",
      file.path(
        "data/processed",
        fileName
      ),
      "\n"
    )
  )
)
