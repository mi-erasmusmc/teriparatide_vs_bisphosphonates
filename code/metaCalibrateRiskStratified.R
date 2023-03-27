#!/usr/bin/env Rscript

# Description:
#     Generates meta-analytic estimates of negative control-calibrated estimates
#     for all outcomes in all risk quarters
# Depends:
#     data/processed/calibrateRiskStratified.rds
# Output:
#     data/processed/metaCalibrateRiskStratified.rds

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

fileDir <- file.path(
  "data/processed",
  paste0(
    paste(
      "calibrateRiskStratified",
      analType,
      sep = "_"
    ),
    ".rds"
  )
)

calibrateRiskStratified <- readRDS(fileDir)
if (analysis == "1095_custom") {
  calibrateRiskStratified <- calibrateRiskStratified %>%
    filter(!(riskStratum == "Q2" & database == "ccae"))
}

metaAnalysisRiskStratified <- function(data) {

  metaRes <- meta::metagen(
    TE         = data$logRr,
    seTE       = data$seLogRr,
    sm         = "HR",
    method.tau = "PM",
    studlab    = data$database
  )

  res <- tibble(
    hr    = exp(metaRes$TE.random),
    lower = exp(metaRes$lower.random),
    upper = exp(metaRes$upper.random)
  )

  return(res)
}


fileName <- paste0(
  paste(
    "metaCalibrateRiskStratified",
    analType,
    sep = "_"
  ),
  ".rds"
)

calibrateRiskStratified %>%
  group_by(analysisType, stratOutcome, estOutcome, riskStratum) %>%
  nest() %>%
  mutate(
    meta = map(
      data,
      ~metaAnalysisRiskStratified(.x)
    )
  ) %>%
  unnest(meta) %>%
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
