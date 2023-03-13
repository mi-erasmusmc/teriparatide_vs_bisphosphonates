#!/usr/bin/env Rscript

# Description:
#     Extracts the subset of analyses that estimated the absolute effects for hip
#     fracture within strata of predicted hip fracture risk
# Depends:
#     data/raw/mappedOverallAbsoluteResults.rds
# Output:
#     data/processed/hipFractureAbsolute.rds

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
protocol <- args[1]
estimand <- args[2]
analysis <- args[3]
args_stratOutcome <- as.numeric(args[4])

analType <- paste(protocol, estimand, analysis, sep = "_")


absoluteResults <- readRDS("data/raw/mappedOverallAbsoluteResults.rds") %>%
    tibble()

fileName <- paste0(
  paste(
    "hipFractureAbsolute",
    analType,
    sep = "_"
  ),
  ".rds"
)

absoluteResults %>%
    filter(
        analysisType == args_analysisType,
        stratOutcome == args_stratOutcome,
        estOutcome == args_estOutcome
    ) %>%
    saveRDS(
      file.path(
        "data/processed",
        fileName
      )
    )
