#!/usr/bin/env Rscript

# Description:
#     Extracts the subset of analyses that estimated the absolute effects for hip
#     fracture within strata of predicted hip fracture risk
# Depends:
#     data/raw/mappedOverallAbsoluteResults.rds
# Output:
#     data/processed/hipFractureAbsolute.rds
    
args <- commandArgs(trailingOnly = TRUE)    
args_stratOutcome <- args[1]
args_estOutcome <- args[2]
args_analysisType <- args[3]

library(tidyverse)

absoluteResults <- readRDS("data/raw/mappedOverallAbsoluteResults.rds") %>%
    tibble()

fileName <- paste0(
  paste(
    "hipFractureAbsolute",
    args_analysisType,
    args_stratOutcome,
    args_estOutcome,
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
