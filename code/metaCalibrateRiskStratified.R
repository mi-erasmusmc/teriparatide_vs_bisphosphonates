#!/usr/bin/env Rscript

# Description:
#     Generates meta-analytic estimates of negative control-calibrated estimates
#     for all outcomes in all risk quarters
# Depends:
#     data/processed/calibrateRiskStratified.rds
# Output:
#     data/processed/metaCalibrateRiskStratified.rds

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
            "calibrateRiskStratified",
            analType,
            sep = "_"
        ),
        ".rds"
    )
)

calibrateRiskStratified <- readRDS(fileDir) %>%
    filter(!(riskStratum == "Q2" & database == "ccae"))

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
