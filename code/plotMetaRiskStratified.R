#!/usr/bin/env Rscript

# Description:
#   Generates the plot regarding the meta-analysis of the calibrated hazard
#   ratios for the three outcomes
# Depends:
#   data/raw/map_outcomes.rds
#   data/processed/calibrateOverallResults.rds 
#   data/processed/metaCalibrateOverall.rds
# Output:
#   figures/plotMetaRiskStratifed_xxx.pdf

library(tidyverse)
library(scales)

args = commandArgs(trailingOnly = TRUE)
suffix <- args[1]
analysis <- args[2]
stratification <- as.numeric(args[3])

map_outcomes <- readRDS(
  "data/raw/map_outcomes.rds"
)

calibrateRiskStratified <- readRDS(
    file.path(
        "data/processed",
        paste0(
            paste(
                "calibrateRiskStratified",
                suffix,
                sep = "_"
            ),
            ".rds"
        )
    )
) %>%
    filter(
      analysisType == analysis,
      stratOutcome == stratification
    ) %>%
    rename("outcome" = "estOutcome")

metaCalibrateRiskStratified <- readRDS(
    file.path(
        "data/processed",
        paste0(
            paste(
                "metaCalibrateRiskStratified",
                suffix,
                sep = "_"
            ),
            ".rds"
        )
    )
) %>%
    filter(
      analysisType == analysis,
      stratOutcome == stratification
    ) %>%
    rename("outcome" = "estOutcome")

riskStratified <- calibrateRiskStratified %>%
    mutate(
        hr = exp(logRr),
        lower    = exp(logLb95Rr),
        upper    = exp(logUb95Rr)
    ) %>%
    select(-contains("Rr")) %>%
    mutate(
        type = "single",
        position = case_when(
            database == "ccae" ~ 5,
            database == "mdcr" ~ 4,
            database == "optum_extended_dod" ~ 3,
            database == "optum_ehr" ~ 2
        )
    )

metaRiskStratified <- metaCalibrateRiskStratified %>%
    mutate(
        database = "overall",
        type     = "meta",
        position = 1
    ) %>%
    relocate(database, .after = stratOutcome)

combined <- rbind(riskStratified, metaRiskStratified) %>%
    mutate(
      database = factor(
        x = database,
        levels = c("overall", "optum_ehr", "optum_extended_dod", "mdcr", "ccae"),
        labels = c("Overall", "Optum-EHR", "Optum-DOD", "MDCR", "CCAE")
      ),
      riskStratum = factor(
        x = riskStratum,
        levels = c("Q1", "Q2"),
        labels = c(
          "Lower 75%\nhip fracture risk",
          "Upper 25%\nhip fracture risk"
        )
      )
    ) %>% 
  left_join(map_outcomes, by = c("outcome" = "outcome_id"))

p <- ggplot(
    data = combined,
    aes(
        x    = hr,
        y    = position,
        xmin = lower,
        xmax = upper,
        color = type,
    )
) +
    facet_wrap(
        ~riskStratum + outcome_name
    ) +
    geom_point(
        aes(
            shape = type, 
            fill  = type
        ), 
        size = 2
    ) +
    geom_errorbar(width = 0) +
    geom_vline(xintercept = 1, linetype = 2) +
    geom_text(
        label = "Favors Teriparatide", 
        x     = .479, 
        y     = -.1, 
        color = "black",
        size  = 2
    ) +
    geom_text(
        label = "Favors Bisphosphonates", 
        x     = 1.620, 
        y     = -.1, 
        color = "black",
        size  = 2
    ) +
    scale_y_continuous(
        breaks = c(1, 2, 3, 4, 5),
        labels = c(
            "Summary",
            "Optum-EHR",
            "Optum-DOD",
            "MDCR",
            "CCAE"
        ),
        limits = c(-.2, 5)
    ) +
    scale_x_continuous(
        breaks = c(0, 1, 2),
        labels = c("0", "1", "2"),
        limits = c(0, 2),
        oob    = squish
    ) +
    scale_color_manual(
        breaks = c("meta", "single"),
        values = c("red", "black")
    ) +
    scale_fill_manual(
        breaks = c("meta", "single"),
        values = c("red", "black")
    ) +
    scale_shape_manual(
        breaks = c("meta", "single"),
        values = c(23, 21)
    ) +
    xlab("Calibrated hazard ratio") +
    # theme_classic() +
    ggthemes::theme_clean() +
    theme(
      legend.position    = "none",
      axis.title.y       = element_blank(),
      strip.background   = element_blank(),
      strip.text.x       = element_text(size = 10)
    )

fileName <- paste0(
  paste(
    "plotMetaRiskStratified",
    analysis,
    stratification,
    sep = "_"
  ),
  ".tiff"
)
ggsave(
  file.path("figures", fileName),
  plot = p, 
  height = 5, 
  width = 7, 
  compression = "lzw+p", 
  dpi = 1000
)
