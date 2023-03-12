#!/usr/bin/env Rscript

# Description:
#   Generates the plot regarding the meta-analysis of the calibrated hazard
#   ratios for the three outcomes
# Depends:
#   data/processed/calibrateOverallResults.rds 
#   data/processed/metaCalibrateOverall.rds
# Output:
#   figures/plotMeta.pdf

library(tidyverse)
suffix <- "age_50_tr_1_gl"

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
        stratOutcome == 102
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
    filter(stratOutcome == 102) %>%
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
            database == "optum_extended_dod" ~ 4,
            database == "optum_ehr" ~ 3
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
            levels = c("overall", "optum_extended_dod", "optum_ehr"),
            labels = c("Overall", "OPTUM (DoD)", "Panther")
        ),
        outcome = factor(
            x = outcome,
            levels = 101:103,
            labels = c(
                "Hip fracture",
                "Major osteoporotic fracture",
                "Vertebral fracture"
            )
        )
    )

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
        ~riskStratum + outcome
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
        x     = .45, 
        y     = -.1, 
        color = "black",
        size  = 2
    ) +
    geom_text(
        label = "Favors Bisphosphonates", 
        x     = 1.675, 
        y     = -.1, 
        color = "black",
        size  = 2
    ) +
    scale_y_continuous(
        breaks = c(1, 3, 4),
        labels = c(
            "Summary",
            "Optum-EHR",
            "Optum-DOD"
        ),
        limits = c(-.2, 5)
    ) +
    scale_x_continuous(
        breaks = c(0, 1, 2),
        labels = c("0", "1", "2"),
        limits = c(0, 3)
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
    theme_classic() +
    theme(
        legend.position  = "none",
        axis.title.y     = element_blank(),
        strip.background = element_blank()
    )

ggsave("figures/plotMetaRiskStratifiedGuidelines.tiff", plot = p, height = 7, width = 9, compression = "lzw+p", dpi = 1000)
