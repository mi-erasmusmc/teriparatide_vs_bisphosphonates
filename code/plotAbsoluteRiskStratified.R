#!/usr/bin/env Rscript

# Author: Alexandros Rekkas
#
# Description:
#   Plots the risk stratified absolute difference for hip fracture with respect
#   to estimated hip fracture risk
# Depends:
#   data/raw/mappedOverallAbsoluteResults.rds
#   data/raw/map_outcomes.rds
#   data/raw/map_exposures.rds
# Output:
#
# Notes:
#   Add mean predicted risk, and rectangle limits to absolute dataframe

library(tidyverse)

args = commandArgs(trailingOnly = TRUE)

protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

analType <- paste(protocol, estimand, analysis, sep = "_")
fileType <- "tiff"


mappedOverallAbsoluteResults <- readRDS(
  "data/raw/mappedOverallAbsoluteResults.rds"
)
map_outcomes <- readRDS(
  "data/raw/map_outcomes.rds"
)
map_exposures <- readRDS(
  "data/raw/map_exposures.rds"
)

prepareDataset <- function(data) {
  data %>%
    left_join(
      map_outcomes,
      by = c("stratOutcome" = "outcome_id")
    ) %>%
    select(-stratOutcome) %>%
    rename("stratOutcome" = "outcome_name") %>%
    left_join(
      map_outcomes,
      by = c("estOutcome" = "outcome_id")
    ) %>%
    select(-estOutcome) %>%
    rename("estOutcome" = "outcome_name") %>%
    left_join(
      map_exposures,
      by = c("treatment" = "exposure_id")
    ) %>%
    select(-treatment) %>%
    rename("treatment" = "exposure_name") %>%
    left_join(
      map_exposures,
      by = c("comparator" = "exposure_id")
    ) %>%
    select(-comparator) %>%
    rename("comparator" = "exposure_name") %>%
    mutate(
      estOutcome = factor(
        estOutcome,
        rev(unique(estOutcome))
      )
    )
}

absolute <- readRDS("data/raw/mappedOverallAbsoluteResults.rds")

if (analysis == "1095_custom_10") {
  riskGroupLabels = c(
    "Hip fracture\nrisk below 2.5%",
    "Hip fracture\nrisk above 2.5%"
  )
  stratification <- 5402
  absolute <- absolute %>%
    filter(!(riskStratum == "Q2" & database == "ccae"))
} else {
  riskGroupLabels = c(
    "Lower \nguideline risk",
    "Upper \nguideline risk"
  )
  stratification <- 5403
}

absolute <- absolute %>%
    filter(
        analysisType == analType,
        stratOutcome == stratification
    ) %>%
  prepareDataset() %>%
    mutate(
        estimate = 100 * estimate,
        lower    = 100 * lower,
        upper    = 100 * upper,
        meanRisk = case_when(
            riskStratum == "Q1" ~.5,
            riskStratum == "Q2" ~1.5
        ),
        estOutcome = factor(
          x = estOutcome,
          levels = c(
            "Hip fracture",
            "Major osteoporotic fracture",
            "Vertebral fracture"
          )
        ),
        riskStratum = factor(
          x = riskStratum,
          levels = c("Q1", "Q2"),
          labels = riskGroupLabels
        ),
        database = case_when(
            database == "ccae" ~ "CCAE",
            database == "mdcr" ~ "MDCR",
            database == "optum_extended_dod" ~ "Optum-DOD",
            database == "optum_ehr" ~ "Optum-EHR"
        )
    )

absolutePlot <- ggplot(
  data = absolute,
  aes(
    x = riskStratum,
    y = estimate,
    ymin = lower,
    ymax = upper,
    color = database,
    group = database
  )
) +
  geom_point(size = 4.5, position = position_dodge(width = .6), key_glyph = "rect") +
  geom_line(size = .8, position = position_dodge(width = .6), linetype = 2, alpha = .55) +
  geom_errorbar(position = position_dodge(width = .6), width = 0, size = 1.2, key_glyph = "rect") +
  scale_y_continuous(
    # trans = "log10",
    # limits = c(.6, 4.04),
    name = "Absolute risk reduction (%)"
  ) +
  # xlab("Risk quarter") +
  facet_grid(~estOutcome, scales = "free") +
  geom_hline(
    aes(yintercept = 0)
  ) +
  scale_color_manual(
    values = c(
      "#264653",
      "#2A9D8F",
      "#E76F51",
      # "#0450B4"
      # "#1184A7"
      # "#6FB1A0",
      "#B4418E"
      # "#EA515F",
      # "#FEA802"
    ),
    breaks = c(
      "CCAE",
      "MDCR",
      "Optum-EHR",
      "Optum-DOD"
    )
  ) +
  guides(col = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(
    #legend.position = c(.2, .962),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 32),
    # legend.background = element_rect(fill = "#a8dadc"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 32),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 35, color = "white"),
    strip.background = element_rect(fill = "#127475"),
    panel.spacing.y = unit(8, "mm"),
    plot.background  = element_rect(fill = "#F5F5F5"),
    panel.background  = element_rect(fill = "#F5F5F5"),
    legend.background  = element_rect(fill = "#F5F5F5")
  )

if (fileType == "tiff") {
  fileName <- paste0(
    paste(
      "plotAbsoluteRiskStratified",
      analType,
      sep = "_"
    ),
    ".tiff"
  )
  ggsave(
    file.path(
      "figures",
      fileName
    ),
    absolutePlot,
    compression = "lzw",
    width       = 650,
    height      = 350,
    units       = "mm",
    dpi         = 300
  )
} else if (fileType == "svg") {
  fileName <- paste0(
    paste(
      "CombinedAbsolute",
      outcome,
      sep = "_"
    ),
    ".svg"
  )
  ggsave(
    file.path(
      "figures",
      fileName
    ),
    absolutePlot,
    width       = 650,
    height      = 350,
    units       = "mm"
  )
}
