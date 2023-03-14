#!/usr/bin/env Rscript

library(tidyverse)
library(scales)

args <- commandArgs(trailingOnly = TRUE)
protocol <- args[1]
estimand <- args[2]
analysis <- args[3]
fileType <- args[4]

if (analysis == "1095_custom_10") {
  stratification <- 5402
  xAxisLabel <- "hip fracture"
  riskLevels <- c("Below 2.5%", "Above 2.5%")
} else {
  stratification <- 5403
  xAxisLabel <- "major osteoporotic fracture"
  riskLevels <- c("Guideline low risk", "Guideline high risk")
}

analType <- paste(protocol, estimand, analysis, sep = "_")

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

absolute <- mappedOverallAbsoluteResults %>%
  filter(
    stratOutcome == stratification,
    analysisType == analType,
    !(riskStratum == "Q2" & database == "ccae")
  ) %>%
 prepareDataset() %>%
  mutate(
    estOutcome = stringr::str_replace_all(estOutcome, "_", " "),
    stratOutcome = stringr::str_replace_all(stratOutcome, "_", " "),
    estimate = 100 * estimate,
    lower = 100 * lower,
    upper = 100 * upper,
    estOutcome = stringr::str_replace_all(estOutcome, "_", " "),
    stratOutcome = stringr::str_replace_all(stratOutcome, "_", " "),
    database = factor(
      database,
      levels = c("ccae", "mdcr", "optum_extended_dod", "optum_ehr"),
      labels = c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")
    ),
    riskStratum = factor(
      riskStratum,
      levels = c("Q1", "Q2"),
      labels = riskLevels
    )
  )

p <- ggplot(
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
  geom_point(position = position_dodge(width = .6), key_glyph = "rect") +
  geom_line(position = position_dodge(width = .6), linetype = 2, alpha = .55) +
  geom_errorbar(position = position_dodge(width = .6), width = 0, key_glyph = "rect") +
  scale_y_continuous(
    # trans = "log10",
    # limits = c(.6, 4.04),
    name = "Absolute risk reduction (%)"
  ) +
  xlab(paste("Predicted", xAxisLabel, "risk")) +
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
      "Optum-DOD",
      "Optum-EHR"
    )
  ) +
  guides(col = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(
    legend.position  = "top",
    legend.text      = element_text(size = 7),
    legend.title     = element_blank(),
    # legend.background = element_rect(fill = "#f1f3f8"),
    legend.background = element_blank(),
    axis.title       = element_text(size = 8.5),
    axis.text        = element_text(size = 6),
    axis.ticks.y     = element_blank(),
    strip.background = element_blank(),
    plot.background  = element_rect(fill = "#f1f3f8"),
    panel.background  = element_rect(fill = "#d6e0f0"),
    axis.line        = element_line(color = "black"),
    strip.text       = element_text(size = 8.5),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(2.5, "mm")
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
    file.path("figures", fileName),
    plot = p,
    height = 4,
    width = 7,
    compression = "lzw+p",
    dpi = 1000
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
