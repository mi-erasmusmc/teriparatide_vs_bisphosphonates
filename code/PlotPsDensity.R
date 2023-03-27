#!/usr/bin/env Rscript

# ===================================
# File: PlotPsDensity.R
# Description:
# ===================================

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

if (analysis == "1095_custom") {
  stratification <- 5402
} else {
  stratification <- 5403
}

analType <- paste(protocol, estimand, analysis, sep = "_")


databases <- c("ccae", "mdcr", "optum_extended_dod", "optum_ehr")
databaseLabels <- c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")

loadPsDensity <- function(db, stratification) {
  fileLocation <- file.path(
    "data",
    "raw",
    paste0(
      "overall_psDensity_",
      paste0(db, "_"),
      paste0(protocol, "_"),
      paste0(db, "_"),
      paste0(analType, "_"),
      "6889_6888_",
      stratification,
      ".rds"
    )
  )

  readRDS(fileLocation) %>% mutate(database = db)
}

p <- lapply(databases, loadPsDensity, stratification) %>%
  bind_rows() %>%
  mutate(
    database = factor(
      database,
      levels = c("ccae", "mdcr", "optum_extended_dod", "optum_ehr"),
      labels = c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")
    ),
    treatment = factor(
      treatment,
      levels = c(6889, 6888),
      labels = c("Teriparatide", "Oral bisphosphonates")
    )
  ) %>%
  ggplot(aes(x = x, y = y)) +
  facet_grid(cols = vars(database)) +
  geom_density(
    stat = "identity",
    aes(color = treatment, group = treatment, fill = treatment)
  ) +
  scale_x_continuous(breaks = seq(0, 1, .5)) +
  ggplot2::scale_fill_manual(
    values = alpha(c("#fc8d59", "#91bfdb"), .7)
  ) +
  ggplot2::scale_color_manual(
    values = alpha(c("#fc8d59", "#91bfdb"), .8)
  ) +
  xlab("Preference score") +
  ylab("Density") +
  theme(
    legend.position  = "top",
    legend.text      = element_text(size = 7),
    legend.title     = element_blank(),
    # legend.background = element_rect(fill = "#F1F3F8"),
    legend.background = element_blank(),
    axis.title       = element_text(size = 8.5),
    axis.text        = element_text(size = 6),
    axis.ticks.y     = element_blank(),
    strip.background = element_blank(),
    plot.background  = element_rect(fill = "#F1F3F8"),
    panel.background  = element_rect(fill = "#D6E0F0"),
    axis.line        = element_line(color = "black"),
    strip.text       = element_text(size = 8.5),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(4.5, "mm")
  )


fileName <- paste0(
  paste(
    "overallPsDensity",
    analType,
    sep = "_"
  ),
  ".tiff"
)
ggsave(
  file.path("figures", fileName),
  plot = p,
  height = 4,
  width = 8,
  compression = "lzw+p",
  dpi = 1000
)

message(
  crayon::green(
    paste(
      "\u2713 Figure saved at:",
      file.path("figures", fileName),
      "\n"
    )
  )
)
