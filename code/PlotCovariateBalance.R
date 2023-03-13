#!/usr/bin/env Rscript

library(tidyverse)

databases <- c("ccae", "mdcr", "optum_extended_dod", "optum_ehr")
databaseLabels <- c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")

args = commandArgs(trailingOnly = TRUE)
protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

if (analysis == "1095_custom_10") {
  stratification <- 5402
} else {
  stratification <- 5403
}

analType <- paste(protocol, estimand, analysis, sep = "_")

loadBalance <- function(db, stratification) {
  fileLocation <- file.path(
    "data",
    "raw",
    paste0(
      "overall_balance_",
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

p <- lapply(databases, loadBalance, stratification) %>%
  bind_rows() %>%
  mutate(
    beforeStdDiff = abs(beforeMatchingStdDiff),
    afterStdDiff = abs(afterMatchingStdDiff),
    database = factor(
      database,
      levels = c("ccae", "mdcr", "optum_extended_dod", "optum_ehr"),
      labels = c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")
    )
  ) %>%
  ggplot(aes(x = beforeStdDiff, y = afterStdDiff)) +
  facet_grid(cols = vars(database)) +
  geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16) +
  scale_y_continuous(
    limits = c(0, .2),
    breaks = seq(0, .2, by = 0.05),
    expand = c(0, 0)
  ) +
  scale_x_continuous(limits = c(0, .5), expand = c(0, 0)) +
  xlab("Before matching") +
  ylab("After matching") +
  theme(
    legend.position  = "none",
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
    "overallCovariateBalance",
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
