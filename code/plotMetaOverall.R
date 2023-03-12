#!/usr/bin/env Rscript

# ==============================================================================
# Description:
#   Generates the plot regarding the meta-analysis of the calibrated hazard
#   ratios for the three outcomes
# Depends:
#   data/processed/calibrateOverallResults.rds 
#   data/processed/metaCalibrateOverall.rds
# Output:
#   figures/plotMeta.pdf
# ==============================================================================

library(tidyverse)
args = commandArgs(trailingOnly = TRUE)
suffix <- args[1]

print(suffix)

calibrateOverallResults <- readRDS(
  file.path(
    "data/processed",
    paste0(
      paste(
        "calibrateOverallResults",
        suffix,
        sep = "_"
      ),
      ".rds"
    )
  )
) 
  # filter(outcome == mainOutcome)

metaCalibrateOverall <- readRDS("data/processed/metaCalibrateOverall_att.rds")
  # filter(outcome == mainOutcome)

overall <- calibrateOverallResults %>%
  select(-contains("Rr")) %>%
  mutate(
    type = "single",
    position = case_when(
      database == "ccae" ~ 5,
      database == "optum_extended_dod" ~ 4,
      database == "optum_ehr" ~ 3,
      database == "mdcr" ~ 2
    )
  )

metaOverall <- metaCalibrateOverall %>%
  mutate(
    database = "overall",
    type     = "meta",
    position = 1
  )

combined <- rbind(overall, metaOverall) %>%
  mutate(
    database = factor(
      x = database,
      levels = c("overall", "optum_extended_dod", "optum_ehr", "mdcr", "ccae"),
      labels = c("Overall", "Optum-DOD", "Optum-EHR", "MDCR", "CCAE")
    ),
    outcome = factor(
      x = outcome,
      levels = 5402:5404,
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
    ~outcome
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
    limits = c(0, 2.2)
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
    legend.position  = "none",
    axis.title.y     = element_blank(),
    strip.background = element_blank(),
    strip.text.x     = element_text(size = 10)
  )

# ggsave("figures/plotMeta.pdf", plot = p, height = 3, width = 7)
ggsave("figures/plotMeta.tiff", plot = p, height = 3, width = 7, compression = "lzw+p", dpi = 600)
# ggsave("figures/plotMeta.png", plot = p, height = 3, width = 7)

