#!/usr/bin/env Rscript

library(tidyverse)
library(scales)

args <- commandArgs(trailingOnly = TRUE)
analType <- args[1]
strat <- args[2]

mappedOverallAbsoluteResults <- readRDS(
  "data/processed/mappedOverallAbsoluteResults.rds"
)
map_outcomes <- readRDS(
  "data/processed/map_outcomes.rds"
)
map_exposures <- readRDS(
  "data/processed/map_exposures.rds"
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

absolute <- prepareDataset(mappedOverallAbsoluteResults) %>%
  filter(
    stratOutcome == strat,
    analysisType == analType,
    database != "mdcr"
  ) %>%
  mutate(
    estOutcome = stringr::str_replace_all(estOutcome, "_", " "),
    stratOutcome = stringr::str_replace_all(stratOutcome, "_", " "),
    estimate = 100 * estimate,
    lower = 100 * lower,
    upper = 100 * upper,
    estOutcome = stringr::str_replace_all(estOutcome, "_", " "),
    stratOutcome = stringr::str_replace_all(stratOutcome, "_", " ")
  )

absolutePlot <- ggplot(
  data = absolute,
  aes(
    x = riskStratum,
    y = estimate,
    color = estOutcome,
    group = estOutcome
  )
) +
  geom_point(size = 4) +
  geom_line() +
  scale_y_continuous(
    # trans = "log10",
    # limits = c(.6, 4.04),
    name = "Absolute risk reduction (%)"
  ) +
  xlab("Risk stratum") +
  facet_grid(~database, scales = "free") +
  geom_hline(
    aes(yintercept = 0),
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = c(
      "#264653",
      "#2A9D8F",
      "#E76F51"
    ),
    breaks = c(
      "hip fracture",
      "major osteoporotic fracture",
      "vertebral fracture"
    )
  ) +
  guides(col = guide_legend(nrow = 3)) +
  theme_bw() +
  theme(
    legend.position = c(.145, .962),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    # legend.background = element_rect(fill = "#a8dadc"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 15, color = "white"),
    strip.background = element_rect(fill = "#127475"),
  )

fileName <- paste0(
  paste(
    "CombinedAbsolute",
    analType,
    sep = "_"
  ),
  ".tiff"
)

ggsave(
  file.path("figures", fileName),
  absolutePlot, 
  compression = "lzw", 
  width       = 600, 
  height      = 350,
  units       = "mm",
  dpi         = 300
)
