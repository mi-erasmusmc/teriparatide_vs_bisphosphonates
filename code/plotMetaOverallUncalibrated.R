#!/usr/bin/env Rscript

# ==============================================================================
# Description:
#   Generates the plot regarding the meta-analysis of the calibrated hazard
#   ratios for the three outcomes
# Depends:
#   data/raw/mappedOverallResults.rds
# Output:
#   figures/plotMetaOverallUncalibrated_xx_xx_xx.tiff
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

args = commandArgs(trailingOnly = TRUE)
protocol <- args[1]
estimand <- args[2]
analysis <- args[3]

analType <- paste(protocol, estimand, analysis, sep = "_")

message(rep("=", 80))
message(crayon::bold("SETTINGS"))
message(paste0("args_protocol:  ", protocol))
message(paste0("args_estimand:  ", estimand))
message(paste0("args_analysis:  ", analysis))
message(rep("=", 80))

relativeResults <- readRDS("data/raw/mappedOverallResults.rds") %>%
  mutate(
    logRr = log(estimate),
    database = stringr::str_to_lower(database)
  ) %>%
  filter(analysisType == analType) %>%
  rename(c("outcome" = "outcomeId"))

suffix <- paste(protocol, estimand, analysis, sep = "_")

metaAnalysis <- function(data) {
  metaRes <- meta::metagen(
    TE = data$logRr,
    seTE = data$seLogRr,
    sm = "HR",
    method.tau = "PM",
    studlab = data$database
  )

  res <- tibble(
    hr = exp(metaRes$TE.random),
    lower = exp(metaRes$lower.random),
    upper = exp(metaRes$upper.random)
  )

  return(res)

}

metaRelativeResults <- relativeResults %>%
  mutate(
    logRr = log(estimate)
  ) %>%
  filter(analysisType == analType) %>%
  group_by(outcome) %>%
  nest() %>%
  mutate(
    ll = map(
      .x = data,
      .f = ~metaAnalysis(.x)
    )
  ) %>%
  unnest(cols = ll) %>%
  select(-data) %>%
  mutate(
    database = "overall",
    position = 1
  )

combined <- bind_rows(
  relativeResults %>%
    select(estimate, lower, upper, outcome, database) %>%
    rename(c("hr" = "estimate")) %>%
  mutate(
    type = "single",
    position = case_when(
      database == "ccae" ~ 5,
      database == "mdcr" ~ 4,
      database == "optum_extended_dod" ~ 3,
      database == "optum_ehr" ~ 2,
    )
  ),
  metaRelativeResults %>% mutate(type = "meta")
) %>%
  mutate(
    database = factor(
      x = database,
      levels = c("overall", "optum_ehr", "optum_extended_dod", "mdcr", "ccae"),
      labels = c("Overall", "Optum-EHR", "Optum-DOD", "MDCR", "CCAE")
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


stripData <- data.frame(database = levels(combined$database)) %>%
  mutate(
    xmin = 0,
    xmax = 2,
    position = 1:5,
    y_min = position - .5,
    y_max = position + .5,
    fill = rep(c("a", "b"), length.out = nrow(.))
  ) %>%
  pivot_longer(
    cols = c(xmin, xmax),
    values_to = "x",
    names_to = "xmin_xmax"
  ) %>%
  select(-xmin_xmax)




annotationTeriparatide <- data.frame(
  hr = .279,
  position = .8,
  lower = 1,
  upper = 1,
  type = "single",
  lab = "Favors Teriparatide",
  riskStratum = factor(
    "Q2",
    levels = c("Q1", "Q2"),
    labels = c(
      "Hip fracture risk below 2.5%",
      "Hip fracture risk above 2.5%"
    )
  )
)

annotationBisphposphonates <- data.frame(
  hr = 1.7,
  position = .8,
  lower = 1,
  upper = 1,
  type = "single",
  lab = "Favors Bisphosphonates",
  riskStratum = factor(
    "Q2",
    levels = c("Q1", "Q2"),
    labels = c(
      "Hip fracture risk below 2.5%",
      "Hip fracture risk above 2.5%"
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
  geom_ribbon(
    data = stripData,
    aes(x = x, ymin = y_min, ymax = y_max, group = position, fill = fill),
    inherit.aes = FALSE,
  ) +
  geom_point(
    aes(
      shape = type,
      fill  = type
    ),
    size = 1.5
  ) +
  geom_errorbar(width = 0) +
  geom_vline(xintercept = 1, linetype = 2, alpha = .4) +
  geom_text(
    data = annotationTeriparatide,
    label = "Favors\nTeriparatide",
    size = 1.7
  ) +
  geom_text(
    data = annotationBisphposphonates,
    label = "Favors\nBisphosphonates",
    size = 1.8
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 2),
    labels = c("0", "1", "2"),
    limits = c(0, 2),
    oob    = squish,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = combined$position,
    labels = combined$database
  ) +
  scale_color_manual(
    breaks = c("meta", "single"),
    values = c("red", "black")
  ) +
  scale_fill_manual(
    breaks = c("meta", "single", "a", "b"),
    values = c("red", "black", "#D6E0F0", "#8D93AB")
  ) +
  scale_shape_manual(
    breaks = c("meta", "single"),
    values = c(23, 21)
  ) +
  xlab("Calibrated hazard ratio") +
  # theme_void() +
  # ggthemes::theme_clean() +
  theme(
    legend.position  = "none",
    axis.title.y     = element_blank(),
    axis.title.x     = element_text(size = 8.5),
    axis.text        = element_text(size = 7),
    axis.ticks.y     = element_blank(),
    strip.background = element_blank(),
    plot.background  = element_rect(fill = "#F1F3F8"),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    strip.text       = element_text(size = 8.5)
  )


fileName <- paste0(
  paste(
    "plotMetaOverallUncalibrated",
    protocol,
    estimand,
    analysis,
    sep = "_"
  ),
  ".tiff"
)

ggsave(
  file.path("figures", fileName),
  plot = p,
  height = 3,
  width = 7,
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
