#!/usr/bin/env Rscript

library(tidyverse)
library(scales)


args <- commandArgs(trailingOnly = TRUE)
analysisType <- as.character(args[1])
target <- as.character(args[2])
riskStratificationOutcome <- as.numeric(args[3])
estimationOutcome <- as.numeric(args[4])
fileType <- as.character(args[5])

databases <- c("ccae", "mdcr", "optum_ehr", "optum_extended_dod")
databaseLabels <- c("CCAE", "MDCR", "Optum-EHR", "Optum-DOD")
map_exposures <- readRDS("data/raw/map_exposures.rds")
pp <- list()

for (i in seq_along(databases)) {
  analysis <- paste(
    databases[i],
    "itt",
    databases[i],
    "itt",
    target,
    sep = "_"
  )
  pattern <- paste0(
    "psDensity_",
    analysis,
    "_",
    1095,
    "_",
    analysisType,
    "_",
    "6889_6888_",
    riskStratificationOutcome,
    "_",
    estimationOutcome,
    ".rds"
  )
  pp[[i]] <- list.files(
    path = "data/raw",
    pattern = pattern,
    full.names = TRUE
  ) %>%
    readRDS() %>%
    dplyr::mutate(
      treatment = ifelse(
        treatment == 1,
        treatmentId,
        comparatorId
      )
    ) %>%
    dplyr::left_join(
    map_exposures,
    by = c("treatment" = "exposure_id")
  ) %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = x,
        y = y
      )
    ) +
    ggplot2::geom_density(
      stat = "identity",
      ggplot2::aes(
        color = exposure_name,
        group = exposure_name,
        fill = exposure_name
      )
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, .5)
    ) +
    ggplot2::facet_grid(~riskStratum) +
    ggplot2::ylab(
      label = databaseLabels[i]
    ) +
    ggplot2::xlab(
      label = "Preference score"
    ) +
    ggplot2::scale_fill_manual(
      values = alpha(c("#fc8d59", "#91bfdb"), .6)
    ) +
    ggplot2::scale_color_manual(
      values = alpha(c("#fc8d59", "#91bfdb"), .9)
    ) +
    theme_classic() +
    ggplot2::theme(
      legend.title    = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.x    = element_blank(),
      axis.line.y     = element_blank(),
      axis.ticks.y    = element_blank(),
      axis.text.y     = element_blank(),
      axis.text.x     = element_text(size = 22),
      axis.title      = element_text(size = 20),
      strip.text      = element_text(size = 25)
    )  
  if (i != 1) {
    pp[[i]] <- pp[[i]] +
      theme(
        strip.text       = element_blank(),
        strip.background = element_blank()
      )
  }
}


plot <- gridExtra::grid.arrange(pp[[1]], pp[[2]], pp[[3]], pp[[4]], nrow = 4)
# ggsave(
#   "figures/PsDensityRiskStratified.tiff",
#   plot, 
#   compression = "lzw", 
#   width       = 600, 
#   height      = 350,
#   units       = "mm",
#   dpi         = 300
# )

fileName <- paste(
  "PsDensityRiskStratified",
  target,
  analysisType,
  sep = "_"
)

if (fileType == "tiff") {
  ggsave(
    file.path(
      "figures",
      paste0(fileName, ".tiff")
    ),
    plot, 
    compression = "lzw", 
    width       = 650, 
    height      = 350,
    units       = "mm",
    dpi         = 300
  )
} else if (fileType == "svg") {
  ggsave(
    file.path(
      "figures",
      paste0(fileName, ".svg")
    ),
    plot, 
    width       = 650, 
    height      = 350,
    units       = "mm"
  )
}


