#!/usr/bin/env Rscript

library(tidyverse)

databases <- c("ccae", "mdcr", "optum_extended_dod", "optum_ehr")
databaseLabels <- c("CCAE", "MDCR", "Optum-DOD", "Optum-EHR")
pp <- list()

for (i in seq_along(databases)) {
  pattern <- paste0(
    "overall_balance_",
    databases[i],
    "_itt_",
    databases[i],
    "_itt_att_1095_gl_",
    "6889_6888_",
    "5402",
    ".rds"
  )
  pp[[i]] <- list.files(
    path = "data/raw",
    pattern = pattern,
    full.names = TRUE
  ) %>%
    map(readRDS) %>%
    bind_rows() %>%
    tibble() %>%	
    CohortMethod::plotCovariateBalanceScatterPlot(
      beforeLabel = " ",
      afterLabel = " "
      ) +
    ggtitle(databaseLabels[i]) +
    theme_bw() +
    theme(
      plot.margin  = margin(2, .25, 2, .25, "cm"),
      plot.title   = element_text(size = 34),
      axis.title.x = element_blank(),
      axis.text.x  = element_text(size = 22),
      axis.text.y  = element_text(size = 22),
      axis.title   = element_text(size = 30)
    )
  
  if (i != 1) {
    pp[[i]] <- pp[[i]] +
      theme(
        axis.text.y = element_blank()
      )
  }
}

plot <- cowplot::plot_grid(
  pp[[1]],
  pp[[2]],
  pp[[3]],
  pp[[4]],
  nrow = 1
) +
  cowplot::draw_label("Before matching", x = .5, y = 0, vjust = -.5, size = 30) +
  cowplot::draw_label("After matching", x = 0, y = .5, vjust = 1.2, angle = 90, size = 30)
ggsave(
  "figures/OverallCovariateBalance.tiff",
  plot, 
  compression = "lzw", 
  width       = 700, 
  height      = 350,
  units       = "mm",
  dpi         = 600
)
