#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)
resultFile <- args[1]

message(
  paste("Fetching", resultFile, "from github")
)

githubAccount <- "https://github.com/mi-erasmusmc"
repo <- "teriparatide_vs_bisphosphonates"
repoLocation <- "raw/results/data/raw"

repo <- file.path(githubAccount, repo, repoLocation)

filePath <- file.path(repo, resultFile)

saveDirectory = file.path("data/raw")

if (!dir.exists(saveDirectory)) {
  dir.create(saveDirectory, recursive = TRUE)
}

saveRDS(
  readRDS(url(filePath)),
  file = file.path(saveDirectory, resultFile)
)

message(
  crayon::green(
    paste(
      "\u2713 File saved at:",
      file.path(file.path(saveDirectory, resultFile)),
      "\n"
    )
  )
)
