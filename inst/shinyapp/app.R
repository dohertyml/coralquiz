library(shiny)
library(base64enc)

# Get dataset argument
args <- commandArgs(trailingOnly = TRUE)
dataset <- NULL
if (length(args) > 0) {
  dataset <- args[[1]]
} else {
  stop("No dataset specified. Use launch_quiz('onetree').")
}

# Construct full path to the dataset folder
img_folder <- file.path(system.file(package = "coralquiz"), dataset)

if (!dir.exists(img_folder)) {
  stop(paste("Folder not found:", img_folder))
}
