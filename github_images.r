# Base URL for the image repo
github_image_base <- function() {
  "https://raw.githubusercontent.com/dohertyml/coralquiz-images/main"
}

# Build full URLs from a location/species/filename table
build_github_urls <- function(df) {
  # df must have: location, species, filename
  df$url <- sprintf(
    "%s/%s/%s/%s",
    github_image_base(),
    df$location,
    df$species,
    df$filename
  )
  df
}

# Load the image index (index.csv) from the coralquiz-images repo
github_image_index <- local({
  .cache <- NULL

  function(refresh = FALSE) {
    if (is.null(.cache) || refresh) {
      url <- paste0(github_image_base(), "/index.csv")
      .cache <<- readr::read_csv(url, show_col_types = FALSE)
    }
    .cache
  }
})

