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
