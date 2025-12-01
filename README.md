coralquiz

Flashcard style coral identification practice and quiz app in R.
Launches a Shiny app that draws images from simple folder structures or from the bundled online image sets, and can optionally record quiz scores to CSV.

✨ What it does

Practice mode <- practice()
Big photo, four chunky options, instant feedback, unlimited questions.

Quiz mode <- quiz()
Fixed number of questions (default 20) with a final score screen.

Zero data wrangling for your own photos
Put photos in folders organised as location / species / image_files and species names become the labels.

Multiple locations or projects
Each top level folder is a separate source. No code changes needed.

Built in online image sets
Packs ready to use training sets that live in a separate image repository, so the R package stays small.

File formats
Supports jpg, jpeg, png, and webp.
Each source must contain at least four species.

Note: the default image sets are hosted online. You need an internet connection to use them.

Installation
install.packages("remotes")

remotes::install_github("dohertyml/coralquiz")

library(coralquiz)

Quick start

The package ships with ready to use online image sources.

caribbean_benthos
CoralNet style photo quadrats of Caribbean benthos, including corals, macroalgae, sponges and other substrata.

caribbean_corals
Closer coral views that focus on coral taxa rather than the whole benthic community.

You can pass these directly to practice() or quiz().

Practice mode

Free play with immediate feedback.

library(coralquiz)

# Practice identifying Caribbean benthos
practice("caribbean_benthos")

# Practice identifying Caribbean corals
practice("caribbean_corals")


This launches the Shiny app in practice mode with unlimited questions.
Select an answer, see if you are correct, move on to the next image.

Quiz mode

Graded run with a fixed number of questions.

library(coralquiz)

# Default 20 questions
quiz("caribbean_benthos")

# Custom number of questions
quiz("caribbean_corals", n = 30)


At the end of the quiz you see how many you got right and a percentage score.

Saving quiz results to CSV

You can optionally save a summary of each quiz attempt.

library(coralquiz)

quiz(
  "caribbean_benthos",
  n        = 20,
  save_csv = TRUE,
  csv_path = getwd(),
  user     = "YOURNAMEHERE"
)


This writes a CSV file containing at least

user name

date and time

source

number of questions

number correct and percentage

Use this if you want to track progress through time or collate results for a class.

Using your own photos (advanced)

You can point the app at any folder of images that follows a simple structure

photos_root/
  your_location_1/
    species_a/
      img001.jpg
      img002.jpg
    species_b/
      img003.jpg
  your_location_2/
    species_c/
      ...


Where

photos_root is the path to the root folder on your machine

each subfolder under photos_root is a source or location

each subfolder under a source is a single species

supported image types are jpg, jpeg, png, webp

each source must contain at least four species

Launch practice mode on your own photos

practice(
  source      = "your_location_1",
  photos_root = "/path/to/photos_root"
)


Launch quiz mode on your own photos

quiz(
  source      = "your_location_1",
  n           = 20,
  photos_root = "/path/to/photos_root",
  save_csv    = FALSE
)

Requirements and notes

R and Shiny installed

Internet connection required for the built in online image sets

For offline use you must supply a local photos_root with your own images

Each source needs at least four species to provide enough answer options

That is it. Install the package, choose a source, and start practising coral identification.
