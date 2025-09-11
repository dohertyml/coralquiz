coral_quiz

Flashcard-style coral identification practice and quiz app in R.
Reads your photos from simple folders (per location → per species), launches a Shiny app, and (optionally) records quiz scores.

✨ What it does

Practice mode: big photo, four chunky options, instant feedback.

Quiz mode: fixed number of questions (default 20) with a score screen.

Zero data wrangling: just put photos in folders; names become labels.

Multiple locations/projects: add a new top-level folder, no code changes.

Works locally or with an external photo library (shared drive / NAS).

Supports jpg, jpeg, png, webp. Requires ≥ 4 species per location.

install.packages("remotes")

remotes::install_github("dohertyml/coralQuiz") 

library(coralQuiz)

library(coralQuiz)

# PRACTICE: free play, immediate feedback
practice("caribbean_benthos")

# QUIZ: graded run (20 questions by default)
quiz("caribbean_benthos", n = 20)

# Optional: save a CSV summary of results
quiz("caribbean_benthos", n = 20, save_csv = TRUE, csv_path = getwd(), user = "YOURNAMEHERE")

