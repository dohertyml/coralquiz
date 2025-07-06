library(shiny)
library(base64enc)

# Get dataset from environment variable
dataset <- Sys.getenv("CORALQUIZ_DATASET", unset = NA)
if (is.na(dataset) || dataset == "") {
  stop("No dataset specified. Use launch_quiz('onetree').")
}

# Construct full path to the dataset folder
img_folder <- file.path(system.file(package = "coralquiz"), dataset)

if (!dir.exists(img_folder)) {
  stop(paste("Folder not found:", img_folder))
}

# Read all genus folders and images
genus_dirs <- list.dirs(img_folder, full.names = TRUE, recursive = FALSE)
image_list <- lapply(genus_dirs, function(dir) {
  genus <- basename(dir)
  files <- list.files(
    dir,
    pattern = "\\.(jpg|jpeg|png)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    return(NULL)
  }
  data.frame(filepath = files, genus = genus, stringsAsFactors = FALSE)
})

# Combine all non-empty image data frames
image_df <- do.call(rbind, image_list)

# If no images were found, stop
if (nrow(image_df) == 0) {
  stop("No images found in the dataset folders. Please check your files.")
}

# Define UI
ui <- fluidPage(
  titlePanel("Coral Genus Flashcard Quiz"),
  fluidRow(
    column(12, align = "center",
           imageOutput("flashcard", height = "400px"),
           br(),
           uiOutput("options"),
           br(),
           textOutput("feedback"),
           br(),
           actionButton("next_img", "Next Image", icon = icon("forward"))
    )
  )
)

# Define server
server <- function(input, output, session) {

  current <- reactiveValues(
    genus = NULL,
    img = NULL,
    choices = NULL,
    answered = FALSE
  )

  load_new_image <- function() {
    chosen_row <- image_df[sample(nrow(image_df), 1), ]
    chosen_genus <- chosen_row$genus
    chosen_img <- chosen_row$filepath
    choices <- sample(unique(image_df$genus), 4)

    if (!chosen_genus %in% choices) {
      choices[1] <- chosen_genus
      choices <- sample(choices)
    }

    current$genus <- chosen_genus
    current$img <- chosen_img
    current$choices <- choices
    current$answered <- FALSE
  }

  load_new_image()

  output$flashcard <- renderImage({
    print(paste("Rendering image:", current$img))
    list(src = current$img, height = "400px")
  }, deleteFile = FALSE)

  output$options <- renderUI({
    lapply(current$choices, function(choice) {
      actionButton(
        inputId = paste0("choice_", choice),
        label = toupper(choice),
        style = "margin:5px; width:150px;"
      )
    })
  })

  observe({
    req(current$choices)
    lapply(current$choices, function(choice) {
      observeEvent(input[[paste0("choice_", choice)]], {
        if (current$answered) return()

        if (choice == current$genus) {
          output$feedback <- renderText("Correct! ✅")
        } else {
          output$feedback <- renderText(
            paste("Incorrect ❌ The correct answer was", toupper(current$genus))
          )
        }
        current$answered <- TRUE
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$next_img, {
    output$feedback <- renderText("")
    load_new_image()
  })
}

# Launch the app
shinyApp(ui, server)
