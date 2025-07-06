library(shiny)
library(base64enc)

# Let user pick their image folder
img_folder <- shiny::choose.dir(default = "~/Desktop/coralquiz/onetree", caption = "Select your coral image folder")

if(is.na(img_folder) || img_folder == ""){
  stop("No folder selected. Exiting app.")
}

# Read all genus folders and images
genus_dirs <- list.dirs(img_folder, full.names = TRUE, recursive = FALSE)
image_df <- do.call(rbind, lapply(genus_dirs, function(dir){
  genus <- basename(dir)
  files <- list.files(dir, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)
  data.frame(filepath = files, genus = genus, stringsAsFactors = FALSE)
}))

ui <- fluidPage(
  titlePanel("Coral Genus Flashcard Quiz"),
  fluidRow(
    column(12, align="center",
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

server <- function(input, output, session) {

  current <- reactiveValues(genus = NULL, img = NULL, choices = NULL, answered = FALSE)

  load_new_image <- function(){
    chosen_row <- image_df[sample(nrow(image_df), 1), ]
    current$genus <- chosen_row$genus
    current$img <- chosen_row$filepath
    current$choices <- sample(unique(image_df$genus), 4)

    if(!current$genus %in% current$choices){
      current$choices[1] <- current$genus
      current$choices <- sample(current$choices)
    }

    current$answered <- FALSE
  }

  load_new_image()

  output$flashcard <- renderImage({
    list(src = current$img, height = "400px")
  }, deleteFile = FALSE)

  output$options <- renderUI({
    lapply(current$choices, function(choice){
      actionButton(inputId = paste0("choice_", choice),
                   label = choice,
                   style = "margin:5px; width:150px;")
    })
  })

  observe({
    lapply(current$choices, function(choice){
      observeEvent(input[[paste0("choice_", choice)]], {
        if(!current$answered){
          if(choice == current$genus){
            output$feedback <- renderText("Correct! ✅")
          } else {
            output$feedback <- renderText(paste("Incorrect ❌ The correct answer was", current$genus))
          }
          current$answered <- TRUE
        }
      })
    })
  })

  observeEvent(input$next_img, {
    output$feedback <- renderText("")
    load_new_image()
  })
}

shinyApp(ui, server)
