# Shiny app used by coralquiz::practice() and coralquiz::quiz()
library(shiny)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

# -------- GitHub image settings --------

valid_ext <- c("jpg","jpeg","png","webp")
nice_species <- function(x) gsub("_", " ", x)

github_raw_base  <- "https://raw.githubusercontent.com/dohertyml/coralquiz-images/main"
github_index_url <- file.path(github_raw_base, "index.csv")

.coralquiz_cache <- new.env(parent = emptyenv())

load_github_index <- function() {
  if (!is.null(.coralquiz_cache$index)) {
    return(.coralquiz_cache$index)
  }

  idx <- tryCatch(
    utils::read.csv(github_index_url, stringsAsFactors = FALSE),
    error = function(e) {
      message("[coralquiz] Failed to fetch GitHub index: ", conditionMessage(e))
      data.frame(
        source      = character(),
        species_key = character(),
        filename    = character(),
        stringsAsFactors = FALSE
      )
    }
  )

  # Basic sanity filter on extensions
  keep <- tolower(tools::file_ext(idx$filename)) %in% valid_ext
  idx  <- idx[keep, , drop = FALSE]

  .coralquiz_cache$index <- idx
  idx
}

collect_sources <- function() {
  idx <- load_github_index()
  sort(unique(idx$source))
}

collect_items <- function(source) {
  idx <- load_github_index()
  idx <- idx[idx$source == source, , drop = FALSE]
  if (!nrow(idx)) return(tibble())

  tibble(
    species_key = idx$species_key,
    species_lab = nice_species(idx$species_key),
    filename    = idx$filename
  )
}

img_src <- function(source, species_key, filename) {
  file.path(github_raw_base, source, species_key, filename)
}

# -------- runtime options from practice()/quiz() --------

opt_mode     <- getOption("coralquiz.mode", "practice")         # "practice" or "quiz"
opt_n        <- getOption("coralquiz.n", NA_integer_)           # number of Qs in quiz
opt_save_csv <- isTRUE(getOption("coralquiz.save_csv", FALSE))
opt_csv_path <- getOption("coralquiz.csv_path", getwd())
opt_user     <- getOption("coralquiz.user", NA_character_)
opt_default  <- getOption("coralquiz.default_source", NULL)

# -------- UI --------

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  # Simple HUD (no source picker)
  div(class = "topbar",
      div(class = "right",
          conditionalPanel(
            condition = sprintf("'%s' === 'quiz'", opt_mode),
            span(class = "hud", "Q: ", textOutput("qnum", inline = TRUE)),
            span(class = "hud-sep", " "),
            span(class = "hud", "Score: ", textOutput("score_text", inline = TRUE))
          )
      )
  ),
  # Main content
  div(class = "wrap",
      uiOutput("status_ui"),   # status/errors shown here if needed
      uiOutput("image_ui"),
      uiOutput("options_ui"),
      div(class = "controls",
          actionButton("next_btn", "Next", class = "btn-next")
      )
  )
)

# -------- server --------

server <- function(input, output, session) {

  # Status helpers
  show_status <- function(msg) {
    output$status_ui <- renderUI({
      div(
        style = "margin: 10px auto 0; max-width: 880px; font-weight:600; background:#fff; color:#000; padding:10px 14px; border-radius:10px;",
        msg
      )
    })
  }
  clear_status <- function() output$status_ui <- renderUI(NULL)

  message("[coralquiz] Using GitHub images from: ", github_raw_base)

  # Fixed, non-reactive choice of source (decided once)
  choose_source <- function() {
    srcs <- collect_sources()
    message("[coralquiz] Sources found: ", paste(srcs, collapse = ", "))
    if (!length(srcs)) return(NULL)
    if (!is.null(opt_default) && opt_default %in% srcs) opt_default else srcs[1]
  }

  # App state
  rv <- reactiveValues(
    bank = NULL, order = integer(0), idx = 0L,
    answered = FALSE, chosen_label = NULL, correct_label = NULL,
    options = NULL, current = NULL,
    score = 0L, total = 0L, target = if (is.na(opt_n)) Inf else as.integer(opt_n),
    finished = FALSE,
    current_source = NULL
  )

  # Build bank safely and report issues to the UI
  build_bank <- function() {
    clear_status()
    if (is.null(rv$current_source)) {
      show_status("No sources found in the GitHub image repository.")
      return(FALSE)
    }

    items <- collect_items(rv$current_source)
    if (nrow(items) == 0) {
      show_status(HTML(paste0(
        "No images found for source <b>", rv$current_source, "</b> in the GitHub index."
      )))
      return(FALSE)
    }

    n_species <- nrow(distinct(items, species_key))
    if (n_species < 4) {
      show_status(HTML(paste0(
        "Need at least <b>4 species</b> for source <b>",
        rv$current_source, "</b>. Found ", n_species, "."
      )))
      return(FALSE)
    }

    rv$bank <- items
    rv$order <- sample(seq_len(nrow(items)))
    rv$idx <- 0L
    rv$answered <- FALSE
    rv$chosen_label <- NULL
    rv$correct_label <- NULL
    rv$options <- NULL
    rv$current <- NULL
    rv$score <- 0L
    rv$total <- 0L
    rv$finished <- FALSE
    TRUE
  }

  next_question <- function() {
    req(!is.null(rv$bank), nrow(rv$bank) > 0)

    if (opt_mode == "quiz" && rv$total >= rv$target) {
      rv$finished <- TRUE
      show_results()
      return(invisible(NULL))
    }

    rv$idx <- rv$idx + 1L
    if (rv$idx > length(rv$order)) {
      rv$order <- sample(seq_len(nrow(rv$bank)))
      rv$idx <- 1L
    }

    row <- rv$bank[rv$order[rv$idx], , drop = FALSE]
    rv$current <- row
    rv$answered <- FALSE
    rv$chosen_label <- NULL
    rv$correct_label <- row$species_lab

    all_species <- rv$bank |> distinct(species_key, species_lab)
    others <- all_species |> filter(species_key != row$species_key) |> slice_sample(n = 3)

    rv$options <- bind_rows(
      tibble(species_key = row$species_key, species_lab = row$species_lab, correct = TRUE),
      mutate(others, correct = FALSE)
    ) |> slice_sample(n = 4)
  }

  # ---- INITIALISE ----
  observeEvent(TRUE, {
    rv$current_source <- choose_source()
    message("[coralquiz] Using source: ", if (is.null(rv$current_source)) "<none>" else rv$current_source)

    if (is.null(rv$current_source)) {
      show_status("No sources found in the GitHub image repository.")
      return(invisible(NULL))
    }

    if (build_bank()) {
      next_question()
    }
  }, once = TRUE, ignoreInit = FALSE)

  # ---- outputs ----
  output$image_ui <- renderUI({
    req(rv$current, rv$current_source)

    src <- img_src(
      source      = rv$current_source,
      species_key = rv$current$species_key,
      filename    = rv$current$filename
    )

    tags$div(
      class = "img-wrap",
      tags$img(src = src, class = "flashcard-img", alt = "coral image")
    )
  })

  output$options_ui <- renderUI({
    req(rv$options)
    div(
      class = "options-grid",
      lapply(seq_len(nrow(rv$options)), function(i) {
        lab <- rv$options$species_lab[i]
        cls <- "opt-btn"
        if (rv$answered) {
          if (rv$options$correct[i]) cls <- paste(cls, "is-correct")
          if (!rv$options$correct[i] && identical(lab, rv$chosen_label))
            cls <- paste(cls, "is-wrong")
        }
        actionButton(paste0("opt_", i), lab, class = cls)
      })
    )
  })

  # ---- interactions ----
  observe({
    req(rv$options)
    lapply(seq_len(nrow(rv$options)), function(i) {
      observeEvent(input[[paste0("opt_", i)]], {
        if (isTRUE(rv$answered) || isTRUE(rv$finished)) return(NULL)
        rv$answered <- TRUE
        rv$chosen_label <- rv$options$species_lab[i]
        if (isTRUE(rv$options$correct[i]) && opt_mode == "quiz") {
          rv$score <- rv$score + 1L
        }
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$next_btn, {
    if (isTRUE(rv$finished)) return(invisible(NULL))
    if (opt_mode == "quiz") {
      if (!isTRUE(rv$answered)) return(invisible(NULL))
      rv$total <- rv$total + 1L
    }
    next_question()
  })

  # HUD
  output$qnum <- renderText({
    if (opt_mode == "quiz") sprintf("%d / %d", rv$total + 1L, rv$target)
  })

  output$score_text <- renderText({
    if (opt_mode == "quiz") sprintf("%d correct", rv$score) else ""
  })

  # Results modal + optional CSV
  show_results <- function() {
    if (opt_mode != "quiz") return(invisible(NULL))
    percent <- round(100 * rv$score / rv$target)

    showModal(modalDialog(
      title = "Quiz complete",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("restart", "Restart", class = "btn btn-primary")
      ),
      div(
        h3(sprintf("Score: %d / %d", rv$score, rv$target)),
        p(sprintf("That is %d%%", percent))
      )
    ))

    if (isTRUE(opt_save_csv)) {
      dir.create(opt_csv_path, recursive = TRUE, showWarnings = FALSE)
      out <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        user      = if (is.null(opt_user)) NA_character_ else as.character(opt_user),
        source    = rv$current_source,
        n         = rv$target,
        correct   = rv$score,
        percent   = percent,
        stringsAsFactors = FALSE
      )
      fn <- file.path(
        opt_csv_path,
        sprintf("coralquiz_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
      )
      try(utils::write.csv(out, fn, row.names = FALSE), silent = TRUE)
    }
  }

  observeEvent(input$restart, {
    removeModal()
    rv$score <- 0L
    rv$total <- 0L
    rv$finished <- FALSE
    rv$order <- sample(seq_len(nrow(rv$bank)))
    rv$idx <- 0L
    next_question()
  })
}

shinyApp(ui, server)
