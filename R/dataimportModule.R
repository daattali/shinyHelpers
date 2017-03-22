#' @export
doit <- function() {
  fileExt = c("csv", "txt", "xlsx")
  
  ui <- fluidPage(
    dataimportUI("vv", fileExt = fileExt),
    br(),
    DT::dataTableOutput("table")
  )
  
  server <- function(input, output, session) {
    mydata <- dataimportServer("vv", fileExt = fileExt)
    
    output$table <- DT::renderDataTable({
      mydata()
    })
  }
  
  runApp(shinyApp(ui = ui, server = server))
}





#' @export
#' @import shiny
dataimportUI <- function(id, label = "Choose a file",
                   fileExt = c("csv", "txt", "xlsx", "rds", "ods")) {

  SUPPORTED_FILES <- c("csv", "txt", "xlsx", "rds", "ods")
  if (!all(fileExt %in% SUPPORTED_FILES) ) {
    stop("Unsupported file formats. Supported formats: ",
         paste(SUPPORTED_FILES, collapse = ", "),
         call. = FALSE)
  }
  
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    fileInput(ns("upload_file"), label, multiple = FALSE,
              accept = paste0(".", fileExt)),
    shinyjs::hidden(
      selectInput(ns("upload_type"), "File type", selected = "",
                  c("", fileExt))
    ),

    conditionalPanel(
      paste0("input['", ns("upload_type"),"']  == 'csv'"),
      div(
          div(h1("Import CSV Options")),
          checkboxInput(ns("upload_options_csv_header"),
                        "First row is column names", TRUE),
          selectInput(ns("upload_options_csv_sep"),
                      "Delimeter",
                      c("Comma" = ",", "Tab" = "\t", "Whitespace" = " ")),
          selectInput(ns("upload_options_csv_quote"),
                      "Quotes around strings",
                      c("Double (\")" = "\"", "Single (')" = "'",
                        "None"))
      )
    ),
    
    conditionalPanel(
      paste0("input['", ns("upload_type"),"'] == 'txt'"),
      div(
          div(h1("Import Text Options")),
          checkboxInput(ns("upload_options_txt_header"),
                        "First row is column names", TRUE),
          selectInput(ns("upload_options_txt_sep"),
                      "Delimeter",
                      c("Comma" = ",", "Tab" = "\t", "Whitespace" = " ")),
          selectInput(ns("upload_options_txt_quote"),
                      "Quotes around strings",
                      c("Double (\")" = "\"", "Single (')" = "'",
                        "None"))
      )
    ),
    
    conditionalPanel(
      paste0("input['", ns("upload_type"),"'] == 'xlsx'"),
      div(
          div(h1("Import Excel Options")),
          checkboxInput(ns("upload_options_xlsx_col_names"),
                        "First row is column names", TRUE),
          numericInput(ns("upload_options_xlsx_sheet"), "Sheet to read",
                       min = 1, value = 1)
      )
    ),
    shinyjs::hidden(
      actionButton(ns("upload_import_btn"), "Import Data",
                   class = "btn-primary")
    ),
    shinyjs::hidden(
      div(id = ns("upload_err"),
          style = "color: red;",
          icon("exclamation-circle"),
          tags$b("Error:"),
          textOutput(ns("upload_err_msg"), inline = TRUE)
      )
    )
  )
}

#' @export
#' @import shiny
dataimportServer <- function(id,
                             fileExt = c("csv", "txt", "xlsx", "rds", "ods")) {
  SUPPORTED_FILES <- c("csv", "txt", "xlsx", "rds", "ods")
  if (!all(fileExt %in% SUPPORTED_FILES) ) {
    stop("Unsupported file formats. Supported formats: ",
         paste(SUPPORTED_FILES, collapse = ", "),
         call. = FALSE)
  }
  return(callModule(dataimportServerHelper, id, fileExt = fileExt))
}

dataimportServerHelper <- function(input, output, session, id, fileExt) {

  # map between a file extension to the function that can be used to import it
  FILETYPE_READ_FXN <- c(
    "csv" = "utils::read.csv",
    "rds" = "base::readRDS",
    "txt" = "utils::read.delim",
    "xlsx" = "readxl::read_excel"
  )

  values <- reactiveValues(
    data = NULL,
    error = NULL
  )
  
  observe({
    shinyjs::toggle("upload_err", condition = !is.null(values$error))
    shinyjs::toggle("upload_import_btn",
                    condition = is.null(values$error) && input$upload_type != "")
  })
  
  output$upload_err_msg <- renderText({
    values$error
  })

  # get the local path of the uploaded file (after fixing its filename)
  upload_file_path <- eventReactive(input$upload_file, {
    values$error <- NULL
    file <- input$upload_file
    new_file <- file.path(dirname(file$datapath), file$name)
    file.rename(from = file$datapath, to = new_file)
    new_file
  })
  
  # after uploading a file 
  observeEvent(upload_file_path(), {
    file <- upload_file_path()
    file_ext <- tools::file_ext(file)
    
    if (!file_ext %in% fileExt) {
      values$error <- "Unsupported file format"
      updateSelectInput(session, "upload_type", selected = "")
      return()
    }
    
    values$error <- NULL

    if (file_ext %in% SUPPORTED_FILES) {
      shinyjs::hide("upload_type")
      updateSelectInput(session, "upload_type", selected = file_ext)  
    } else {
      shinyjs::show("upload_type")
      updateSelectInput(session, "upload_type", selected = "")
    }
  })

  # do.call() can't handle a namespaced function, so split it up  
  get_read_fxn <- function(fxn) {
    fxn_split <- strsplit(fxn, "::")[[1]]
    if (length(fxn_split) == 1) {
      return(fxn_split[[1]])
    } else {
      return(get(fxn_split[[2]], asNamespace(fxn_split[[1]])))
    }
  }
  
  observeEvent(input$upload_import_btn, {
    # Figure out what function to call
    file <- upload_file_path()
    file_ext <- input$upload_type
    read_fxn <- FILETYPE_READ_FXN[file_ext]
    fxn <- get_read_fxn(read_fxn)

    # Gather all the parameters of the read function
    params_regex <- paste0("^upload_options_", file_ext, "_(.*)$")
    params_inputs <- grep(params_regex, names(input), value = TRUE)
    read_params <- list(file)
    lapply(params_inputs, function(x) {
      param <- gsub(params_regex, "\\1", x)
      value <- input[[x]]

      if (param == "quote") {
        if (value == "None") {
          value <- ""
        }
      }
      read_params[[param]] <<- value
    })
    
    values$data <- do.call(fxn, read_params)
  })
  
  return(reactive(values$data))
}