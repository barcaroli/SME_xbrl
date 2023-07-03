# Shiny app to
# 1) select an xbrl file in a folder by indicating a search string (use the fiscal code);
# 2) select the elements you are interested to by indicating a search string;
# 3) inspect the obtained html;
# 4) download as a pdf.

# Remember to replace "path" with the actual directory where your XBRL files 
# are stored.
# path <- "C:\\Users\\UTENTE\\Google Drive\\Ballin\\PMI xbrl 3\\TX161114_92665\\BilanciXbrl_8526_92665\\"
path <- "D:\\Google Drive\\Ballin\\PMI xbrl 3\\TX161114_92665\\BilanciXbrl_8526_92665\\"
setwd(path)

# Minimal length of the content of a given element
minimum <- 100

# Import necessary libraries
library(shiny)
library(XBRL)
library(webshot)
# Only first time execute
# webshot::install_phantomjs()

# Define UI
ui <- fluidPage(
  titlePanel("XBRL Viewer and PDF converter"),
  sidebarLayout(
    sidebarPanel(
      textInput("fileSearch", "Search XBRL files"),
      actionButton("searchButton", "Search Files"),
      uiOutput("fileUI"),
      textInput("elementSearch", "Search elements"),
      uiOutput("selectUI"),
      checkboxInput("selectAll", "Select All", value = FALSE)
    ),
    mainPanel(
      textOutput("text"),
      htmlOutput("html_view"),
      downloadButton('downloadPdf', 'Download PDF')
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveValues()
  
  observeEvent(input$searchButton, {
    regex_search <- glob2rx(paste0("*", input$fileSearch, "*.xbrl"))
    file_list <- list.files(path=path, pattern = regex_search)
    data$file_list <- file_list
    updateSelectInput(session, "file1", choices = file_list)
  })
  
  output$fileUI <- renderUI({
    req(data$file_list)
    selectInput("file1", "Choose a XBRL File:", choices = data$file_list)
  })
  
  Fact <- eventReactive(input$file1, {
    req(input$file1)
    full_file_path <- file.path(path, input$file1)
    doc <- xbrlParse(full_file_path)
    Fact <- xbrlProcessFacts(doc)
    Fact <- Fact[nchar(Fact$fact) > 2, ]
    Fact
  })
  
  filtered_data <- reactive({
    req(Fact())
    if (!is.null(input$elementSearch) & input$elementSearch != "") {
      indices <- grep(input$elementSearch, Fact()$elementId, ignore.case = TRUE)
      Fact()[indices, ]
    } else {
      Fact()
    }
  })
  
  output$selectUI <- renderUI({
    req(filtered_data())
    selectInput("selected_element", "Choose an element:", choices = filtered_data()$elementId, multiple = TRUE, selected = if (input$selectAll) filtered_data()$elementId)
  })
  
  observeEvent(input$selectAll, {
    updateSelectInput(session, "selected_element", selected = if (input$selectAll) filtered_data()$elementId)
  })
  
  output$text <- renderText({
    req(input$selected_element)
    paste("Selected Elements: ", toString(input$selected_element))
  })
  
  output$html_view <- renderUI({
    req(input$selected_element)
    subset_data <- filtered_data()[filtered_data()$elementId %in% input$selected_element, "fact", drop = FALSE]
    HTML(as.character(subset_data$fact))
  })
  
  output$downloadPdf <- downloadHandler(
    filename = function() {
      paste(sub(".xbrl","",input$file1), "_", input$elementSearch, '.pdf', sep='')
    },
    content = function(file) {
      req(input$selected_element)
      subset_data <- filtered_data()[filtered_data()$elementId %in% input$selected_element, "fact", drop = FALSE]
      write.table(subset_data,"temp.html")
      html_lines <- readLines("temp.html", encoding = "UTF-8")
      html_lines <- html_lines[-c(1:2)]
      html_lines <- html_lines[-length(html_lines)]
      html_block_init <- c("<!DOCTYPE html>",
                           "<html>",
                           "<head>",
                           "<meta charset=\"UTF-8\">",
                           "</head>",
                           "<body>")
      html_block_end <- c("</body>",
                          "</html>")
      new_html_lines <- c(html_block_init, html_lines, html_block_end)
      writeLines(new_html_lines, "temp.html", useBytes = TRUE)
      webshot("temp.html", file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
