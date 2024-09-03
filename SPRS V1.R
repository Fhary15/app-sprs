library(shiny)
library(seminr)
library(semPlot)
library(DT)
library(lubridate)
library(shinydashboard)


######## DESAIN ANTARMUKA SHINY ########

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "SPRS",
    tags$li(
      class = "dropdown",
      style = "position: absolute; top: 2px;  right: 30px;",
      tags$a("umkaba.ac.id", href = "#", style = "color: white;")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Home", tabName = "HOME", icon = icon("home")),
      menuItem(" Input Data", tabName = "data", icon = icon("table")),
      menuItem(" PLS-SEM", tabName = "analisis", icon = icon("file-text")),
      menuItem(" About", tabName = "faq", icon = icon("user"))
    )
    
  ),
  
  
  dashboardBody( 
    tabItems(
      tabItem(tabName = "HOME",
              mainPanel(
                tags$img(
                  src = "BG1.png",
                  height = "666px",
                  style = "margin-left: -25px; margin-top: -26px;"
                )
              )
      ),
      tabItem(tabName = "data",
              box( title = "Upload Data Disini",
                   status = "primary",
                   solidHeader = TRUE,
                   
                   width = 12,
                   
                   fileInput("file", "Upload Data (.CSV)", accept = ".csv")),
              box( title = "Data",
                   status = "primary",
                   solidHeader = TRUE,
                   
                   width = 12,
                   dataTableOutput("uploaded_data"))),
      
      tabItem(tabName = "analisis",
              box( title = "Masukan Model",
                   status = "primary",
                   solidHeader = TRUE,
                   #tags$style(HTML('.box-header { background-color: #0d6c6b !important; color: white; }')),
                   width = 12,
                   fluidRow(
                     column(6, 
                            textAreaInput(
                              "measurement_model", 
                              "Measurement Model", #title
                              #value = "constructs(\ncomposite(\"E service quality\", multi_items(\"ES\", c(1, 2, 3, 4, 5, 6, 7))),\ncomposite(\"E security seals\", multi_items(\"SS\", c(1, 2, 3))),\ncomposite(\"Kepuasan Pelanggan\", multi_items(\"KP\", c(1, 2, 3, 4))),\ncomposite(\"Loyalitas Pelanggan\", multi_items(\"LP\", c(1, 2, 3))))",
                              rows = 3,
                              placeholder = "Input sintaks untuk pengukuran model..."
                            )
                     ),
                     column(6, 
                            textAreaInput(
                              "structural_model",
                              "Structural Model", #title
                              #value = "relationships(\npaths(from = c(\"E service quality\", \"E security seals\"), to = \"Kepuasan Pelanggan\"),\npaths(from = \"Kepuasan Pelanggan\", to = \"Loyalitas Pelanggan\"))",
                              rows = ,
                              placeholder = "Input sintaks untuk mengukur model struktural..."
                            )
                     )
                   ),
                   actionButton("process_button", "Mulai Analisis", class = "btn btn-primary", style = " color: white;")
              ),
              
              box( title = "Hasil Analisis",
                   status = "primary",
                   solidHeader = TRUE,
                   #tags$style(HTML('.box-header { background-color: #0d6c6b !important; color: white; }')),
                   width = 12,
                   tabsetPanel(
                     
                     tabPanel("Summary", verbatimTextOutput("summary")),
                     tabPanel("Outer Loadings", verbatimTextOutput("loadings")),
                     tabPanel("Cross Loadings", verbatimTextOutput("validity")),
                     tabPanel("Reliability", verbatimTextOutput("reliability")),
                     
                     tabPanel("Path coefficients", verbatimTextOutput("paths")),
                     tabPanel("Bootstrapped Paths", verbatimTextOutput("bootstrapped_paths")),
                     tabPanel("Model Plot", 
                              style = "padding: 10px; ",
                              htmlOutput("model_plot"),
                              uiOutput("model_plot_tab")
                     ),
                     
                   )
              )
      ),
      
      
      
      tabItem(tabName = "faq" ,         
              mainPanel(
                tags$img(
                  src = "BG2.png",
                  height = "673px",
                  style = "margin-left: -35px; margin-top: -30px;"
                )
              ))
      
      
      
      
    ))
)


##### FUNGSI SERVER ####

server <- function(input, output, session) {
  data_table <- NULL
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, sep = ",")
  })
  
  output$uploaded_data <- renderDT({
    datatable(data(), 
              options = list(scrollX = TRUE, scrollY = "400px", fixedHeader=TRUE))
  })
  
  
  # Perform SEM analysis using SEMinR
  
  sem_analysis <- eventReactive(input$process_button, {
    if (!is.null(data()) && !is.null(input$measurement_model) && !is.null(input$structural_model)) {
      measurement_model <- eval(parse(text = input$measurement_model))
      structural_model <- eval(parse(text = input$structural_model))
      
      simple_model <- estimate_pls(
        data = data(),
        measurement_model = measurement_model,
        structural_model = structural_model,
        inner_weights = path_weighting,
        missing = mean_replacement,
        missing_value = "-99"
      )
      
      # Plot model di dalam blok sem_analysis
      model_plot <- plot(simple_model)
      
      summary_simple <- summary(simple_model)
      boot_simple <- bootstrap_model(
        seminr_model = simple_model,
        nboot = 1000,
        cores = NULL,
        seed = 123
      )
      summary_boot <- summary(boot_simple)
      
      return(list(summary_simple = summary_simple, summary_boot = summary_boot, model = simple_model, model_plot = model_plot))
    }
  })
  
  
  
  ######## FUNGSI RENDER OUTPUT ########
  
  #fungsi output nilai summary
  output$summary <- renderPrint({
    sem_analysis()$summary_simple
  })
  
  #fungsi output nilai outer loading
  output$loadings <- renderPrint({
    loadings <- sem_analysis()$summary_simple$loadings
    loadings <- round(loadings, 3)  # Membulatkan ke angka desimal
    loadings
  })
  
  #fungsi output nilai cross loading
  output$validity <- renderPrint({
    validity <- sem_analysis()$summary_simple$validity$cross_loadings
    validity <- round(validity, 3)  # Membulatkan ke angka desimal
    validity
  })
  
  #fungsi output nilai realibilitas
  output$reliability <- renderPrint({
    sem_analysis()$summary_simple$reliability
  })
  
  
  #fungsi output nilai koefisien jalur
  output$paths <- renderPrint({
    sem_analysis()$summary_simple$paths
  })
  
  output$bootstrapped_paths <- renderPrint({
    bootstrapped_paths <- sem_analysis()$summary_boot$bootstrapped_paths
    bootstrapped_paths <- bootstrapped_paths[, c("Original Est.", "T Stat.")]
    bootstrapped_paths <- round(bootstrapped_paths, 3)  # Membulatkan ke angka desimal
    bootstrapped_paths
  })
  
  
  #fungsi output plot hanya tersedia di program R
  output$model_plot_tab <- renderUI({
    if (!is.null(sem_analysis()$model_plot)) {
      sem_analysis()$model_plot
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
