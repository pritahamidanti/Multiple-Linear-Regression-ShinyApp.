library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(DT)
set.seed(123)

# Data provided by the user
user_data <- data.frame(
  X1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  X2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  X3 = c(5.0, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  X4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  X5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Multiple Linear Regression Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Download Report", tabName = "downloadReport", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Plot Options",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            checkboxInput("se", "Add confidence interval around the regression line", TRUE),
            textInput("xlab1", label = "X1 Label:", value = "Number of Website Visitors per Month"),
            textInput("xlab2", label = "X2 Label:", value = "Number of Monthly Transactions"),
            textInput("xlab3", label = "X3 Label:", value = "Average Number of Items per Transaction"),
            textInput("xlab4", label = "X4 Label:", value = "Customer Satisfaction Rating (Scale 1-10)"),
            textInput("xlab5", label = "X5 Label:", value = "Number of Online Advertisements Run per Month"),
            textInput("ylab", label = "Y Label:", value = "Monthly Sales Volume (in thousands of USD)")
          ),
          box(
            title = "Data Summary",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput("tbl"),
            uiOutput("data")
          ),
          box(
            title = "Regression Parameters",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            verbatimTextOutput("summary")
          ),
          box(
            title = "Predict",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            textInput("predict_X1", label = "Enter the value X1 :", value = ""),
            textInput("predict_X2", label = "Enter the value X2 :", value = ""),
            textInput("predict_X3", label = "Enter the value X3 :", value = ""),
            textInput("predict_X4", label = "Enter the value X4 :", value = ""),
            textInput("predict_X5", label = "Enter the value X5 :", value = ""),
            actionButton("predictButton", "Predict"),
            verbatimTextOutput("hasilPredict")
          ),
          box(
            title = "Assumptions",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotOutput("assumptions")
          ),
          box(
            title = "Interaction ANOVA",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            verbatimTextOutput("interactionANOVA")
          ),
          box(
            title = "Interaction Plot",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("interactionPlot")
          )
        )
      ),
      tabItem(
        tabName = "downloadReport",
        box(
          title = "Download Report",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          radioButtons("format", "Download report:", c("HTML"), inline = TRUE),
          checkboxInput("echo", "Show code in report?", FALSE),
          downloadButton("downloadReportButton", label = "Download Report")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Use the provided dataset
  input_data <- reactiveVal(user_data)
  
  output$tbl <- DT::renderDT({
    data <- input_data()
    DT::datatable(data, extensions = "Buttons", options = list(
      lengthChange = FALSE,
      dom = "Blfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print")
    ))
  })
  
  output$data <- renderUI({
    data <- input_data()
    withMathJax(
      paste0("\\(\\bar{X1} =\\) ", round(mean(data$X1), 3)),
      br(),
      paste0("\\(\\bar{X2} =\\) ", round(mean(data$X2), 3)),
      br(),
      paste0("\\(\\bar{X3} =\\) ", round(mean(data$X3), 3)),
      br(),
      paste0("\\(\\bar{X4} =\\) ", round(mean(data$X4), 3)),
      br(),
      paste0("\\(\\bar{X5} =\\) ", round(mean(data$X5), 3)),
      br(),
      paste0("\\(\\bar{y} =\\) ", round(mean(data$y), 3)),
      br(),
      paste0("\\(n =\\) ", nrow(data))
    )
  })
  
  output$summary <- renderPrint({
    data <- input_data()
    fit <- lm(y ~ X1 + X2 + X3 + X4 + X5, data = data)
    summary(fit)
  })
  
  output$assumptions <- renderPlot({
    data <- input_data()
    fit <- lm(y ~ X1 + X2 + X3 + X4 + X5, data = data)
    par(mfrow = c(2, 2))
    plot(fit, which = c(1:3, 5))
  })
  
  output$hasilPredict <- renderPrint({
    req(input$predictButton)
    new_data <- data.frame(
      X1 = as.numeric(input$predict_X1),
      X2 = as.numeric(input$predict_X2),
      X3 = as.numeric(input$predict_X3),
      X4 = as.numeric(input$predict_X4),
      X5 = as.numeric(input$predict_X5)
    )
    
    fit_val <- lm(y ~ X1 + X2 + X3 + X4 + X5, data = input_data())
    nilai_prediksi <- predict(fit_val, newdata = new_data)
    paste("Predict Value:", round(nilai_prediksi, 2))
  })
  
  output$interactionANOVA <- renderPrint({
    data <- input_data()
    if (is.null(data)) {
      return(NULL)
    }
    fit <- aov(y ~ X1 * X5, data = data)  
    summary(fit)
  })
  
  output$interactionPlot <- renderPlotly({
    data <- input_data()
    fit <- lm(y ~ X1 + X5, data = data)
    p <- ggplot(data, aes_string(x = "X1", y = "y", color = "X5")) +
      geom_point() +
      stat_smooth(method = "lm", se = input$se) +
      ylab(input$ylab) +
      xlab(input$xlab1) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$downloadReportButton <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, HTML = "html"
      ))
    },
    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format, HTML = html_document()
      ))
      file.rename(out, file)
    }
  )
}

shinyApp(ui = ui, server = server)
