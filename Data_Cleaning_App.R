library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(rmarkdown)
library(stats)
library(tidyr)
library(rlang)

# Built-in datasets
builtin_datasets <- list(
  "iris" = iris,
  "mtcars" = mtcars,
  "ToothGrowth" = ToothGrowth
)

ui <- fluidPage(
  titlePanel("Data Cleaner for ML"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_choice", "Choose Data Source:",
                   choices = c("Upload CSV" = "upload", "Use Built-in Dataset" = "builtin"),
                   selected = "upload"),
      
      conditionalPanel("input.data_choice == 'upload'",
                       fileInput("file", "Upload CSV File", accept = ".csv")),
      
      conditionalPanel("input.data_choice == 'builtin'",
                       selectInput("builtin_select", "Select Dataset:", choices = names(builtin_datasets))),
      
      hr(),
      uiOutput("drop_vars_ui"),
      actionButton("drop_vars_btn", "Drop Selected Variables"),
      hr(),
      uiOutput("var_select_ui"),
      actionButton("analyze_btn", "Analyze Dependent Variable"),
      br(), br(),
      actionButton("summary_btn", "Show Summary of Other Variables"),
      br(), br(),
      uiOutput("num_to_factor_ui"),
      actionButton("convert_num2fac", "Convert Selected to Factor"),
      br(), br(),
      uiOutput("factor_to_num_ui"),
      actionButton("convert_fac2num", "Convert Selected to Numeric"),
      br(), br(),
      actionButton("missing_btn", "Check Missing Values"),
      br(), br(),
      actionButton("bivariate_btn", "Run Bivariate Analysis"),
      br(), br(),
      h4("Export Cleaned Output"),
      radioButtons("report_format", "Select Report Format", choices = c("HTML", "PDF"), inline = TRUE),
      checkboxInput("include_code", "Include R Code in Report", value = FALSE),
      downloadButton("download_report", "Download Report")
    ),
    
    mainPanel(
      h4("Dataset Info"),
      verbatimTextOutput("dim_output"),
      h4("Structure of Data"),
      verbatimTextOutput("str_output"),
      h4("Dependent Variable Analysis"),
      uiOutput("var_analysis"),
      h4("Summary of Other Variables"),
      uiOutput("summary_output"),
      h4("Missing Values"),
      dataTableOutput("missing_output"),
      h4("Bivariate Analysis Results"),
      dataTableOutput("bivariate_output")
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    if (input$data_choice == "upload") {
      req(input$file)
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else {
      req(input$builtin_select)
      builtin_datasets[[input$builtin_select]]
    }
  })
  
  rv <- reactiveValues(data = NULL)
  
  observeEvent(raw_data(), {
    rv$data <- raw_data()
  })
  
  output$drop_vars_ui <- renderUI({
    req(rv$data)
    selectInput("drop_vars", "Select Variables to Drop:", choices = names(rv$data), multiple = TRUE)
  })
  
  observeEvent(input$drop_vars_btn, {
    req(input$drop_vars)
    rv$data <- rv$data[, !(names(rv$data) %in% input$drop_vars), drop = FALSE]
  })
  
  output$var_select_ui <- renderUI({
    req(rv$data)
    selectInput("dep_var", "Select Dependent Variable", choices = names(rv$data))
  })
  
  output$num_to_factor_ui <- renderUI({
    req(rv$data)
    num_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
    selectInput("num2fac", "Convert Numeric to Factor:", choices = num_vars, multiple = TRUE)
  })
  
  output$factor_to_num_ui <- renderUI({
    req(rv$data)
    fac_vars <- names(rv$data)[sapply(rv$data, is.factor)]
    selectInput("fac2num", "Convert Factor to Numeric:", choices = fac_vars, multiple = TRUE)
  })
  
  observeEvent(input$convert_num2fac, {
    req(input$num2fac)
    for (var in input$num2fac) {
      rv$data[[var]] <- as.factor(rv$data[[var]])
    }
  })
  
  observeEvent(input$convert_fac2num, {
    req(input$fac2num)
    for (var in input$fac2num) {
      rv$data[[var]] <- as.numeric(as.character(rv$data[[var]]))
    }
  })
  
  output$dim_output <- renderPrint({
    req(rv$data)
    dim(rv$data)
  })
  
  output$str_output <- renderPrint({
    req(rv$data)
    str(rv$data)
  })
  
  output$var_analysis <- renderUI({
    req(input$analyze_btn)
    req(input$dep_var)
    isolate({
      df <- rv$data
      var <- df[[input$dep_var]]

      if (is.numeric(var)) {
        plotOutput("dep_plot")
      } else {
        dataTableOutput("dep_table")
      }
    })
  })

  output$dep_plot <- renderPlot({
    req(input$analyze_btn)
    req(input$dep_var)
    df <- rv$data
    var <- df[[input$dep_var]]
    validate(need(is.numeric(var), "Dependent variable is not numeric."))

    ggplot(df, aes_string(input$dep_var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(title = paste("Distribution of", input$dep_var))
  })

  output$dep_table <- renderDataTable({
    req(input$analyze_btn)
    req(input$dep_var)
    df <- rv$data
    var <- df[[input$dep_var]]
    validate(need(!is.numeric(var), "Dependent variable is not categorical."))

    tab <- df %>%
      count(!!sym(input$dep_var)) %>%
      mutate(Proportion = round(n / sum(n), 3))

    datatable(tab)
  })
  
  output$summary_output <- renderUI({
    req(input$summary_btn)
    isolate({
      df <- rv$data
      dep <- input$dep_var
      summary_df <- df[, names(df) != dep, drop = FALSE]
      
      renderPrint({
        summary(summary_df)
      })
    })
  })
  
  output$missing_output <- renderDataTable({
    req(input$missing_btn)
    df <- rv$data
    missing_summary <- sapply(df, function(x) sum(is.na(x)) / length(x)) * 100
    data.frame(
      Variable = names(missing_summary),
      Missing_Percent = round(missing_summary, 2)
    )
  })
  
  output$bivariate_output <- renderDataTable({
    req(input$bivariate_btn)
    df <- rv$data
    dep <- input$dep_var
    indeps <- setdiff(names(df), dep)
    res <- data.frame(Variable = character(), Method = character(), P_Value = numeric(), Significance = character())
    
    for (var in indeps) {
      p <- NA
      method <- ""
      sig <- ""
      try({
        if (is.numeric(df[[dep]]) && is.numeric(df[[var]])) {
          test <- cor.test(df[[dep]], df[[var]], method = "pearson")
          p <- test$p.value
          method <- "Pearson Correlation"
        } else if (is.numeric(df[[dep]]) && is.factor(df[[var]])) {
          if (nlevels(df[[var]]) == 2) {
            test <- t.test(df[[dep]] ~ df[[var]])
            p <- test$p.value
            method <- "T-test"
          } else {
            test <- aov(df[[dep]] ~ df[[var]])
            p <- summary(test)[[1]][["Pr(>F)"]][1]
            method <- "ANOVA"
          }
        } else if (is.factor(df[[dep]]) && is.numeric(df[[var]])) {
          if (nlevels(df[[dep]]) == 2) {
            test <- t.test(df[[var]] ~ df[[dep]])
            p <- test$p.value
            method <- "T-test"
          } else {
            test <- aov(df[[var]] ~ df[[dep]])
            p <- summary(test)[[1]][["Pr(>F)"]][1]
            method <- "ANOVA"
          }
        } else if (is.factor(df[[dep]]) && is.factor(df[[var]])) {
          tbl <- table(df[[dep]], df[[var]])
          if (all(tbl >= 5)) {
            test <- chisq.test(tbl)
            p <- test$p.value
            method <- "Chi-Square"
          } else {
            test <- fisher.test(tbl)
            p <- test$p.value
            method <- "Fisher's Exact"
          }
        }
        
        if (!is.na(p)) {
          if (p < 0.001) sig <- "***"
          else if (p < 0.01) sig <- "**"
          else if (p < 0.05) sig <- "*"
          else sig <- ""
          
          res <- rbind(res, data.frame(Variable = var, Method = method, P_Value = round(p, 5), Significance = sig))
        }
      }, silent = TRUE)
    }
    datatable(res)
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("cleaning_report_", Sys.Date(), ifelse(input$report_format == "PDF", ".pdf", ".html"))
    },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      echo_setting <- if (isTRUE(input$include_code)) "TRUE" else "FALSE"
      rmd <- paste0(
        "---\n",
        "title: 'Data Cleaning Summary'\n",
        "output: ", ifelse(input$report_format == "PDF", "pdf_document", "html_document"), "\n",
        "params:\n  data: NA\n  dep_var: '", input$dep_var, "'\n---\n\n",
        "```{r setup, include=FALSE}\n",
        "library(dplyr)\nlibrary(ggplot2)\nlibrary(rlang)\n```\n\n",
        "## Dataset Dimensions\n```{r, echo=", echo_setting, "}\ndim(params$data)\n```\n\n",
        "## Data Structure\n```{r, echo=", echo_setting, "}\nstr(params$data)\n```\n\n",
        "## Summary of Variables\n```{r, echo=", echo_setting, "}\nsummary(params$data[, names(params$data) != params$dep_var])\n```\n\n",
        "## Missing Value Percentages\n```{r, echo=", echo_setting, "}\nsapply(params$data, function(x) sum(is.na(x))/length(x)) * 100\n```\n\n",
        "## Dependent Variable Analysis\n```{r, echo=", echo_setting, "}\n",
        "if (is.numeric(params$data[[params$dep_var]])) {\n",
        "  ggplot(params$data, aes_string(params$dep_var)) +\n    geom_histogram(bins = 30, fill = 'steelblue', color = 'white') +\n    theme_minimal()\n",
        "} else {\n  dplyr::count(params$data, !!sym(params$dep_var))\n}\n```"
      )
      writeLines(rmd, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file,
                        params = list(data = rv$data, dep_var = input$dep_var),
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
