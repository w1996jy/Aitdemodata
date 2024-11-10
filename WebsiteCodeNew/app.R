library("golem")
library(shiny)
library(bslib)
library(bsicons)
library(readxl)
library(dplyr)
library(writexl)
library(shinyWidgets)
library(shinyFiles)
library(readr)
library(psych)
library(pheatmap)
library(ggplot2)
library(DT)
library(DESeq2)
library(SummarizedExperiment)
library(matrixStats)
library(Mfuzz)
library(tidyverse)
library(Biobase)
library(VIM)
library(KmeansTrendAnalyzer)
library(CCA)
library(ropls)
library(VennDiagram)
library(grid)
library(tikzDevice)
library(ggridges)
library(seqinr)
library(Biostrings)
library(ggseqlogo)

options(shiny.maxRequestSize = 50 * 1024^2)  # 设置为50MB

#' @title Shiny App UI Function
#' @description This function defines the user interface (UI) for the Shiny app.
#' @param request Shiny request
#' @name app_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      theme = bs_theme(bootswatch = "lumen"),  # 选择默认主题
      title = "Ait",
      nav_panel(
        "Home page",
        icon = bs_icon("bank"),
        homepage_ui("homepage")
      ),
      nav_menu(
        "Data processing",
        icon = bs_icon("card-checklist"),
        merge_file_ui("merge_file"),
        dataTransform_ui("dataTransform"),
        Long_Wide_Data_T_ui("Long_Wide_Data_T"),
        Grouping_statistics_ui("Grouping_statistics"),
        Grouping_sorting_ui("Grouping_sorting"),
        describe_ui("describe"),
        randomNum_ui("randomNum")
      ),
      nav_menu(
        "Analyse",
        icon = bs_icon("tools"),
        cor_ui("cor"),
        corxy_ui("corxy"),
        deseq2_ui("deseq2"),
        mfuzz_ui("mfuzz"),
        kmeans_ui("kmeans"),
        pca_ui("pca"),
        CCA_ui("CCA"),
        OPLS_DA_ui("OPLS_DA")
      ),
      nav_menu(
        "Plot",
        icon = bs_icon("wrench"),
        bar_ui("bar"),
        Venn_ui("Venn"),
        enrichment_bubble_ui("enrichment_bubble"),
        VolcanoPlot_ui("VolcanoPlot"),
        histogram_ui("histogram"),
        upset_ui("upset"),
        GO_bar_class_ui("GO_bar_class"),
        ridgePlot_ui("ridgePlot"),
        boxplot_ui("boxplot"),
        pie_ui("pie")
      ),
      nav_menu(
        "Sequence",
        icon = bs_icon("check2-all"),
        sequence_extract_ui("sequence_extract"),
        reverse_complement_ui("reverse_complement"),
        seqLogo_ui("seqLogo")
      ),
      nav_panel(
        "Web",
        icon = bs_icon("rocket-takeoff"),
        web_ui("web")
      ),
      nav_panel(
        "Help",
        icon = bs_icon("exclamation-circle"),
        help_ui("help")
      )
      # flexible tools
      # footer = flexible_tools_ui("flexible_tools")
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @name golem_add_external_resources
#' @export
#'
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProteomeAPP"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "Ait")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv(
        "R_CONFIG_ACTIVE",
        "default"
      )
    ),
    use_parent = TRUE,
    # Modify this if your config file is somewhere else
    file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
#' Merge File UI Module
#' 
#' @description 
#' UI for merging files
#' 
#' @param id A unique identifier for the module.
#' 
merge_file_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabPanel("merge_file",
             titlePanel("Merge file"),
             sidebarLayout(
               sidebarPanel(
                 fileInput(ns("file1"), "Choose first Excel file", accept = c(".xlsx")),
                 fileInput(ns("file2"), "Choose second Excel file", accept = c(".xlsx")),
                 uiOutput(ns("sheet_ui1")),
                 uiOutput(ns("sheet_ui2"))
               ),
               mainPanel(
                 uiOutput(ns("col_ui1")),
                 uiOutput(ns("col_ui2")),
                 actionButton(ns("join_btn"), "Join Files"),
                 downloadButton(ns("downloadData"), "Download")
               )
             )
    ),
    tabPanel("Preview",
             mainPanel(
               tableOutput(ns("preview"))
             )
    )
  )
}
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @name app_server
#' @export
#'
app_server <- function(input, output, session) {
  # Your application server logic
  bslib::bs_themer()
  callModule(merge_file_server, "merge_file")
  callModule(dataTransform_server, "dataTransform")
  callModule(Long_Wide_Data_T_server, "Long_Wide_Data_T")
  callModule(Grouping_statistics_server, "Grouping_statistics")
  callModule(Grouping_sorting_server, "Grouping_sorting")
  callModule(describe_server, "describe")
  callModule(randomNum_server, "randomNum")
  callModule(cor_server, "cor")
  callModule(corxy_server, "corxy")
  callModule(deseq2_server, "deseq2")
  callModule(mfuzz_server, "mfuzz")
  callModule(kmeans_server, "kmeans")
  callModule(pca_server, "pca")
  callModule(CCA_server, "CCA")
  callModule(OPLS_DA_server, "OPLS_DA")
  callModule(Bar_server, "bar")
  callModule(Venn_server, "Venn")
  callModule(enrichment_bubble_server, "enrichment_bubble")
  callModule(VolcanoPlot_server, "VolcanoPlot")
  callModule(histogram_server, "histogram")
  callModule(upset_server, "upset")
  callModule(GO_bar_class_server, "GO_bar_class")
  callModule(ridgePlot_server, "ridgePlot")
  callModule(boxplot_server, "boxplot")
  callModule(pie_server, "pie")
  callModule(sequence_extract_server, "sequence_extract")
  callModule(reverse_complement_server, "reverse_complement")
  callModule(seqLogo_server, "seqLogo")
}
#' homepage UI Module
#' @description homepage UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title homepage_ui
#' @name homepage_ui
#' @import shiny
#' @import bslib
#' @export
#'
homepage_ui <- function(id) {
  fluidPage(
    # Software Introduction Section
    div(style = "font-family: 'Times New Roman'; width: 50%; margin: auto; margin-bottom: 20px; font-size: 16px;",
        h2("Software Introduction", style = "font-size: 20px;"),
        p("Ait V.1.0.0 is a powerful bioinformatics analysis tool built on the Shiny framework, designed to help researchers and data analysts easily perform data processing, statistical analysis, and visualization. The application features an intuitive user interface that supports various data analysis functions, making it suitable for in-depth exploration of biological and experimental data."),
        h3("Key Features:", style = "font-size: 18px;"),
        tags$ul(
          tags$li("Data Processing: Functions such as file merging, data transformation, long-wide data transformation, grouping statistics, and descriptive statistics enable users to efficiently manage and preprocess their data."),
          tags$li("Analysis Tools: Includes correlation analysis, DESeq2 analysis, clustering analysis (Mfuzz and K-means), PCA, and OPLS-DA, helping users extract important patterns and relationships from their data."),
          tags$li("Rich Visualization Options: Offers various chart generation tools, including bar plots, Venn diagrams, volcano plots, box plots, and pie charts, allowing users to visually present their data results with customizable color choices and download options."),
          tags$li("Sequence Analysis: Supports sequence extraction, reverse complement, and sequence logo operations, facilitating in-depth analysis of genomic and transcriptomic data."),
          tags$li("Web: Many useful external links.")
        ),
        p("With Ait V.1.0.0, users can efficiently process and analyze complex data, enhancing research productivity and supporting scientific discoveries.")
    ),
    
    # Add Flowchart Section above the image
  )
}
#' Merge File UI Module
#' @description Merge File UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title merge_file
#' @name merge_file_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
merge_file_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Merge File',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose first Excel file", accept = c(".xlsx")),
          fileInput(ns("file2"), "Choose second Excel file", accept = c(".xlsx")),
          uiOutput(ns("sheet_ui1")),
          uiOutput(ns("sheet_ui2"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                mainPanel(
                  uiOutput(ns("col_ui1")),
                  uiOutput(ns("col_ui2"))
                )
              ),
              accordion_panel(
                title = "Run",
                actionButton(ns("join_btn"), "Join Files")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadData"), "Download Table")
              )
            ),
            tabPanel(
              "Preview",
              mainPanel(
                DT::DTOutput(ns("preview")) # 使用 DTOutput 来显示交互式数据表
              )
            )
            
          )
        )
      )
    )
  )
}
#' Merge File Server Module
#' @description Server logic for merging files
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import readxl
#' @import dplyr
#' @import writexl
#' @import shinyWidgets
#' @import shinyFiles
#' @import DT
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom utils write.table
#' @export
#'
merge_file_server <- function(input, output, session) {
  ns <- session$ns
  
  data1 <- reactive({
    req(input$file1)
    req(input$sheet1)
    read_excel(input$file1$datapath, sheet = input$sheet1)
  })
  
  data2 <- reactive({
    req(input$file2)
    req(input$sheet2)
    read_excel(input$file2$datapath, sheet = input$sheet2)
  })
  
  observe({
    req(input$file1)
    sheets <- excel_sheets(input$file1$datapath)
    updateSelectInput(session, "sheet1", choices = sheets)
  })
  
  observe({
    req(input$file2)
    sheets <- excel_sheets(input$file2$datapath)
    updateSelectInput(session, "sheet2", choices = sheets)
  })
  
  output$sheet_ui1 <- renderUI({
    req(input$file1)
    selectInput(ns("sheet1"), "Select sheet from first file", choices = NULL)
  })
  
  output$sheet_ui2 <- renderUI({
    req(input$file2)
    selectInput(ns("sheet2"), "Select sheet from second file", choices = NULL)
  })
  
  output$col_ui1 <- renderUI({
    req(data1())
    selectInput(ns("col1"), "Select column from first file", choices = colnames(data1()))
  })
  
  output$col_ui2 <- renderUI({
    req(data2())
    selectInput(ns("col2"), "Select column from second file", choices = colnames(data2()))
  })
  
  joined_data <- eventReactive(input$join_btn, {
    req(data1(), data2(), input$col1, input$col2)
    left_join(data1(), data2(), by = setNames(input$col2, input$col1))
  })
  
  # 使用 DT 包的 renderDT
  output$preview <- DT::renderDT({
    req(joined_data())
    DT::datatable(joined_data()) # 生成交互式数据表
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("joined_data.xlsx")
    },
    content = function(file) {
      write_xlsx(joined_data(), path = file)
    }
  )
}
#' dataTransform UI Module
#' @description Merge File UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
dataTransform_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'dataTransform',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("datafile"), "Upload Data File (CSV)", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of Data Transformation & Normalization",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput(ns("transformation"), "Select Data Transformation Method",
                            choices = c("None" = "none",
                                        "Log2" = "log2",
                                        "Natural Log (LN)" = "ln",
                                        "Log10" = "log10",
                                        "Power2" = "power2",
                                        "Square Root (Sqrt)" = "sqrt")),
                selectInput(ns("normalization"), "Select Normalization Method",
                            choices = c("None" = "none",
                                        "Auto" = "auto",
                                        "Range" = "range",
                                        "MinMax" = "minmax",
                                        "MaxAbs" = "maxabs",
                                        "Log" = "log",
                                        "Vast" = "vast",
                                        "Pareto" = "pareto",
                                        "Level" = "level",
                                        "Robust" = "robust",
                                        "Median" = "median",
                                        "Center" = "center"))
              ),
              accordion_panel(
                title = "Run",
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("download"), "Download Table")
              )
            ),
            mainPanel(
              DT::DTOutput(ns("dataTable"))  # Use DTOutput for interactive table
            )
          )
        )
      )
    )
  )
}
# Server Function for Data Transformation Module
#' Data Transformation and Normalization Server Module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return None
#' @name dataTransform_server
#' @importFrom stats IQR median sd
#' @importFrom utils read.csv write.csv
#' @export
#'
dataTransform_server <- function(input, output, session) {
  
  # Reactive expression to read data with first column as row names
  data <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath, row.names = 1)
    df
  })
  
  # Reactive expression to apply transformations and normalization
  transformed_data <- reactive({
    req(input$run)  # Ensure 'Run' button is clicked before processing
    df <- data()
    
    # Apply transformation
    if (input$transformation == "log2") {
      df <- df %>% mutate(across(everything(), log2))
    } else if (input$transformation == "ln") {
      df <- df %>% mutate(across(everything(), log))
    } else if (input$transformation == "log10") {
      df <- df %>% mutate(across(everything(), log10))
    } else if (input$transformation == "power2") {
      df <- df %>% mutate(across(everything(), function(x) x^2))
    } else if (input$transformation == "sqrt") {
      df <- df %>% mutate(across(everything(), sqrt))
    }
    
    # Apply normalization
    if (input$normalization == "range" || input$normalization == "minmax") {
      df <- df %>% mutate(across(everything(), ~ (. - min(.)) / (max(.) - min(.))))
    } else if (input$normalization == "maxabs") {
      df <- df %>% mutate(across(everything(), ~ . / max(abs(.))))
    } else if (input$normalization == "log") {
      df <- df %>% mutate(across(everything(), function(x) log(x + 1)))
    } else if (input$normalization == "vast") {
      # Placeholder for 'vast' method
    } else if (input$normalization == "pareto") {
      df <- df %>% mutate(across(everything(), function(x) (x - mean(x)) / sqrt(sd(x))))
    } else if (input$normalization == "level") {
      # Placeholder for 'level' method
    } else if (input$normalization == "robust") {
      df <- df %>% mutate(across(everything(), function(x) (x - median(x)) / IQR(x)))
    } else if (input$normalization == "median") {
      df <- df %>% mutate(across(everything(), function(x) x - median(x)))
    } else if (input$normalization == "center") {
      df <- df %>% mutate(across(everything(), function(x) x - mean(x)))
    }
    
    df
  })
  
  output$dataTable <- renderDT({
    req(input$run)
    datatable(transformed_data(), rownames = TRUE)  # Display row names
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("transformed_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(transformed_data(), file, row.names = TRUE)
    }
  )
}
#' dataTransform UI Module
#' @description Merge File UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
Long_Wide_Data_T_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Long-Wide Data Transformation',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of Data Transformation & Normalization",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                radioButtons(ns("transformation"), "Select transformation type:",
                             choices = list("Long format" = "long", "Wide format" = "wide")),
                uiOutput(ns("columns"))
              ),
              accordion_panel(
                title = "Transform",
                actionButton(ns("transform"), "Transform Data"),
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadData"), "Download result")
              )
            ),
            mainPanel(
              h3("Original Data"),
              DTOutput(ns("originalData")),  # 显示上传的原始数据
              h3("Transformed Data"),
              DTOutput(ns("dataTable"))      # 显示转换后的数据
            )
          )
        )
      )
    )
  )
}

#' @import readr
#' @import DT
#' @import shiny
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom utils read.csv write.csv
Long_Wide_Data_T_server <- function(input, output, session) {
  ns <- session$ns
  
  # 读取上传的文件
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  
  # 动态生成选择列的 UI
  output$columns <- renderUI({
    req(data())
    
    if(input$transformation == "long") {
      checkboxGroupInput(ns("cols"), "Select columns to pivot to long format:",
                         choices = names(data()), selected = names(data())[1:2])
    } else {
      selectInput(ns("cols"), "Select the column to use as key:", choices = names(data()), selected = names(data())[1])
    }
  })
  
  # 使用 DT 显示上传的原始数据
  output$originalData <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, autoWidth = TRUE))  # 交互式表格显示原始数据
  })
  
  # 进行数据转换
  transformedData <- eventReactive(input$transform, {
    req(input$cols)
    
    if (input$transformation == "long") {
      tidyr::pivot_longer(data(), cols = all_of(input$cols), names_to = "variable", values_to = "value")
    } else {
      tidyr::pivot_wider(data(), names_from = all_of(input$cols), values_from = "value")
    }
  })
  
  # 使用 DT 显示转换后的数据
  output$dataTable <- renderDT({
    req(transformedData())
    datatable(transformedData(), options = list(pageLength = 10, autoWidth = TRUE))  # 交互式表格显示转换后的数据
  })
  
  # 提供下载转换后的数据
  output$downloadData <- downloadHandler(
    filename = function() { paste("transformed_data", ".csv", sep = "") },
    content = function(file) {
      write_csv(transformedData(), file)
    }
  )
}
#' Grouping_statistics UI Module
#' @description Grouping_statistics UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
Grouping_statistics_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Grouping statistics',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of join result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
                selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
                selectInput(ns("summary_func"), "Select summary function:",
                            choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n"))
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run Summary")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadData"), "Download Table")
              )
            ),
            mainPanel(
              DTOutput(ns("summaryTable"))
            )
          )
        )
      )
    )
  )
}
#' @title Grouping Statistics Server Function
#' @description This function defines the server logic for grouping statistics in a Shiny application.
#' @param input Shiny input object, used to access input values from the UI.
#' @param output Shiny output object, used to send output values to the UI.
#' @param session Shiny session object, used to manage session-level information.
#' @import readr
#' @import shiny
#' @import DT
#' @import dplyr
#' @importFrom utils read.csv write.csv
#' @export
#'
Grouping_statistics_server <- function(input, output, session) {
  ns <- session$ns
  # 读取上传的数据
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, show_col_types = FALSE)  # 隐藏列类型警告
  })
  
  # 动态更新选择列
  observe({
    req(data())
    
    # 更新选择输入，确保数据可用时才进行更新
    updateSelectInput(session, "group_column", choices = names(data()), selected = names(data())[1])
    updateSelectInput(session, "summary_column", choices = names(data()), selected = names(data())[2])
  })
  
  # 分组统计逻辑
  summaryData <- eventReactive(input$run, {
    req(input$group_column, input$summary_column)  # 确保所需输入存在
    
    # 根据选择的汇总函数动态生成汇总表达式
    summary_function <- switch(input$summary_func,
                               "mean" = mean,
                               "sum" = sum,
                               "n" = NULL)  # 当选择 n 时，不使用 summary_function
    
    # 执行分组汇总
    if (input$summary_func == "n") {
      # 对选择的列进行计数
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Count = n(), .groups = "drop")  # 添加 .groups = "drop" 来避免警告
    } else {
      # 对选择的列应用 mean 或 sum
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Summary = summary_function(get(input$summary_column), na.rm = TRUE), .groups = "drop")  # 添加 .groups = "drop"
    }
  })
  
  # 显示汇总表
  output$summaryTable <- renderDT({
    req(summaryData())
    datatable(summaryData(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  # 下载处理程序
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Grouping_statistics.csv")
    },
    content = function(file) {
      write.csv(summaryData(), file) # 使用 file 参数
    }
  )
  
}

#' Grouping_sorting UI Module
#' @description Merge File UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
Grouping_sorting_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    title = 'Grouping sorting',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
                selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
                selectInput(ns("summary_func"), "Select summary function:",
                            choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),
                
                selectInput(ns("sort_order"), "Select sort order:",
                            choices = c("Ascending" = "asc", "Descending" = "desc"))),
              accordion_panel(
                title = "Run",
                actionButton(ns("run"), "Run Summary")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadData"), "Download Table")
              )
            ),
            mainPanel(
              # h3("Grouped and Sorted Summary"),
              DTOutput(ns("summaryTable"))
            )
          )
        )
      )
    )
  )
}

#' @title Grouping sorting Server Function
#' @description This function defines the server logic for grouping statistics in a Shiny application.
#' @param input Shiny input object, used to access input values from the UI.
#' @param output Shiny output object, used to send output values to the UI.
#' @param session Shiny session object, used to manage session-level information.
#' @import readr
#' @import dplyr
#' @export
#'
Grouping_sorting_server <- function(input, output, session) {
  # 读取上传的数据
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, show_col_types = FALSE)  # 隐藏列类型警告
  })
  
  # 动态更新选择列
  observe({
    req(data())
    updateSelectInput(session, "group_column", choices = names(data()), selected = names(data())[1])
    updateSelectInput(session, "summary_column", choices = names(data()), selected = names(data())[2])
  })
  
  # 分组统计逻辑
  summaryData <- eventReactive(input$run, {
    req(input$group_column, input$summary_column)  # 确保所需输入存在
    
    # 根据选择的汇总函数动态生成汇总表达式
    summary_function <- switch(input$summary_func,
                               "mean" = mean,
                               "sum" = sum,
                               "n" = NULL)  # 当选择 n 时，不使用 summary_function
    
    # 执行分组汇总
    summary_df <- if (input$summary_func == "n") {
      # 对选择的列进行计数
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      # 对选择的列应用 mean 或 sum
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Summary = summary_function(get(input$summary_column), na.rm = TRUE), .groups = "drop")
    }
    
    # 根据选择的排序顺序进行排序
    if (input$sort_order == "asc") {
      summary_df <- summary_df %>% arrange(across(everything()))
    } else {
      summary_df <- summary_df %>% arrange(desc(across(everything())))
    }
    
    return(summary_df)
  })
  
  # 显示汇总表
  output$summaryTable <- renderDT({
    req(summaryData())
    datatable(summaryData(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # 下载分组统计结果
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("summary_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summaryData(), file, row.names = FALSE)
    }
  )
}
#' Describe UI Module
#' @name describe_ui
#' @description describe UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
describe_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    title = 'Descriptive Statistics',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("data_file"), "Upload CSV data")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_analysis"), "Run")
        ),
        accordion_panel(
          title = "Download",
          downloadButton(ns("download_table"), "Download Table")
        )
      ),
      mainPanel(
        DTOutput(ns("desc_table"))
      )
    )
  )
}

#' Server for Descriptive Statistics
#' @name describe_server
#' @description Handles the server-side logic for descriptive statistics analysis
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom psych describe
#' @importFrom DT datatable
#' @importFrom utils read.csv
#' @export
describe_server <- function(input, output, session) {
  ns <- session$ns
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, row.names = 1)
  })
  
  desc_stats <- eventReactive(input$run_analysis, {
    req(data())
    psych::describe(data())
  })
  
  output$desc_table <- renderDT({
    req(desc_stats())
    DT::datatable(as.data.frame(desc_stats()), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      "descriptive_statistics.csv"
    },
    content = function(file) {
      write.csv(as.data.frame(desc_stats()), file)
    },
    contentType = "text/csv"
  )
}
#' randomNum UI Module
#' @description randomNum UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
randomNum_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'randomNum',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Parameter",
          selectInput(ns("distribution"), "Select Distribution:",
                      choices = c("Uniform" = "uniform",
                                  "Normal" = "normal",
                                  "Binomial" = "binomial",
                                  "Poisson" = "poisson")),
          numericInput(ns("n"), "Number of Random Numbers:", value = 10, min = 1),
          
          conditionalPanel(
            condition = "input.distribution == 'normal'",
            numericInput(ns("mean"), "Mean:", value = 0),
            numericInput(ns("sd"), "Standard Deviation:", value = 1)
          ),
          
          conditionalPanel(
            condition = "input.distribution == 'binomial'",
            numericInput(ns("size"), "Number of Trials:", value = 10),
            numericInput(ns("prob"), "Probability of Success:", value = 0.5, min = 0, max = 1)
          ),
          
          conditionalPanel(
            condition = "input.distribution == 'poisson'",
            numericInput(ns("lambda"), "Lambda (Mean):", value = 3)
          )
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("generate"), "Run")
        ),
        accordion_panel(
          title = "Download",
          downloadButton(ns("downloadData"), "Download Table")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            mainPanel(
              DTOutput(ns("resultsTable"))  # 显示结果表格
            )
          )
        )
      )
    )
  )
}
# Server部分
#' @description
#' A short description...
#' @name randomNum_server
#' @title randomNum_server
#' @param input description
#' @param output description
#' @param session description
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
randomNum_server <- function(input, output, session) {
  ns <- session$ns
  result_data <- reactiveVal(NULL)  # 存储生成的随机数
  
  observeEvent(input$generate, {
    req(input$n)
    result <- switch(input$distribution,
                     uniform = runif(input$n),
                     normal = rnorm(input$n, mean = input$mean, sd = input$sd),
                     binomial = rbinom(input$n, size = input$size, prob = input$prob),
                     poisson = rpois(input$n, lambda = input$lambda))
    
    result_data(data.frame(Random_Numbers = result))  # 存储为数据框
    
    output$resultsTable <- renderDT({
      datatable(result_data(), options = list(pageLength = 5))
    })
  })
  
  # 下载CSV文件
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("random_numbers_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(result_data(), file)
    }
  )
}
#' cor UI Module
#' @description cor UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
cor_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Correlation analysis',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("data_file"), "Upload CSV data")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_analysis"), "Run")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 300,
          navset_card_tab(
            height = 300,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                textInput(ns("plot_width"), "Plot Width (inches):", value = "10"),
                textInput(ns("plot_height"), "Plot Height (inches):", value = "10"),
                textInput(ns("fontsize"), "Font Size:", value = "15"),
                radioButtons(ns("color_scheme"), "Color Scheme:",
                             choices = list("Red-Blue" = "RdBu", "Green-Blue" = "GnBu", "Heat" = "heat"))
              ),
              accordion_panel(
                title = 'Download',
                radioButtons(ns("download_format"), "Select download format:",
                             choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                downloadButton(ns("download_plot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("heatmap_plot")
              ))
          )
        ),
        fluidRow(  # Use fluidRow to arrange elements horizontally
          column(
            width = 6,  # Each column will take half of the row width
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Table of correlation",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = "Download",
                  downloadButton(ns("download_table"), "Download")
                )
              ),
              mainPanel(
                DTOutput(ns("cor_table"))
              )
            )
          ),
          column(
            width = 6,  # Another half of the row
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Table of Pvalue",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = "Download",
                  downloadButton(ns("download_p_table"), "Download")
                )
              ),
              mainPanel(
                DTOutput(ns("pvalue_table"))
              )
            )))
      )
    )
  )
}

#' cor Server Module
#' @description Server logic for cor
#' @name cor_server
#' @param input, output, session Standard shiny server arguments
#' @import shiny
#' @import pheatmap
#' @import dplyr
#' @import writexl
#' @import pheatmap
#' @import ggplot2
#' @import DT
#' @importFrom dplyr left_join
#' @importFrom grDevices colorRampPalette
#' @importFrom stats setNames cor.test rbinom rnorm rpois runif
#' @importFrom utils read.csv
#' @export
#'
utils::globalVariables(c("calc_cor_and_pval", "colorRampPalette", "runif", "rnorm",
                         "rbinom", "rpois"))
cor_server <- function(input, output, session) {
  ns <- session$ns
  data <- reactive({
    req(input$data_file)
    # Read data and ensure all columns are numeric, removing non-numeric columns
    raw_data <- read.csv(input$data_file$datapath, row.names = 1)
    numeric_data <- raw_data[sapply(raw_data, is.numeric)]  # Keep only numeric columns
    numeric_data
  })
  
  calc_cor_and_pval <- function(data) {
    n <- ncol(data)
    cor_matrix <- matrix(NA, n, n)
    pval_matrix <- matrix(NA, n, n)
    rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)
    rownames(pval_matrix) <- colnames(pval_matrix) <- colnames(data)
    for (i in 1:n) {
      for (j in i:n) {
        test <- cor.test(data[, i], data[, j])
        cor_matrix[i, j] <- test$estimate
        cor_matrix[j, i] <- test$estimate
        pval_matrix[i, j] <- test$p.value
        pval_matrix[j, i] <- test$p.value
      }
    }
    list(cor = cor_matrix, pval = pval_matrix)
  }
  
  cor_and_pval <- eventReactive(input$run_analysis, {
    req(data())
    calc_cor_and_pval(data())
  })
  
  output$heatmap_plot <- renderPlot({
    req(cor_and_pval())
    print(cor_and_pval()$cor)
    color_scheme <- switch(input$color_scheme,
                           "RdBu" = colorRampPalette(c("greenyellow", "white", "red"))(100),
                           "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                           "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
    pheatmap(cor_and_pval()$cor,
             display_numbers = TRUE,
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             fontsize = as.numeric(input$fontsize),
             color = color_scheme)
  })
  
  output$cor_table <- renderDT({
    req(cor_and_pval())
    cor_df <- as.data.frame(cor_and_pval()$cor)
    datatable(cor_df, rownames = TRUE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  output$pvalue_table <- renderDT({
    req(cor_and_pval())
    pval_df <- as.data.frame(cor_and_pval()$pval)
    datatable(pval_df, rownames = TRUE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("correlation_heatmap.", input$download_format, sep = "")
    },
    content = function(file) {
      color_scheme <- switch(input$color_scheme,
                             "RdBu" = colorRampPalette(c("greenyellow", "white", "red"))(100),
                             "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                             "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
      ggsave(file, plot = pheatmap(cor_and_pval()$cor,
                                   display_numbers = TRUE,
                                   cluster_rows = FALSE,
                                   cluster_cols = FALSE,
                                   fontsize = as.numeric(input$fontsize),
                                   color = color_scheme,
                                   silent = TRUE)$gtable,
             device = input$download_format,
             width = as.numeric(input$plot_width),
             height = as.numeric(input$plot_height))
    },
    contentType = "image"
  )
  
  output$download_table <- downloadHandler(
    filename = function() {
      "correlation_table.csv"
    },
    content = function(file) {
      write.csv(as.data.frame(cor_and_pval()$cor), file)
    },
    contentType = "text/csv"
  )
  
  output$download_p_table <- downloadHandler(
    filename = function() {
      "pvalue_table.csv"
    },
    content = function(file) {
      write.csv(as.data.frame(cor_and_pval()$pval), file)
    },
    contentType = "text/csv"
  )
}
#' @title Calculate the correlation between two sets of data
#' @name corxy_ui
#' @description corxy UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @export
#'
corxy_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Correlation Analysis of Two Traits',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = c(".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Fig of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("xcol")),  # Dropdown for selecting x-axis column
                uiOutput(ns("ycol")),  # Dropdown for selecting y-axis column
                textInput(ns("xaxis"), "X Axis Title", "X Axis"),
                textInput(ns("yaxis"), "Y Axis Title", "Y Axis"),
                colourpicker::colourInput(ns("pointColor"), "Choose Point Color", value = "black"),
                colourpicker::colourInput(ns("lineColor"), "Choose Line Color", value = "blue"),
                radioButtons(ns("format"), "Choose Download Format", choices = c("PDF", "PNG", "JPG", "SVG")),
                numericInput(ns("width"), "Width (in inches)", value = 7, min = 1, max = 20),
                numericInput(ns("height"), "Height (in inches)", value = 5, min = 1, max = 20)
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadPlot"), "Download Plot"),
                helpText("Select the columns for x and y axes, adjust titles,
                         colors, size, and format, and download the plot.")
              )
            ),
            mainPanel(
              plotOutput(ns("plot"))
            )
          )
        )
      )
    )
  )
}
#' @title Calculate the correlation between two sets of data server
#' @name corxy_server
#' @description corxy UI Module
#' @param input A unique identifier for the Shiny namespace.
#' @param output A unique identifier for the Shiny namespace.
#' @param session A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom stats lm
#' @importFrom colourpicker colourInput
#' @export
corxy_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    validate(need(ncol(df) >= 2, "The CSV file must have at least two columns"))
    df
  })
  
  # Generate UI for selecting x and y columns
  output$xcol <- renderUI({
    df <- dataInput()
    selectInput(ns("xcol"), "X Axis", names(df), selected = names(df)[1])
  })
  
  output$ycol <- renderUI({
    df <- dataInput()
    selectInput(ns("ycol"), "Y Axis", names(df), selected = names(df)[2])
  })
  
  # Generate the plot
  plotInput <- reactive({
    data <- dataInput()
    
    # Ensure the selected columns are numeric
    x <- data[[input$xcol]]
    y <- data[[input$ycol]]
    
    validate(
      need(is.numeric(x), "The selected X axis column must be numeric"),
      need(is.numeric(y), "The selected Y axis column must be numeric")
    )
    
    # Create linear model
    model <- lm(y ~ x)
    
    # Calculate the position for the annotation (center on x-axis and top of y-axis)
    annotate_x <- mean(range(x))
    annotate_y <- max(y) + 0.05 * (max(y) - min(y))  # Slightly above the top of y range
    
    # Create plot
    ggplot(data, aes(x = x, y = y)) +
      geom_point(color = input$pointColor) +
      geom_smooth(method = "lm", se = FALSE, color = input$lineColor) +
      annotate("text", x = annotate_x, y = annotate_y,
               label = as.character(as.expression(paste0("y = ", format(model[["coefficients"]][[2]], digits = 2), "x + ",
                                                         format(model[["coefficients"]][[1]], digits = 2),
                                                         ", R2 = ", format(summary(model)$r.squared, digits = 3)))),
               vjust = 1, hjust = 0.5) +
      labs(x = input$xaxis, y = input$yaxis) +
      theme_bw()
  })
  
  output$plot <- renderPlot({
    plotInput()
  })
  
  # Downloadable plot handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", switch(input$format, "PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"), sep = ".")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = switch(input$format, "PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
             width = input$width, height = input$height)
    }
  )
}
#' Draw a deseq2
#' @description Creates a UI for DESeq2 analysis.
#' @param id A time-series omics matrix.
#' @import shiny
#' @import shinythemes
#' @import dashboardthemes
#' @import shinydashboard
#' @import shinyWidgets
#' @import DT
#' @import DESeq2
#' @import pheatmap
#' @import ggplot2
#' @name deseq2_ui
#' @export
#'
deseq2_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'DESeq2',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("count_file"), "Upload count data (CSV)"),
          fileInput(ns("meta_file"), "Upload metadata (CSV)")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_analysis"), "Run")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 300,
          navset_card_tab(
            height = 300,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              accordion_panel(
                title = "Download",
                downloadButton(ns("download_results"), "Download Table")
              )
            ),
            mainPanel(
              DTOutput(ns("results_table"))
            )
          )
        ),
        fluidRow(  # Use fluidRow for horizontal alignment
          column(
            width = 6,  # Half of the row
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Volcano Plot",
              sidebar = accordion(
                accordion_panel(
                  title = "Parameter",
                  radioButtons(ns("download_format"), "Select download format:",
                               choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                  downloadButton(ns("download_volcano_plot"), "Download")
                )
              ),
              mainPanel(
                plotOutput(ns("volcano_plot"))
              )
            )
          ),
          column(
            width = 6,  # Half of the row
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Heatmap plot",
              sidebar = accordion(
                accordion_panel(
                  title = "Parameter",
                  radioButtons(ns("download_format"), "Select download format:",
                               choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                  downloadButton(ns("download_heatmap_plot"), "Download Heatmap")
                )
              ),
              mainPanel(
                plotOutput(ns("heatmap_plot"))
              )
            )
          )
        )
      )
    )
  )
}

#' DESeq2 Server Module
#' @description Server logic for DESeq2 analysis
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @importFrom DESeq2 DESeqDataSetFromMatrix
#' @importFrom DESeq2 DESeq
#' @importFrom DESeq2 results
#' @import pheatmap
#' @import ggplot2
#' @import SummarizedExperiment
#' @importFrom utils head write.csv
#' @importFrom matrixStats rowVars
#' @importFrom stats setNames
#' @name deseq2_server
#' @export
#'
utils::globalVariables(c("log2FoldChange", "pvalue", "color"))
deseq2_server <- function(input, output, session) {
  observeEvent(input$run_analysis, {
    req(input$count_file, input$meta_file)
    
    count_data <- read.csv(input$count_file$datapath, row.names = 1)
    if (ncol(count_data) < 2) {
      showNotification("Count data must have at least two columns", type = "error")
      return()
    }
    
    meta_data <- read.csv(input$meta_file$datapath, row.names = 1)
    if (!"Condition" %in% colnames(meta_data)) {
      showNotification("Metadata must contain 'Condition' column", type = "error")
      return()
    }
    
    dds <- DESeqDataSetFromMatrix(countData = count_data, colData = meta_data, design = ~ Condition)
    dds <- DESeq(dds)
    results <- results(dds)
    
    output$results_table <- renderDT({
      datatable(
        as.data.frame(results),
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })
    
    volcano_plot <- reactive({
      log2FC_threshold <- 1
      pvalue_threshold <- 0.05
      
      results$color <- "gray"
      results$color[results$log2FoldChange > log2FC_threshold & results$pvalue < pvalue_threshold] <- "red"
      results$color[results$log2FoldChange < -log2FC_threshold & results$pvalue < pvalue_threshold] <- "blue"
      
      ggplot(results, aes(x = log2FoldChange, y = -log10(pvalue), color = color)) +
        geom_point() +
        scale_color_identity() +
        theme_bw() +
        labs(title = "Volcano Plot", x = "log2 Fold Change", y = "-log10 p-value") +
        geom_hline(yintercept = -log10(pvalue_threshold), col = "red") +
        geom_vline(xintercept = c(-log2FC_threshold, log2FC_threshold), col = "blue")
    })
    
    output$volcano_plot <- renderPlot({
      print(volcano_plot())
    })
    
    heatmap_plot <- reactive({
      topVarGenes <- head(order(rowVars(assay(dds)), decreasing = TRUE), 20)
      mat <- assay(dds)[topVarGenes, ]
      mat <- mat - rowMeans(mat)
      pheatmap(mat, annotation_col = as.data.frame(colData(dds)[, "Condition", drop = FALSE]))
    })
    
    output$heatmap_plot <- renderPlot({
      print(heatmap_plot())
    })
    
    output$download_results <- downloadHandler(
      filename = function() {
        "deseq2_results.csv"
      },
      content = function(file) {
        write.csv(as.data.frame(results), file)
      }
    )
    
    output$download_volcano_plot <- downloadHandler(
      filename = function() {
        paste("volcano_plot.", input$download_format, sep = "")
      },
      content = function(file) {
        ggsave(file, plot = volcano_plot(), device = input$download_format)
      }
    )
    
    output$download_heatmap_plot <- downloadHandler(
      filename = function() {
        paste("heatmap_plot.", input$download_format, sep = "")
      },
      content = function(file) {
        ggsave(file, plot = heatmap_plot(), device = input$download_format)
      }
    )
  })
}
#' mfuzz UI Module
#' @description mfuzz UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @name mfuzz_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import shinythemes
#' @import DT
#' @export
#'
mfuzz_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Mfuzz Clustering',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run"), "Run")
        ),
      ),
      page_fluid(
        fluidRow(  # Use fluidRow for horizontal layout
          column(
            width = 6,  # Half of the row
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Table of Mfuzz analyse",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Download',
                  downloadButton(ns("download"), "Download Table")
                )
              ),
              mainPanel(
                shiny::dataTableOutput(ns("clusterSummary"))
              )
            )
          ),
          column(
            width = 6,  # Half of the row
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Mfuzz plot",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  numericInput(ns("clusters"), "Number of Clusters", value = 6, min = 1),
                  numericInput(ns("fuzziness"), "Fuzziness (m)", value = 1.25, min = 1, step = 0.01)
                ),
                accordion_panel(
                  title = 'Download',
                  downloadButton(ns("downloadPlot"), "Download PDF")
                )
              ),
              mainPanel(
                plotOutput(ns("mfuzzPlot"))
              )
            )
          )
        )
      )
    )
  )
}

#' Server logic for Mfuzz Clustering
#'
#' Defines the server logic for the Mfuzz clustering module in a Shiny application.
#' This includes reading the input file, preprocessing the data, running the Mfuzz clustering algorithm,
#' and providing the results for download and visualization.
#'
#' @param input A unique identifier for the module.
#' @param output description
#' @param session description
#' @import shiny
#' @import Mfuzz
#' @import tidyverse
#' @import Biobase
#' @import VIM
#' @import dplyr
#' @return A Shiny server function that handles clustering and data processing.
#' @name mfuzz_server
#' @importFrom grDevices dev.off pdf
#' @importFrom utils read.table write.table
#' @export
#'
utils::globalVariables(c("pdf", "dev.off", "."))
mfuzz_server <- function(input, output, session) {
  ns <- session$ns
  clusteringResult <- reactiveVal(NULL)
  observeEvent(input$run, {
    req(input$file)
    
    # Data pre-processing
    yeast <- read.csv(input$file$datapath, row.names = 1)
    
    yeastF_kNN <- VIM::kNN(yeast, k = 3)
    rownames(yeastF_kNN) <- rownames(yeast)
    yeastF_clean <- yeastF_kNN %>%
      dplyr::select(!contains("imp")) %>%
      as.data.frame() %>%
      dplyr::filter(rowSums(.) != 0)
    
    # 检查数据是否有效
    if (nrow(yeastF_clean) == 0 || ncol(yeastF_clean) == 0) {
      showNotification("The dataset is invalid after preprocessing.", type = "error")
      return(NULL)
    }
    
    # Convert to ExpressionSet
    yeastF_clean <- tryCatch({
      Biobase::ExpressionSet(assayData = data.matrix(yeastF_clean))
    }, error = function(e) {
      showNotification("Failed to create ExpressionSet object.", type = "error")
      return(NULL)
    })
    
    # 检查ExpressionSet对象是否创建成功
    if (is.null(yeastF_clean)) {
      return(NULL)
    }
    
    yeastF <- Mfuzz::standardise(yeastF_clean)
    
    # Fuzziness estimation and clustering
    m <- input$fuzziness
    cl <- tryCatch({
      Mfuzz::mfuzz(yeastF, c = input$clusters, m = m)
    }, error = function(e) {
      showNotification("Clustering failed.", type = "error")
      return(NULL)
    })
    
    # 保存聚类结果
    clusteringResult(list(data = yeastF, cluster = cl))
    
    # Plot clustering result
    output$mfuzzPlot <- renderPlot({
      req(cl)
      Mfuzz::mfuzz.plot(yeastF, cl, mfrow = c(2, 3), new.window = FALSE)
    })
    
    # Show cluster summary
    output$clusterSummary <- DT::renderDataTable({
      req(clusteringResult())
      # 创建数据框
      mfuzz_table <- data.frame(
        ID = names(clusteringResult()$cluster$cluster),
        cluster = clusteringResult()$cluster$cluster
      )
      # 返回交互式表格
      DT::datatable(mfuzz_table)
    })
    
    
    # Download cluster output
    output$download <- downloadHandler(
      filename = function() {
        paste("clusters_output", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        req(clusteringResult())
        write.table(clusteringResult()$cluster$cluster, file, quote = FALSE, row.names = TRUE, col.names = FALSE, sep = "\t")
      })
    
    
  })
  # Download plot as PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("mfuzz_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(clusteringResult())
      pdf(file, width = 8, height = 6)
      Mfuzz::mfuzz.plot(clusteringResult()$data, clusteringResult()$cluster, mfrow = c(2, 3), new.window = FALSE)
      dev.off()
    }
  )
  
}
#' kmeans UI Module
#' @description kmeans UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @name kmeans_ui
#' @export
#'
kmeans_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Kmeans analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Kmeans plot",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput("dropdown", "Data transformation",
                            choices = c("scale","log2", "log10", "no")),
                textOutput("selected"),
                numericInput(ns("num_input"), "Cluster number:", value = 6, min = 2),
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                actionButton(ns("run"), "Run"),
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadPlot"), "Download PDF"),
                br(),
                downloadButton(ns("downloadTable"), "Download Table")
              )
            ),
            mainPanel(
              plotOutput(ns("KmeansPlot"))  # 显示生成的图表
            )
          )
        )
      )
    )
  )
}
#' @importFrom stats kmeans
#' @importFrom dplyr mutate
#' @param data A data frame or matrix containing the data to be clustered. Rows represent observations,
#' and columns represent features.
#' @param centers An integer specifying the number of clusters to generate in the K-means analysis.
#' @name KmeansR_Ait
#' @title title
#' @export
#'
utils::globalVariables(c("Cluster"))
KmeansR_Ait <- function(data, centers) {
  data_scale <- data.frame(round(t(apply(data, 1, scale)), 2))
  colnames(data_scale) <- colnames(data)
  cl <- kmeans(data_scale, centers = centers)
  data_new <- data.frame("index" = rownames(data_scale), "Cluster" = cl$cluster, data_scale) %>%
    as_tibble() %>%
    dplyr::mutate("Cluster" = paste0("Cluster", Cluster))
  return(data_new)
}

#' @name kmeans_server
#' @title title kmeans_server
#' @param input description
#' @param output description
#' @param session description
#' @importFrom utils read.table
#' @import KmeansTrendAnalyzer
#' @export
#'
utils::globalVariables(c("Cluster"))
kmeans_server <- function(input, output, session) {
  ns <- session$ns
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read.csv(input$file$datapath,row.names = 1)  # Read the CSV file
  })
  observeEvent(input$run, {
    output$KmeansPlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      KmeansTrendAnalyzer::KmeansR(df,centers = input$num_input,table = TRUE)
    })
  })
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Kmeans_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width
      height <- input$height
      pdf(file, width = width, height = height)
      req(uploaded_data())
      df <- uploaded_data()
      KmeansTrendAnalyzer::KmeansR(df,centers = input$num_input,table = TRUE)
      dev.off()
    }
  )
  # 下载表格
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("kmeans_result", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(uploaded_data())
      df <- uploaded_data()
      result_table <- KmeansR_Ait(df, centers = input$num_input)
      write.csv(result_table, file)
    }
  )
  
  
}
#' pca UI Module
#' @description pca UI Module
#' @title title
#' @name pca_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
pca_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'PCA Analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload Matrix File", multiple = FALSE),
          fileInput(ns("group"), "Upload Group File", multiple = FALSE)
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("colorSelectors"))
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("Run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                textInput(ns("width"), "Plot Width (inches):", "8"),
                textInput(ns("height"), "Plot Height (inches)", "6"),
                radioButtons(ns("format"), "Select Download Format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                downloadButton(ns("downloadBtn2"), "Download Figure"),
                br(),
                downloadButton(ns("downloadPlot"), "Download Table")
              )
            ),
            mainPanel(
              tags$hr(style = "border-color: black; border-width: 5px;"),
              tabsetPanel(
                tabPanel("Input data of Matrix File", DT::dataTableOutput(ns("contentsFile"))),
                tabPanel("Input data of Group File", DT::dataTableOutput(ns("contentsGroup"))),
                tabPanel("Figure", plotOutput(ns("resultPlot1")))
              )
            )
          )
        )
      )
    )
  )
}

#' pca UI pca_server
#' @description pca pca_server
#' @title pca_server
#' @name pca_server
#' @param input A unique identifier for the Shiny namespace.
#' @param output A unique identifier for the Shiny namespace.
#' @param session A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @import DT
#' @import ggplot2
#' @importFrom utils write.table
#' @importFrom utils read.csv
#' @export
#'
utils::globalVariables(c("%||%", "PC1", "PC2","Sample","Group"))
pca_server <- function(input, output, session) {
  
  yanse <- c('#00f000','#0000f0','#b30000','#f0f000',
             '#00f0f0','#a000f0','#f0a000','#7e3343',
             '#00f0a0','#fb8072','#80b1d3','#fdb462',
             '#b3de69','#fccde5','#bc80bd','#ccebc5',
             '#ffed6f','#64b267','#47a7bd','#f36621',
             '#31629d','#9fde00','#ffbe2a','#ec008c','#ff7404')
  
  contentsFile <- reactive({
    req(input$file)
    inFile <- input$file
    data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE, row.names = 1)
    data
  })
  
  contentsGroup <- reactive({
    req(input$group)
    inFile <- input$group
    data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    data
  })
  
  output$contentsFile <- DT::renderDataTable({
    contentsFile()
  })
  
  output$contentsGroup <- DT::renderDataTable({
    contentsGroup()
  })
  
  output$colorSelectors <- renderUI({
    req(input$group)
    
    group_data <- contentsGroup()
    unique_groups <- unique(group_data$Group)
    
    lapply(seq_along(unique_groups), function(i) {
      colourpicker::colourInput(inputId = session$ns(paste0("color_", i)),
                                label = paste("Choose color for", unique_groups[i]),
                                value = yanse[i])
    })
  })
  
  plot_fun1 <- reactive({
    df <- contentsFile()
    df <- as.data.frame(df)
    group <- contentsGroup()
    group <- as.data.frame(group)
    
    if (nrow(df) == 0 || nrow(group) == 0) {
      return(NULL)
    }
    
    pca_data <- prcomp(t(df), scale = FALSE)
    
    df_sample <- data.frame(Sample = rownames(pca_data$x), pca_data$x)
    df_sample <- df_sample %>%
      left_join(group, by = "Sample")
    
    unique_groups <- unique(df_sample$Group)
    
    selected_colors <- sapply(seq_along(unique_groups), function(i) {
      input[[paste0("color_", i)]] %||% yanse[i]
    })
    
    ggplot(df_sample, aes(x = PC1, y = PC2, label = Sample)) +
      geom_point(aes(colour = Group), size = 3, shape = 16) +
      xlab(paste("PC1", "(", round(summary(pca_data)$importance[2, 1]*100, 1), "%)", sep = " ")) +
      ylab(paste("PC2", "(", round(summary(pca_data)$importance[2, 2]*100, 1), "%)", sep = " ")) +
      theme(legend.background = element_rect(colour = "black", size = 0.5)) +
      theme_bw() +
      scale_colour_manual(values = selected_colors) +
      scale_fill_manual(values = selected_colors)
  })
  
  observeEvent(input$Run, {
    output$resultPlot1 <- renderPlot({
      req(contentsFile(), contentsGroup())
      plot_fun1()
    })
  })
  
  output$downloadBtn2 <- downloadHandler(
    filename = function() {
      paste("PCA_Plot", input$format, sep = ".")
    },
    content = function(file) {
      ggsave(file, plot = plot_fun1(), device = input$format,
             width = as.numeric(input$width), height = as.numeric(input$height))
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("PCA_Data", "csv", sep = ".")
    },
    content = function(file) {
      df <- contentsFile()
      pca_data <- prcomp(t(df), scale = FALSE)
      write.csv(pca_data$x, file)
    }
  )
}
#' CCA UI Module
#' @description CCA UI Module
#' @param id A unique identifier for the Shiny namespace,CCA.
#' @title CCA_ui
#' @name CCA_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
CCA_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'CCA Analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Upload CSV File1", accept = ".csv"),
          fileInput(ns("file2"), "Upload CSV File2", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("CCA_Plot"))  # Display the generated plot
            )
          )
        )
      )
    )
  )
}
#' CCA_server Server Module
#' @description Server logic for CCA_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import CCA
#' @importFrom grDevices pdf dev.off
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom utils write.table read.csv
#' @export
#'
CCA_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$run, {
    req(input$file1, input$file2)  # Ensure that both files are uploaded
    
    # Read and process uploaded files
    file1 <- read.csv(input$file1$datapath, row.names = 1)
    file2 <- read.csv(input$file2$datapath, row.names = 1)
    file1 <- as.matrix(file1)  # Convert to matrix
    file2 <- as.matrix(file2)  # Convert to matrix
    
    # Perform Canonical Correlation Analysis (CCA)
    res.cc <- CCA::cc(file1, file2)
    
    # Render the CCA plot
    output$CCA_Plot <- renderPlot({
      CCA::plt.cc(res.cc, d1 = 1, d2 = 3, type = "v", var.label = TRUE)
    })
  })
  
  # Download plot as PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("CCA_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # User-defined width for PDF
      height <- input$height # User-defined height for PDF
      
      pdf(file, width = width, height = height)  # Open PDF device
      
      req(input$file1, input$file2)  # Ensure that both files are uploaded
      
      # Read and process uploaded files
      file1 <- read.csv(input$file1$datapath, row.names = 1)
      file2 <- read.csv(input$file2$datapath, row.names = 1)
      file1 <- as.matrix(file1)  # Convert to matrix
      file2 <- as.matrix(file2)  # Convert to matrix
      
      # Perform Canonical Correlation Analysis (CCA)
      res.cc <- CCA::cc(file1, file2)
      
      # Create and print the CCA plot
      CCA::plt.cc(res.cc, d1 = 1, d2 = 3, type = "v", var.label = TRUE)
      
      dev.off()  # Close PDF device
    }
  )
}
#' OPLS_DA UI Module
#' @description OPLS_DA UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title OPLS_DA_ui
#' @name OPLS_DA_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @importFrom colourpicker colourInput
#' @export
#'
OPLS_DA_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'OPLS-DA Analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("datafile"), "Upload Metabolomics Data (CSV)", accept = ".csv"),
          fileInput(ns("groupfile"), "Upload Group Data (CSV)", accept = ".csv"),
          tableOutput(ns("result_table"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(ns("group1_col"), "Group1 color", value = "#F27FB2"),
                colourpicker::colourInput(ns("group2_col"), "Group2 color", value = "#A3C8F7"),
                textInput(ns("xlable"), "X Label:", value = ""),
                textInput(ns("ylable"), "Y Label:", value = "Count")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 8, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              tags$div(
                style = "text-align: left; margin: 10px 0;",
                tags$span("ropls plot")
              ),
              tags$hr(),  # 插入另一个水平线
              plotOutput(ns("OPLS_DA_Plot")),  # 显示生成的图表
              tags$div(
                style = "text-align: left; margin: 10px 0;",
                tags$span("ggplot2 plot")
              ),
              tags$hr(),  # 插入另一个水平线
              plotOutput(ns("OPLS_DA_ggplot_Plot"))
            )
          )
        )
      )
    )
  )
}
#' OPLS_DA_server Server Module
#' @description Server logic for OPLS_DA_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import dplyr
#' @import writexl
#' @import shinyWidgets
#' @import shinyFiles
#' @import ggprism
#' @importFrom grDevices pdf dev.off
#' @importFrom dplyr select pull
#' @importFrom utils write.table read.csv
#' @name OPLS_DA_server
#' @export
#'
utils::globalVariables(c("p1", "o1"))
OPLS_DA_server <- function(input, output, session) {
  ns <- session$ns
  observeEvent(input$run, {
    req(input$datafile, input$groupfile)  # Ensure files are uploaded
    
    # Read uploaded files
    df <- read.csv(input$datafile$datapath, row.names = 1)
    group_data <- read.csv(input$groupfile$datapath)
    
    # Process data
    group <- data.frame(Sample = colnames(df)) %>%
      left_join(group_data, by = "Sample") %>%
      dplyr::select(Group) %>%
      pull(Group)
    output$OPLS_DA_Plot <- renderPlot({
      # Run OPLS-DA
      ATR_oplsda <- ropls::opls(t(df),
                                group,
                                predI = 1,
                                orthoI = 1,
                                crossvalI = 6,
                                log10L = TRUE)
    })
    output$OPLS_DA_ggplot_Plot <- renderPlot({
      ATR_oplsda <- ropls::opls(t(df),
                                group,
                                predI = 1,
                                orthoI = 1,
                                crossvalI = 6,
                                log10L = TRUE)
      OPLS_defentu <- data.frame(ATR_oplsda@scoreMN,
                                 ATR_oplsda@orthoScoreMN)
      OPLS_defentu$Group=group
      OPLS_defentu$label=rownames(OPLS_defentu)
      #设置适合科研的背景色
      theme_set(ggprism::theme_prism(border = TRUE))
      ggplot(OPLS_defentu,aes(p1,o1,label = rownames(data)))+
        geom_point(aes(colour=Group),shape=16,size = 3)+
        #theme(legend.background = element_rect(colour="black", size=0.5))+
        labs(x=paste0('T score[1] (',ATR_oplsda@modelDF$R2X[1]*100,'%)'),
             y=paste0('Orthogonal T score[1] (',ATR_oplsda@modelDF$R2X[2]*100,'%)'))+
        stat_ellipse(aes(fill=Group),
                     type = "norm", geom ="polygon",
                     alpha=0.2,color=NA,show.legend = FALSE)+
        scale_fill_manual(values = c(input$group1_col, input$group2_col))+
        scale_color_manual(values = c(input$group1_col, input$group2_col))
    })
  })
  
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("OPLS-DA_score_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # 用户输入的宽度
      height <- input$height # 用户输入的高度
      pdf(file, width = width, height = height)  # 设置 PDF 尺寸
      req(input$datafile, input$groupfile)  # Ensure files are uploaded
      
      # Read uploaded files
      df <- read.csv(input$datafile$datapath, row.names = 1)
      group_data <- read.csv(input$groupfile$datapath)
      
      # Process data
      group <- data.frame(Sample = colnames(df)) %>%
        left_join(group_data, by = "Sample") %>%
        dplyr::select(Group) %>%
        pull(Group)
      ATR_oplsda <- ropls::opls(t(df),
                                group,
                                predI = 1,
                                orthoI = 1,
                                crossvalI = 6,
                                log10L = TRUE)
      OPLS_defentu <- data.frame(ATR_oplsda@scoreMN,
                                 ATR_oplsda@orthoScoreMN)
      OPLS_defentu$Group=group
      OPLS_defentu$label=rownames(OPLS_defentu)
      #设置适合科研的背景色
      theme_set(ggprism::theme_prism(border = TRUE))
      p <- ggplot(OPLS_defentu,aes(p1,o1,label = rownames(data)))+
        geom_point(aes(colour=Group),shape=16,size = 3)+
        #theme(legend.background = element_rect(colour="black", size=0.5))+
        labs(x=paste0('T score[1] (',ATR_oplsda@modelDF$R2X[1]*100,'%)'),
             y=paste0('Orthogonal T score[1] (',ATR_oplsda@modelDF$R2X[2]*100,'%)'))+
        stat_ellipse(aes(fill=Group),
                     type = "norm", geom ="polygon",
                     alpha=0.2,color=NA,show.legend = FALSE)+
        scale_fill_manual(values = c(input$group1_col, input$group2_col))+
        scale_color_manual(values = c(input$group1_col, input$group2_col))
      print(ATR_oplsda)
      print(p)  # 输出图表到PDF文件
      dev.off()  # 关闭PDF设备
    }
  )
  
}
#' barUI Module
#' @description Mbar UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title bar_ui
#' @name bar_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
bar_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Bar Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file_upload"), "Upload CSV file",
                    accept = ".csv")
        )
      ),
      page_fluid(
        fluidRow(
          column(
            width = 6,  # Half of the row
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Table",
              mainPanel(
                # 使用 DT 渲染上传文件的内容
                DTOutput(ns("file_content"))
              )
            )
          ),
          column(
            width = 6,  # Half of the row
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Figure of result",
              layout_sidebar(
                sidebar = accordion(
                  accordion_panel(title = 'Parameter',
                                  selectInput(ns("x_var"), "Select X linkage data:",
                                              choices = NULL),
                                  selectInput(ns("y_var"), "Select Y linkage data:",
                                              choices = NULL),
                                  textInput(ns("title"), "Enter the title:",value = "tittle"),
                                  textInput(ns("x_label"), "Enter the X linkage label:",
                                            value = "xlabel"),
                                  textInput(ns("y_label"), "Enter the Y linkage label:",
                                            value = "ylabel"),
                                  textInput(ns("bar_width"), "Enter the bar width:",
                                            value = 0.5),
                                  # 自定义标题、标签字体大小和颜色
                                  numericInput(ns("title_size"), "Title font size:",
                                               value = 20, min = 1, step = 1),
                                  colourpicker::colourInput(ns("title_color"), "Title color:",
                                                            value = "black"),
                                  numericInput(ns("label_size"), "Linkage label font size:",
                                               value = 20, min = 1, step = 1),
                                  colourpicker::colourInput(ns("label_color"), "Linkage label color:",
                                                            value = "black"),
                                  # 柱子的颜色
                                  colourpicker::colourInput(ns("bar_color"), "Bar color",
                                                            value = "#03A9F4")
                  ),
                  accordion_panel(
                    title = "Run",
                    actionButton(ns("run_button"), "Run")
                  ),
                  accordion_panel(
                    title = "Download",
                    numericInput(ns("width"), "Image width (px):",
                                 value = 800, min = 100, step = 10),
                    numericInput(ns("height"), "Image height (px):",
                                 value = 600, min = 100, step = 10),
                    selectInput(ns("format"), "Select download format:",
                                choices = c("pdf", "png", "jpg", "svg","eps",
                                            "ps", "tex","jpeg","bmp","wmf")),
                    downloadButton(ns("download_plot"), "Download")
                  )
                ),
                mainPanel(
                  # 显示图形
                  plotOutput(ns("plot_output"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Bar_server Server Module
#' @description Server logic for merging files Bar_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import ggplot2
#' @import shinyWidgets
#' @import shinyFiles
#' @import DT
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom utils write.table
#' @export
#'
Bar_server <- function(input, output, session) {
  # Reactive value to store the uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # 监听文件上传事件
  observeEvent(input$file_upload, {
    req(input$file_upload)  # 确保文件上传控件有文件输入
    # 读取上传的 CSV 文件
    data <- read.csv(input$file_upload$datapath)
    # 将数据存储在 reactive value 中
    uploaded_data(data)
    
    # 更新下拉菜单的选择项
    updateSelectInput(session, "x_var", choices = names(data))
    updateSelectInput(session, "y_var", choices = names(data))
  })
  
  # 使用 DT 渲染上传文件的内容
  output$file_content <- renderDT({
    req(uploaded_data())  # 确保有数据可用
    datatable(uploaded_data())  # 使用 DT::datatable 显示数据
  })
  
  # 监听 Run 按钮点击事件
  observeEvent(input$run_button, {
    # 确保有数据可用
    req(uploaded_data())
    
    # 获取上传的数据
    data <- uploaded_data()
    
    # 生成图形
    new_plot <- ggplot(data, aes(
      x = !!sym(input$x_var),
      y = !!sym(input$y_var)
    )) +
      geom_bar(stat = "identity", width = as.numeric(input$bar_width), fill = input$bar_color) +
      labs(
        title = input$title,
        x = input$x_label,
        y = input$y_label
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = input$title_size, color = input$title_color),
        axis.title = element_text(size = input$label_size, color = input$label_color),
        axis.text = element_text(size = input$label_size, color = input$label_color)
      )
    
    # 更新输出图形
    output$plot_output <- renderPlot({
      new_plot
    })
    
    # 处理下载图像的逻辑
    output$download_plot <- downloadHandler(
      filename = function() {
        paste(ifelse(input$title != "", input$title, "plot"), Sys.Date(), ".", input$format, sep = "")
      },
      content = function(file) {
        # 保存图像文件
        ggsave(file, plot = new_plot, width = input$width / 100, height = input$height / 100, units = "in", device = input$format)
      }
    )
  })
}
#' Venn UI Module
#' @description Venn UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title Venn_ui
#' @name Venn_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
Venn_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Venn Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV file", accept = c(".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            layout_sidebar(
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  uiOutput(ns("col_ui")),
                  colourpicker::colourInput(ns("color1"), "Select Color 1", value = "red"),
                  colourpicker::colourInput(ns("color2"), "Select Color 2", value = "blue"),
                  colourpicker::colourInput(ns("color3"), "Select Color 3", value = "green"),
                  colourpicker::colourInput(ns("color4"), "Select Color 4", value = "yellow"),
                  colourpicker::colourInput(ns("color5"), "Select Color 5", value = "purple")
                ),
                accordion_panel(
                  title = 'Run',
                  actionButton(ns("plot_venn"), "Run"),
                ),
                accordion_panel(
                  title = 'Download',
                  numericInput(ns("width"), "Width (inches)", value = 7),
                  numericInput(ns("height"), "Height (inches)", value = 7),
                  selectInput(ns("format"), "Select Format", choices = c("pdf", "png", "jpg", "svg", "eps", "ps", "tex", "jpeg", "bmp")),
                  downloadButton(ns("downloadVenn"), "Download")
                )
              ),
              mainPanel(
                plotOutput(ns("venn_plot"))
              )
            )
          )
        )
      )
    )
  )
}
#' Venn_server Server Module
#' @description Server logic for Venn_server
#' @name Venn_server
#' @title Venn_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @importFrom grid grid.draw
#' @import VennDiagram
#' @import tikzDevice
#' @importFrom grDevices bmp jpeg png postscript svg
#' @export
#'
Venn_server <- function(input, output, session) {
  ns <- session$ns
  
  dataset <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  output$col_ui <- renderUI({
    req(dataset())
    cols <- colnames(dataset())
    checkboxGroupInput(ns("selected_cols"), "Select columns for Venn Diagram (up to 5)", choices = cols, selected = cols[1:min(5, length(cols))])
  })
  
  output$venn_plot <- renderPlot({
    req(dataset(), input$selected_cols)
    validate(
      need(length(input$selected_cols) <= 5, "Please select up to 5 columns")
    )
    
    data <- dataset()[, input$selected_cols, drop = FALSE]
    
    sets <- lapply(data, function(col) unique(col))
    names(sets) <- colnames(data)
    
    colors <- c(input$color1, input$color2, input$color3, input$color4, input$color5)[1:length(sets)]
    
    venn.plot <- venn.diagram(
      x = sets,
      category.names = colnames(data),
      fill = colors,
      filename = NULL, # 设置为NULL，避免保存文件
      output = TRUE
    )
    grid.draw(venn.plot)
  })
  
  output$downloadVenn <- downloadHandler(
    filename = function() {
      paste("venn_diagram.", input$format, sep = "")
    },
    content = function(file) {
      device <- switch(input$format,
                       pdf = pdf,
                       png = png,
                       jpg = jpeg,
                       svg = svg,
                       eps = postscript,
                       ps = postscript,
                       tex = function(...) tikzDevice::tikz(...),
                       jpeg = jpeg,
                       bmp = bmp
      )
      
      device(file, width = input$width, height = input$height)
      data <- dataset()[, input$selected_cols, drop = FALSE]
      
      sets <- lapply(data, function(col) unique(col))
      names(sets) <- colnames(data)
      
      colors <- c(input$color1, input$color2, input$color3, input$color4, input$color5)[1:length(sets)]
      
      venn.plot <- venn.diagram(
        x = sets,
        category.names = colnames(data),
        fill = colors,
        filename = NULL, # 设置为NULL，避免保存文件
        output = TRUE
      )
      grid.draw(venn.plot)
      dev.off()
    }
  )
  
}
#' enrichment_bubble UI Module
#' @description enrichment_bubble UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title enrichment_bubble
#' @name enrichment_bubble_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
enrichment_bubble_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Enrichment Bubble',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                numericInput(ns("num_entries"), "Number of Entries per Group", 10, min = 1),
                checkboxInput(ns("header"), "Header", TRUE),
                textInput(ns("plot_title"), "Plot Title", "Enrichment Bubble Plot"),
                textInput(ns("x_label"), "X-axis Label", ""),
                textInput(ns("y_label"), "Y-axis Label", ""),
                numericInput(ns("num_colors"), "Number of Colors", 2, min = 2),
                uiOutput(ns("color_inputs"))
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("plot_width"), "Plot Width (inches)", 10, min = 1),
                numericInput(ns("plot_height"), "Plot Height (inches)", 8, min = 1),
                radioButtons(ns("file_type"), "File Type", choices = c("PNG" = "png", "SVG" = "svg", "PDF" = "pdf"), selected = "png"),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("bubblePlot"))
            )
          )
        )
      )
    )
  )
}
#' @description enrichment_bubble_server
#' @title title enrichment_bubble_server
#' @param id enrichment_bubble_server id
#' @param input description
#' @param output description
#' @param session description
#' @importFrom ggplot2 ggplot geom_point scale_color_gradientn theme_bw labs theme
#' @importFrom shiny NS moduleServer
#' @importFrom colourpicker colourInput
#' @importFrom dplyr group_by arrange slice group_by
#' @importFrom readr read_csv
#' @importFrom ggplot2 ggsave
#' @name enrichment_bubble_server
#' @export
#'
utils::globalVariables(c("Pvalue", "Description", "Count_all"))
enrichment_bubble_server <- function(input, output, session) {
  ns <- session$ns
  # 动态生成颜色选择输入框
  output$color_inputs <- renderUI({
    num_colors <- input$num_colors
    color_inputs <- lapply(1:num_colors, function(i) {
      colourpicker::colourInput(ns(paste0("color", i)), paste0("Color ", i), value = ifelse(i == 1, "blue", "red"))
    })
    do.call(tagList, color_inputs)
  })
  
  output$bubblePlot <- renderPlot({
    req(input$file1)
    
    data <- read_csv(input$file1$datapath, col_names = input$header)
    
    plot_data <- data %>%
      group_by(Group) %>%
      arrange(Pvalue) %>%
      slice_head(n = as.numeric(input$num_entries)) %>%
      ungroup()
    
    colors <- sapply(1:input$num_colors, function(i) input[[paste0("color", i)]])
    color_scale <- scale_color_gradientn(colors = colors)
    
    ggplot(plot_data, aes(x = Group, y = Description)) +
      geom_point(aes(size = Count_all, color = Pvalue)) +
      color_scale +
      theme_bw() +
      labs(
        title = input$plot_title,
        x = input$x_label,
        y = input$y_label,
        size = "Count",
        color = "P-value"
      ) +
      theme(axis.text.y = element_text(size = 8))
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Enrichment_Bubble_Plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$file_type, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = input$file_type, width = input$plot_width, height = input$plot_height)
    }
  )
  
}
#' VolcanoPlot UI Module
#' @description VolcanoPlot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title VolcanoPlot_ui
#' @name VolcanoPlot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @import DT
#' @export
#'
VolcanoPlot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Volcano Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file_upload"), "Upload CSV file:",
                    accept = c(".csv"))
        )
      ),page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput(ns("pvalue_col"), "Select P-value column:", choices = NULL),
                selectInput(ns("log2fc_col"), "Select log2 Fold Change column:", choices = NULL),
                selectInput(ns("vip_col"), "Select VIP column:", choices = NULL),  # Select VIP column
                # Threshold inputs
                numericInput(ns("pvalue_threshold"), "P-value threshold:", value = 0.05),
                colourpicker::colourInput(ns("pvalue_line_color"), "P-value Line Color:", value = "black"),  # P-value line color (default black)
                numericInput(ns("log2fc_threshold"), "log2 Fold Change threshold:", value = 1),
                colourpicker::colourInput(ns("log2fc_line_color"), "log2 Fold Change Line Color:", value = "black"),  # log2 Fold Change line color (default black)
                
                # Point color inputs for upregulated and downregulated points
                colourpicker::colourInput(ns("upregulated_color"), "Upregulated Points Color:", value = "red"),  # Upregulated points color
                colourpicker::colourInput(ns("downregulated_color"), "Downregulated Points Color:", value = "blue"),  # Downregulated points color
                colourpicker::colourInput(ns("not_significant_color"), "Not Significant Points Color:", value = "gray"),  # Not significant points color
                
                # Axis range toggle and inputs
                checkboxInput(ns("use_x_range"), "Set X-axis range", value = FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("use_x_range"), "']"),
                  numericInput(ns("x_min"), "X-axis minimum:", value = -3),
                  numericInput(ns("x_max"), "X-axis maximum:", value = 3)
                ),
                checkboxInput(ns("use_y_range"), "Set Y-axis range", value = FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("use_y_range"), "']"),
                  numericInput(ns("y_min"), "Y-axis minimum:", value = 0),
                  numericInput(ns("y_max"), "Y-axis maximum:", value = 10)
                )
              ),
              accordion_panel(
                title = "Run",
                actionButton(ns("run_btn"), "Run")
              ),
              accordion_panel(
                title = "Download",
                radioButtons(ns("file_format"), "Choose file format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
                             selected = "png"),
                # Download button
                downloadButton(ns("download_plot"), "Download")
              )
            ),
            mainPanel(
              uiOutput(ns("output_ui")
              )
            )
          )
        )
      )
    )
  )
}
#' VolcanoPlot_server Module
#' @description Server logic for VolcanoPlot_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import ggplot2
#' @name VolcanoPlot_server
#' @importFrom utils write.table data
#' @export
#'
utils::globalVariables(c("data"))
VolcanoPlot_server <- function(input, output, session, data) {
  ns <- session$ns
  # Reactive value to store the data
  data1 <- reactive({
    req(input$file_upload)
    read.csv(input$file_upload$datapath)
  })
  
  observeEvent(input$file_upload, {
    req(data1())
    # Update select input choices based on data columns
    updateSelectInput(session, "pvalue_col", choices = names(data1()))
    updateSelectInput(session, "log2fc_col", choices = names(data1()))
    updateSelectInput(session, "vip_col", choices = names(data1()))
  })
  
  # Generate the volcano plot based on user inputs
  plot_reactive <- reactive({
    req(input$pvalue_col, input$log2fc_col, input$vip_col)
    
    # Data cleaning
    data_clean <- data1()
    data_clean[[input$pvalue_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$pvalue_col]])))
    data_clean[[input$log2fc_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$log2fc_col]])))
    data_clean[[input$vip_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$vip_col]])))
    
    # Check if any selected columns contain NA after conversion
    if (any(is.na(data_clean[[input$pvalue_col]]))) {
      stop("Error: P-value column contains non-numeric values that could not be converted.")
    }
    if (any(is.na(data_clean[[input$log2fc_col]]))) {
      stop("Error: log2 Fold Change column contains non-numeric values that could not be converted.")
    }
    if (any(is.na(data_clean[[input$vip_col]]))) {
      stop("Error: VIP column contains non-numeric values that could not be converted.")
    }
    
    # Categorize points based on thresholds
    data_clean$category <- ifelse(data_clean[[input$log2fc_col]] > input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Up",
                                  ifelse(data_clean[[input$log2fc_col]] < -input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Down", "Not Significant"))
    
    # Start building the plot
    plot <- ggplot(data_clean, aes_string(x = input$log2fc_col, y = paste0("-log10(", input$pvalue_col, ")"), size = input$vip_col, color = "category")) +
      geom_point() +
      scale_color_manual(values = c("Up" = input$upregulated_color, "Down" = input$downregulated_color, "Not Significant" = input$not_significant_color)) +
      geom_hline(yintercept = -log10(input$pvalue_threshold), linetype = "dashed", color = input$pvalue_line_color) +  # P-value threshold line
      geom_vline(xintercept = c(-input$log2fc_threshold, input$log2fc_threshold), linetype = "dashed", color = input$log2fc_line_color) +  # log2 Fold Change threshold lines
      theme_bw() +
      labs(x = "log2 Fold Change", y = "-log10(P-value)",
           title = "Volcano Plot", color = "Category", size = "VIP")
    
    # Apply X-axis range if toggle is enabled
    if (input$use_x_range) {
      plot <- plot + xlim(input$x_min, input$x_max)
    }
    
    # Apply Y-axis range if toggle is enabled
    if (input$use_y_range) {
      plot <- plot + ylim(input$y_min, input$y_max)
    }
    
    plot
  })
  
  # Render the volcano plot
  output$output_ui <- renderUI({
    req(input$run_btn)
    renderPlot({ plot_reactive() })
  })
  # Download handler for the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("volcano_plot", Sys.Date(), ".", input$file_format, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), device = input$file_format, width = 8, height = 6, units = "in")
    }
  )
}
#' histogram UI Module
#' @description histogram UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title histogram_ui
#' @name histogram_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
histogram_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Histogram',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("selectUI")),
                numericInput(ns("bins"), "Number of bins:", 30, min = 1),
                colourpicker::colourInput(ns("lineColor"), "Select Line Color", value = "red"),
                colourpicker::colourInput(ns("fillColor"), "Select Bar Fill Color", value = "lightblue"),
                numericInput(ns("plotWidth"), "Plot Width (inches):", 7, min = 1),
                numericInput(ns("plotHeight"), "Plot Height (inches):", 5, min = 1)
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("generate"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                radioButtons(ns("format"), "Select Download Format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("histPlot")),
              textOutput(ns("errorMsg"))
            )
          )
        )
      )
    )
  )
  
}

#' histogram UI Module
#' @description histogram_server Module
#' @name histogram_server
#' @title histogram_server
#' @param input description
#' @param output description
#' @param session description
#' @importFrom utils read.csv
#' @import ggplot2
#' @export
#'
utils::globalVariables(c("x", "..density.."))

histogram_server <- function(input, output, session) {
  ns <- session$ns
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$selectUI <- renderUI({
    req(data())
    selectInput(session$ns("column"), "Select Column for Analysis", choices = names(data()))
  })
  
  # Reactive expression for generating the plot based on user input
  plot_reactive <- reactive({
    req(data(), input$column, input$generate > 0)
    selected_data <- data()[[input$column]]
    valid_data <- na.omit(as.numeric(selected_data))
    
    if (length(valid_data) > 0) {
      ggplot(data = data.frame(x = valid_data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = input$bins, color = "black", fill = input$fillColor) +
        stat_function(fun = dnorm, args = list(mean = mean(valid_data, na.rm = TRUE), sd = stats::sd(valid_data, na.rm = TRUE)),
                      color = input$lineColor, size = 1) +
        labs(title = "Histogram with Normal Distribution Curve",
             x = "Value", y = "Density") +
        theme_bw()
    } else {
      NULL
    }
  })
  
  output$histPlot <- renderPlot({
    plot_reactive()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("histogram-", Sys.Date(), ".", input$format, sep="")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), device = input$format,
             width = input$plotWidth, height = input$plotHeight, units = "in")
    }
  )
}
#' upset_ui  Module
#' @description upset_ui Module
#' @param id A unique identifier for the Shiny namespace.
#' @title upset_ui
#' @name upset_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
upset_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'UpSetR Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
          helpText("The uploaded CSV file should have at least two columns.")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Download Plot',
                numericInput(ns("width"), "Width (in inches)", value = 12, min = 1),
                numericInput(ns("height"), "Height (in inches)", value = 6, min = 1),
                downloadButton(ns("downloadPlot"), "Download")
              ),
              accordion_panel(
                title = 'Download Data',
                downloadButton(ns("downloadData"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("plot"))
            )
          )
        )
      )
    )
  )
}
#' upset_server Module
#' @description upset_server Module
#' @name upset_server
#' @title upset_server
#' @param input description
#' @param output description
#' @param session description
#' @importFrom utils read.csv write.csv
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @importFrom UpSetR upset
#' @import ggplot2
#' @export
#'
utils::globalVariables(c("Name", "Set"))
upset_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    shiny::validate(need(ncol(df) >= 2, "The CSV file must have at least two columns"))
    df
  })
  
  # Reactive expression to process the data for UpSetR
  upset_data <- reactive({
    df <- dataInput()
    long_df <- df %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Set", values_to = "Name")
    
    long_df %>%
      dplyr::distinct(Name, Set) %>%
      tidyr::pivot_wider(names_from = Set, values_from = Set, values_fill = list(Set = "0")) %>%
      dplyr::mutate(across(-Name, ~ifelse(. == "0", 0, 1))) %>%
      as.data.frame()
  })
  
  # Reactive expression to generate the UpSetR plot
  plot_obj <- reactive({
    upset_data <- upset_data()
    UpSetR::upset(upset_data, order.by = "freq")
  })
  
  # Render the UpSetR plot
  output$plot <- renderPlot({
    plot_obj()
  })
  
  # Download handler for the UpSetR plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("upset_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = input$width, height = input$height)
      print(plot_obj())
      dev.off()
    }
  )
  
  # Download handler for the processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("upset_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(upset_data(), file, row.names = FALSE)
    }
  )
}
#' GO_bar_class UI Module
#' @description GO_bar_class UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title GO_bar_class_ui
#' @name GO_bar_class_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @export
#'
GO_bar_class_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'GO Bar Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(ns("bp"), "Biological process color", value = "#F27FB2"),
                colourpicker::colourInput(ns("cc"), "Cellular component color", value = "#A3C8F7"),
                colourpicker::colourInput(ns("mf"), "Molecular function color", value = "#ED8000"),
                textInput(ns("xlable"), "X Label:", value = ""),
                textInput(ns("ylable"), "Y Label:", value = "Count"),
                textInput(ns("group"), "Group Label:", value = "Group")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("goBarPlot"))  # 显示生成的图表
            )
          )
        )
      )
    )
  )
}

#' GO_bar_class_server Module
#' @description GO_bar_class_server Module
#' @param id A unique identifier for the Shiny namespace.
#' @param input description
#' @param output description
#' @param session description
#' @title GO_bar_class_server
#' @name GO_bar_class_server
#' @import ggplot2
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @importFrom utils read.table
#' @export
#'
utils::globalVariables(c("Count"))
GO_bar_class_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded file
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read.csv(input$file$datapath)  # Read the CSV file
  })
  
  # 监听"Run"按钮的点击事件并生成图表
  observeEvent(input$run, {
    output$goBarPlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      
      ggplot(df, aes(x = Description, y = Count, fill = Ontology)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = input$xlable, y = input$ylable) +
        scale_x_discrete(limits = unique(df$Description)) +
        scale_fill_manual(values = c(input$bp, input$cc, input$mf))
    })
  })
  
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("GO_Bar_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # 用户输入的宽度
      height <- input$height # 用户输入的高度
      pdf(file, width = width, height = height)  # 设置 PDF 尺寸
      req(uploaded_data())  # 确保数据存在
      df <- uploaded_data()
      
      # 绘制与UI一致的图表并保存为PDF
      p <- ggplot(df, aes(x = Description, y = Count, fill = Ontology)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = input$xlable, y = input$ylable) +
        scale_x_discrete(limits = unique(df$Description)) +
        scale_fill_manual(values = c(input$bp, input$cc, input$mf))
      
      print(p)  # 输出图表到PDF文件
      dev.off()  # 关闭PDF设备
    }
  )
}
#' ridgePlot UI Module
#' @description ridgePlot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title ridgePlot_ui
#' @name ridgePlot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
ridgePlot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Ridge Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("xcol_ui")),  # 动态生成X轴选择
                uiOutput(ns("ycol_ui")),  # 动态生成Y轴选择
                uiOutput(ns("fillcol_ui")),  # 动态生成填充选择
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("RidgePlot"))  # 显示生成的图表
            )
          )
        )
      )
    )
  )
}

#' @import ggridges
#' @param input description
#' @param output description
#' @param session description
#' @name ridgePlot_server
#' @title ridgePlot_server
#' @importFrom utils read.table
#' @import ggplot2
#' @export
#'
ridgePlot_server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    df <- read.csv(input$file$datapath)  # Read the CSV file
    return(df)
  })
  
  output$xcol_ui <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("xcol"), "Select X Column:", choices = names(uploaded_data()))
  })
  
  output$ycol_ui <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("ycol"), "Select Y Column:", choices = names(uploaded_data()))
  })
  
  output$fillcol_ui <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("fillcol"), "Select Fill Column:", choices = names(uploaded_data()))
  })
  
  observeEvent(input$run, {
    output$RidgePlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      x_var <- input$xcol
      y_var <- input$ycol
      fill_var <- input$fillcol
      
      ggplot(data = df, aes_string(x = x_var, y = y_var, fill = fill_var)) +
        geom_density_ridges(scale = 1.6, alpha = 0.3) +
        theme_classic()
    })
  })
  
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Ridge_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(uploaded_data())  # 确保数据存在
      df <- uploaded_data()
      x_var <- input$xcol
      y_var <- input$ycol
      fill_var <- input$fillcol
      
      pdf(file, width = input$width, height = input$height)  # 设置 PDF 尺寸
      print(
        ggplot(data = df, aes_string(x = x_var, y = y_var, fill = fill_var)) +
          geom_density_ridges(scale = 1.6, alpha = 0.3) +
          theme_classic()
      )
      dev.off()  # 关闭PDF设备
    }
  )
}
#' boxplot UI Module
#' @description boxplot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title boxplot_ui
#' @name boxplot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @export
#'
boxplot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Boxplot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("xcol_ui")),  # X-axis column selection
                uiOutput(ns("ycol_ui")),  # Y-axis column selection
                colourpicker::colourInput(ns("box_color"), "Box color:", value = "black"),
                colourpicker::colourInput(ns("box_fill"), "Box fill color:", value = "red"),
                textInput(ns("x_label"), "Enter the X-axis label:", value = "Group"),
                textInput(ns("y_label"), "Enter the Y-axis label:", value = "Value")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 6, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("boxPlot"))  # Correct plotOutput id
            )
          )
        )
      )
    )
  )
}

#' boxplot_server Module
#' @description Server logic for boxplot_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import ggplot2
#' @import shiny
#' @importFrom utils read.csv
#' @export
#'
boxplot_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Dynamically generate UI for X and Y column selection based on uploaded data
  output$xcol_ui <- renderUI({
    req(data())
    selectInput(ns("xcol"), "Select X-axis (Grouping Column):", choices = names(data()))
  })
  
  output$ycol_ui <- renderUI({
    req(data())
    selectInput(ns("ycol"), "Select Y-axis (Numeric Column):", choices = names(data()))
  })
  
  # Create the boxplot when the action button is pressed
  observeEvent(input$run, {
    output$boxPlot <- renderPlot({
      req(input$xcol, input$ycol)
      plot_data <- data()
      
      ggplot(plot_data, aes_string(x = input$xcol, y = input$ycol)) +
        geom_boxplot(color = input$box_color, fill = input$box_fill) +
        labs(x = input$x_label, y = input$y_label) +
        theme_bw()
    })
  })
  
  # Allow user to download the plot as a PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("boxplot-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = input$width, height = input$height)
    }
  )
}
#' pie_ UI Module
#' @description pie_ UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title pie_ui
#' @name pie_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
pie_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Pie Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("colorSelectors")),
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("generate"), "Run"),
              ),
              accordion_panel(
                title = 'Download Table',
                downloadButton(ns("downloadData"), "Download")
              ),
              accordion_panel(
                title = 'Download Plot',
                numericInput(ns("width"), "Download Width (inches):", value = 8, min = 1),
                numericInput(ns("height"), "Download Height (inches):", value = 6, min = 1),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              # 创建一个新的面板，包含DT表格和绘图
              tabsetPanel(
                tabPanel(
                  title = "Plot",
                  plotOutput(ns("pieChart"))
                ),
                tabPanel(
                  title = "Data Table",
                  DTOutput(ns("table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' pie_server Module
#' @description Server logic for pie
#' @name pie_server
#' @title pie_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import dplyr
#' @import DT
#' @import colourpicker
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom writexl write_xlsx
#' @importFrom utils write.table
#' @export
#'
utils::globalVariables(c("Classification", "colors", "Percentage"))
pie_server <- function(input, output, session) {
  ns <- session$ns
  classification_counts <- reactiveVal()
  plot_obj <- reactiveVal()  # 保存生成的plot对象
  
  observeEvent(input$generate, {
    req(input$file)
    
    data <- read.csv(input$file$datapath)
    
    # 检查是否存在“Classification”列
    req("Classification" %in% names(data))
    
    class_counts <- data %>%
      group_by(Classification) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    classification_counts(class_counts)
    
    output$colorSelectors <- renderUI({
      lapply(1:nrow(class_counts), function(i) {
        colourpicker::colourInput(ns(paste("color", i, sep = "_")),
                                  paste("Select Color for", class_counts$Classification[i]),
                                  value = sample(colors(), 1))
      })
    })
    
    observe({
      req(input$file)
      
      color_values <- sapply(1:nrow(class_counts), function(i) {
        input[[paste("color", i, sep = "_")]]
      })
      
      plot <- ggplot(class_counts, aes(x = "", y = Count, fill = Classification)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        theme_void() +
        theme(legend.title = element_blank()) +
        geom_text(aes(label = paste(Count, "(", round(Percentage, 1), "%)", sep = "")),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = color_values)  # 使用动态生成的颜色
      
      plot_obj(plot)  # 保存plot对象以便下载
      
      output$pieChart <- renderPlot({
        print(plot)  # 显示生成的饼图
      })
    })
    
    output$table <- renderDT({
      datatable(class_counts)
    })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("pie_chart_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      plot <- plot_obj()  # 获取生成的plot对象
      
      # 使用ggsave保存为PDF
      ggsave(file, plot = plot, device = "pdf", width = input$width, height = input$height)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("classification_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(classification_counts(), file, row.names = FALSE)
    }
  )
}
options(shiny.maxRequestSize = 200 * 1024 * 1024)  # 最大上传文件大小为10MB
#' sequence_extract UI Module
#' @description sequence_extract UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title sequence_extract_ui
#' @name sequence_extract_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
sequence_extract_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Sequence Extract',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("id_file"), "Upload ID File (CSV)"),
          fileInput(ns("fasta_file"), "Upload FASTA File")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                numericInput(ns("nbchar"), "Sequence Length per Line (bp)", value = 60, min = 1)
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run_btn"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                downloadButton(ns("download_fasta"),"Download", disabled = TRUE)
              )
            ),
            mainPanel(
              verbatimTextOutput(ns("status"))
            ),
            tags$script(
              'Shiny.addCustomMessageHandler("enableButton", function(message) {
         $("#" + message.id).prop("disabled", false);
      });'
            )
          )
        )
      )
    )
  )
}
#' sequence_extract_server Module
#' @description Server logic for sequence_extract
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import seqinr
#' @importFrom readr read_csv
#' @title sequence_extract_server
#' @name sequence_extract_server
#' @export
#'
sequence_extract_server <- function(input, output, session) {
  ns <- session$ns
  temp_file <- NULL
  
  observeEvent(input$run_btn, {
    req(input$id_file, input$fasta_file)
    
    # Read ID file
    id_data <- read_csv(input$id_file$datapath)
    ids <- id_data[[1]]
    
    # Read FASTA file
    fasta_data <- read.fasta(input$fasta_file$datapath, forceDNAtolower = FALSE)
    
    # Extract matching sequences
    matched_ids <- ids[ids %in% names(fasta_data)]
    select_list <- lapply(matched_ids, function(id) grep(id, names(fasta_data), ignore.case = TRUE))
    select_transcript <- fasta_data[unlist(select_list)]
    
    # Write to temporary FASTA file
    temp_file <<- tempfile(fileext = ".fa")
    write.fasta(sequences = select_transcript,
                names = names(select_transcript),
                nbchar = input$nbchar,
                file.out = temp_file)
    
    # Update status information
    output$status <- renderText("Sequence extraction completed!")
    
    # Enable download button
    session$sendCustomMessage(type = 'enableButton', message = list(id = 'download_fasta'))
  })
  
  output$download_fasta <- downloadHandler(
    filename = function() {
      paste("extracted_sequences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".fa", sep = "")
    },
    content = function(file) {
      file.copy(temp_file, file)
    },
    contentType = "application/octet-stream"
  )
}
#' reverse_complement UI Module
#' @description reverse_complement UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title reverse_complement_ui
#' @name reverse_complement_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
reverse_complement_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Reverse Complement',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Parameter",
          radioButtons(ns("complement_type"), "Choose complement type:",
                       choices = list("Forward Complement" = "forward", "Reverse Complement" = "reverse"),
                       selected = "reverse")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_btn"), "Run"),
        ),
        accordion_panel(
          title = "Download",
          downloadButton(ns("download_fasta"), "Download")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Result",
            textAreaInput(ns("input_seq"), "Enter DNA sequence",
                          rows = 10, width  = "100%",
                          placeholder = "Enter DNA sequence...",
                          value = "ATTTATCCTTCAGCCCCTCTAGGCTTAGCAGTCAGCTTGACTTGAGGAGGAAAAAAAAACAACACACCACACTGCATTAATAATGGGGCGATCACCATGCTGTGAGAAGATTGGACTGAAGAAAGGTCCATGGACGCCGGAGGAGGATGAGAAGCTGCTTGCCTTCGTTGAGGAACATGGACACGGGAGCTGGCGGGCATTACCTGCGAAGGCAGGCTTGCAGAGGTGTGGGAAGAGCTGCAGGTTGAGGTGGACAAACTACCTGAGGCCGGACATCAAGAGGGGCAAGTTCAGCTTGCAAGAAGAACAGACCATCATCCAGCTTCACGCTCTTTTAGGCAACAGGTGGTCGGCCATTGCAACACATCTACCAAATCGCACGGACAACGAGATCAAGAACCACTGGAACACGCACCTCAAGAAAAGGCTGGCCAAGATTGGGATCGATCCTGTCACCCACAAATCTACCTGTGGCACTCTCACTGGCACCACGAACGACAGATCAGCCAAGGCCGCGGCAAGCCTCAGCCACATGGCACAATGGGAGAATGCCCGCCTCGAGGCTGAGGCACGGCTGGCTCGAGAATCGAAGACACGAACAGCAACACCGACGCCATCTGCACTCCATGCGCAGCCAATGGATCTACCTGCCTCTGCTGCTTCTCCATGGCTTGACGTGTTGCATGCTTGGCAGGGTGCAAAGATAGACCTGGAGTCACCTACCTCCACACTGACGTTTACAGGGAGCAATGGTGGCATGCTGCCAACCCCCAGGACCAACGGACCAGAGGTATCAGAAAGCAACTCCGCGATGTCGCATTATCAGATGAGCGATGAGTTGGAGGGTGAAGAAACCTATTGGCAGATCTTCAGCAAGCACCAAGTGCCGGAAGTGGACAGCAAGGAGAGTGAAGATGACTTCATTGGCTGTGAGGAGCCGTGGTTCTCAGGGATGGCTGGGGTTGGAGCTGGCATGCTGCTTGATGTATCCAATGAGCATGAGCTATCAGAATGCTGGGGTGAGTCCAGCAGTGGCCAAACTGTTGAGCACAGCAAGCAAGCATCCGATAAGGAGGACAAGGATTATTGGAATTGGGTCCTTGACAGAGTAAACTCAGAGCTGACAGCACAGTCGCCTTCCTTGGTCTAAAATACAGCCCCCCCCCCCCCCCCCCCCCCCCCTGCAGTTTTTTTTTCAACGACTCCTTAGCAAAATCTATTGATCATCTTAAGTCAAAAGAGAAACAGAACATACGAGAAGTACAAATAAATTCTACAGAAGGATGTTTTTTGTCCATGCATGTGAGTGAAATAATTAGCTGTTGTGTGTATGTAGTAAATATTTTTTTCCCTGATTTCTGAATGTATGTTAAGCCTCCAGGACAACAGCAGGGACACTACATTAATGGTCCAAGGACTTTACAGTTGTTAGTTCAACGGCCAAGTTGTCTTGTCTCCCTCTCTTTTTGAATCCATCCCAGAATGAGTAAGAA"),
            mainPanel(
              tags$div(
                style = "width: 100%;",
                verbatimTextOutput(ns("output_seq"))
              )
            )
          )
        )
      )
    )
  )
}
#' reverse_complement_server Module
#' @description Server logic for reverse_complement
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import Biostrings
#' @importFrom readr read_csv
#' @title reverse_complement_server
#' @name reverse_complement_server
#' @export
#'
reverse_complement_server <- function(input, output, session) {
  complement_seq <- eventReactive(input$run_btn, {
    req(input$input_seq)
    
    # Remove whitespace characters
    cleaned_input_seq <- gsub("\\s+", "", input$input_seq)
    input_seq <- DNAString(cleaned_input_seq)
    
    if (input$complement_type == "reverse") {
      reverseComplement(input_seq)
    } else {
      complement(input_seq)
    }
  })
  
  output$output_seq <- renderText({
    as.character(complement_seq())
  })
  
  output$download_fasta <- downloadHandler(
    filename = function() {
      paste("complement_sequence_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".fa", sep = "")
    },
    content = function(file) {
      seq_set <- DNAStringSet(complement_seq())
      writeXStringSet(seq_set, filepath = file, format = "fasta")
    }
  )
}
#' seqLogo UI Module
#' @description seqLogo UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @param title The title of the sequence logo UI, default is "Sequence Logo".
#' @title seqLogo ui Module
#' @name seqLogo_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
seqLogo_ui <- function(id, title = "Sequence Logo") {
  ns <- NS(id)
  nav_panel(
    title = 'Sequence Logo',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose DNA Sequence File", accept = c(".fa", ".fasta"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Download',
                radioButtons(ns("format"), "Select Download Format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                numericInput(ns("width"), "Width of the Image:", value = 8, min = 5, max = 20),
                numericInput(ns("height"), "Height of the Image:", value = 6, min = 5, max = 20),
                downloadButton(ns("downloadBtn"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("logoPlot"))
            )
          )
        )
      )
    )
  )
}
#' Server logic for DNA Sequence Logo Module
#'
#' Handles file input, rendering of the sequence logo plot, and downloading of the plot.
#' @param input Shiny server input
#' @param output Shiny server output
#' @param session Shiny session object
#' @importFrom ggseqlogo ggseqlogo
#' @importFrom Biostrings readDNAStringSet
#' @importFrom ggplot2 ggsave
#' @name seqLogo_server
#' @title seqLogo_server
#' @export
#'
seqLogo_server <- function(input, output, session) {
  ns <- session$ns
  
  # Render DNA sequence logo plot
  output$logoPlot <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    
    dna_seqs <- readDNAStringSet(inFile$datapath)
    dna_seqs <- as.character(dna_seqs)
    
    if (length(dna_seqs) > 0) {
      ggseqlogo(dna_seqs, seq_type = "dna")
    } else {
      plot.new()
      text(0.5, 0.5, "No valid sequences found.", cex = 1.5)
    }
  })
  
  # Handle the download of the sequence logo
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("sequence-logo.", input$format, sep = "")
    },
    content = function(file) {
      req(input$file1)
      inFile <- input$file1
      dna_seqs <- readDNAStringSet(inFile$datapath)
      dna_seqs <- as.character(dna_seqs)
      plot <- ggseqlogo(dna_seqs, seq_type = "dna")
      
      ggsave(file, plot = plot, device = input$format, width = input$width, height = input$height)
    }
  )
}
#' web UI Module
#' @description web UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title web_ui
#' @name web_ui
#' @import shiny
#' @import bslib
#' @export
#'
web_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      lapply(c(
        list(
          list(name = "iTOL", url = "https://itol.embl.de/", desc = "Phylogenetic Tree Analysis Website"),
          list(name = "MEME", url = "https://meme-suite.org/meme/", desc = "Motif Analysis Toolbox"),
          list(name = "SwissTargetPrediction", url = "http://www.swisstargetprediction.ch/", desc = "Component Target Prediction Database"),
          list(name = "PlantTFDB", url = "https://planttfdb.gao-lab.org/", desc = "Plant Transcription Factor Database"),
          list(name = "STRING", url = "https://cn.string-db.org/", desc = "Protein-Protein Interaction Networks"),
          list(name = "BlastKOALA", url = "https://www.kegg.jp/blastkoala/", desc = "KEGG BLAST for Functional Annotation"),
          list(name = "PlantPAN", url = "https://plantpan.itps.ncku.edu.tw/plantpan4/index.html", desc = "Plant Promoter Analysis Navigator"),
          list(name = "National Genomics Data Center", url = "https://ngdc.cncb.ac.cn/", desc = "NGDC"),
          list(name = "TCMSP", url = "https://old.tcmsp-e.com/tcmsp.php", desc = "Traditional Chinese Medicine Systems Pharmacology Database"),
          list(name = "PubChem", url = "https://pubchem.ncbi.nlm.nih.gov/", desc = "Open Chemistry Database"),
          list(name = "DAVID", url = "https://david.ncifcrf.gov/", desc = "Database for Annotation, Visualization and Integrated Discovery"),
          list(name = "Reactome", url = "https://reactome.org/", desc = "Open Access Pathway Database"),
          list(name = "JASPAR", url = "https://jaspar.elixir.no/", desc = "Database of Transcription Factor Binding Profiles"),
          list(name = "Ensembl", url = "https://asia.ensembl.org/index.html", desc = "Genome Browser for Vertebrates and Other Eukaryotes"),
          list(name = "MBRole", url = "https://csbg.cnb.csic.es/mbrole2/", desc = "Metabolic and Biochemical Role Analysis Tool"),
          list(name = "NCBI BLAST", url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi", desc = "Basic Local Alignment Search Tool"),
          list(name = "PubMed", url = "https://pubmed.ncbi.nlm.nih.gov/", desc = "Database of Biomedical Literature"),
          list(name = "SignalP", url = "https://services.healthtech.dtu.dk/services/SignalP-6.0/", desc = "Prediction of Signal Peptides"),
          list(name = "Pfam", url = "http://pfam.xfam.org/", desc = "Protein Family Database"),
          list(name = "AnimalTFDB", url = "https://guolab.wchscu.cn/AnimalTFDB", desc = "Animal Transcription Factor Database"),
          list(name = "Mentha", url = "https://mentha.uniroma2.it/", desc = "Database for Plant Secondary Metabolites"),
          list(name = "HMMER", url = "https://www.ebi.ac.uk/Tools/hmmer/", desc = "Protein Sequence Analysis Using Hidden Markov Models"),
          list(name = "eHOMD", url = "https://www.homd.org/", desc = "The Expanded Human Oral Microbiome Database")
        )
      ), function(item) {
        column(2,
               tags$div(style = "border: 1px solid #ccc; padding: 15px; height: 150px; text-align: center; margin-bottom: 15px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                        tags$p(item$desc, style = "font-weight: bold; margin: 0;"),
                        tags$a(href = item$url, target = "_blank", paste("Visit", item$name), style = "text-decoration: none;")
               )
        )
      })
    )
  )
}
#' help UI Module
#' @description help UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title help_ui
#' @name help_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
help_ui <- function(id) {
  ns <- NS(id)  # 创建命名空间以避免ID冲突
  tabPanel(
    title = "Help",
    fluidPage(
      theme = bslib::bs_theme(),  # 添加bslib主题支持
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100vh;",  # 使用 Flexbox 居中
        p("If you want to learn more, please click ", tags$a(href = "https://w1996jy.github.io/Aitcookbook/", "Ait Cookbook", target = "_blank"), "."),  # 添加学习链接
        br(),  # 换行
        p("If you need help while using the software, please contact me at Email: fyliangfei@163.com.")
      )
    )
  )
}
# Run the application
shinyApp(ui = app_ui, server = app_server)
