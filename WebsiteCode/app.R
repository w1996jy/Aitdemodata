library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(writexl)
library(shinyWidgets)
library(shinyFiles)
options(shiny.maxRequestSize = 50 * 1024^2)  # 设置为50MB

#' Application User Interface
#' @description Defines the user interface of the Shiny application.
#' @param request Internal parameter for {shiny}. Do not remove.
#' @import shiny
#' @import shinythemes
#' @name app_ui
#' @noRd
app_ui <- function(request) {
  fluidPage(
    golem_add_external_resources(),
    navbarPage(
      theme = shinytheme("readable"),
      title = "Ait V.1.0.0",
      # Home Tab
      tabPanel("Home", homepage_ui("home_id")),
      navbarMenu(
        "Data processing",
        tabPanel("Merge File", merge_file_ui("merge_file")),
        tabPanel("dataTransform",dataTransform_ui("dataTransform")),
        tabPanel("Long-Wide Data Transformation",Long_Wide_Data_T_ui("Long_Wide_Data_T")),
        tabPanel("Grouping statistics",Grouping_statistics_ui("Grouping_statistics")),
        tabPanel("Grouping sorting",Grouping_sorting_ui("Grouping_sorting")),
        tabPanel("Descriptive Statistics", describe_ui("describe_id")),
        tabPanel("Random Number Generator", randomNum_ui("randomNum"))
      ),
      
      # Analysis Menu
      navbarMenu(
        "Analysis",
        tabPanel("Correlation Analysis", cor_ui("cor_id")),
        tabPanel("Correlation Analysis of Two Traits", corxy_ui("correlation1")),
        tabPanel("DESeq2", deseq2_ui("deseq2_id")),
        tabPanel("Mfuzz Clustering", mfuzz_ui("mfuzz")),
        tabPanel("Kmeans analyse", kmeans_ui("kmeans")),
        tabPanel("KEGG Pathway Annotation", KEGG_ann_ui("KEGG_ann")),
        tabPanel("PCA", pca_ui("pca")),
        tabPanel("CCA Analyse", CCA_ui("CCA")),
        tabPanel("OPLS-DA Analyse", OPLS_DA_ui("OPLS_DA"))
      ),
      
      # Plot Menu
      navbarMenu(
        "Plot",
        tabPanel("Bar", bar_ui("bar")),
        tabPanel("Venn", Venn_ui("venn")),
        tabPanel("Enrichment Bubble", enrichment_bubble_ui("enrichment_bubble")),
        tabPanel("Volcano Plot", VolcanoPlot_ui("volcano_plot")),
        tabPanel("Wordcloud Plot", wordcloud2_ui("wordcloudModule")),
        tabPanel("Histogram", histogram_ui("histogram")),
        tabPanel("UpSetR Visualization", upset_ui("upset")),
        tabPanel("GO Bar Plot", GO_bar_class_ui("GO_bar_class")),
        tabPanel("Ridge Plot", ridgePlot_ui("ridgePlot")),
        tabPanel("Boxplot", boxplot_ui("boxplot")),
        tabPanel("Pie", pie_ui("pie"))
      ),
      
      # Sequence Menu
      navbarMenu(
        "Sequence",
        tabPanel("Sequence Extract", Sequence_extract_ui("sequence_extract")),
        tabPanel("Reverse Complement", reverse_complement_ui("reverse_complement")),
        tabPanel("Sequence Logo", seqLogo_ui("seqLogoModule"))
      ),
      
      # Convert Menu
      navbarMenu(
        "Convert",
        tabPanel("SVG to ...", svg_ui("svg_converter"))
      ),
      
      # Web Tab
      tabPanel("Web", Web_ui("Web_id")),
      
      # Help Tab
      tabPanel("Help", help_ui("project_init_id"))
    )
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

#' Merge File Server Module
#' @description Server logic for merging files
#' @param input, output, session Standard shiny server arguments
#' @import shiny
#' @import readxl
#' @import dplyr
#' @import writexl
#' @import shinyWidgets
#' @import shinyFiles
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom utils head
#' @noRd
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
  
  output$preview <- renderTable({
    req(joined_data())
    head(joined_data())
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

#' Application Server Logic
#' @param input, output, session Standard shiny server arguments
#' @noRd
app_server <- function(input, output, session) {
  # Call the merge_file_server module
  callModule(merge_file_server, "merge_file")
  callModule(cor_server, "cor_id")
  callModule(describe_server, "describe_id")
  callModule(deseq2_server, "deseq2_id")
  callModule(Bar_server, "bar")
  callModule(Venn_server, "venn")
  enrichment_bubble_server("enrichment_bubble")
  callModule(VolcanoPlot_server, "volcano_plot")
  callModule(wordcloud2_server, "wordcloudModule")
  callModule(histogram_server, "histogram")
  callModule(pca_server, "pca")
  callModule(corxy_server, "correlation1")
  callModule(upset_server, "upset")
  callModule(Sequence_extract_server, "sequence_extract")
  callModule(reverse_complement_server, "reverse_complement")
  callModule(seqLogo_server, "seqLogoModule")
  callModule(svg_server, "svg_converter")
  callModule(dataTransform_server, "dataTransform")
  callModule(Grouping_statistics_server, "Grouping_statistics")
  callModule(Grouping_sorting_server, "Grouping_sorting")
  callModule(randomNum_server, "randomNum")
  callModule(mfuzz_server, "mfuzz")
  callModule(kmeans_server, "kmeans")
  callModule(KEGG_ann_server, "KEGG_ann")
  callModule(CCA_server, "CCA")
  callModule(GO_bar_class_server, "GO_bar_class")
  callModule(ridgePlot_server, "ridgePlot")
  callModule(boxplot_server, "boxplot")
  callModule(pie_server, "pie")
  callModule(OPLS_DA_server, "OPLS_DA")
  callModule(Long_Wide_Data_T_server, "Long_Wide_Data_T")
}

# Add external resources function (dummy for now)
golem_add_external_resources <- function() {
  # This would add custom CSS/JS resources if needed
}
# 加载所需的库
library(shiny)
library(readr)
library(dplyr)
library(DT)

# UI 函数
Grouping_statistics_ui <- function(id) {
  ns <- NS(id)  # 使用命名空间
  fluidPage(
    titlePanel("Grouping Statistics"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
        selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
        selectInput(ns("summary_func"), "Select summary function:", 
                    choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),
        
        actionButton(ns("run"), "Run Summary")
      ),
      
      mainPanel(
        h3("Grouped Summary"),
        DTOutput(ns("summaryTable"))
      )
    )
  )
}

# 服务器部分
Grouping_statistics_server <- function(input, output, session) {
  
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
}
Long_Wide_Data_T_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Long-Wide Data Transformation"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        radioButtons(ns("transformation"), "Select transformation type:",
                     choices = list("Long format" = "long", "Wide format" = "wide")),
        uiOutput(ns("columns")), # 动态列选择
        actionButton(ns("transform"), "Transform Data"),
        downloadButton(ns("downloadData"), "Download Transformed Data")
      ),
      
      mainPanel(
        h3("Original Data"),
        DTOutput(ns("originalData")),  # 显示上传的原始数据
        h3("Transformed Data"),
        DTOutput(ns("dataTable"))      # 显示转换后的数据
      )
    )
  )
}
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
      pivot_longer(data(), cols = all_of(input$cols), names_to = "variable", values_to = "value")
    } else {
      pivot_wider(data(), names_from = all_of(input$cols), values_from = "value")
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

#' Correlation Analysis User Interface
#' @description Defines the user interface for the correlation analysis module in the Shiny application.
#' @param id A string that specifies the namespace for the module.
#' @import shiny
#' @import colourpicker
#' @importFrom shiny NS tabPanel sidebarLayout sidebarPanel mainPanel fileInput uiOutput textInput radioButtons numericInput downloadButton plotOutput helpText 
#' @name corxy_ui
#' @noRd
#' 
corxy_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Correlation Analysis of Two Traits",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
        uiOutput(ns("xcol")),  # Dropdown for selecting x-axis column
        uiOutput(ns("ycol")),  # Dropdown for selecting y-axis column
        textInput(ns("xaxis"), "X Axis Title", "X Axis"),
        textInput(ns("yaxis"), "Y Axis Title", "Y Axis"),
        colourpicker::colourInput(ns("pointColor"), "Choose Point Color", value = "black"),
        colourpicker::colourInput(ns("lineColor"), "Choose Line Color", value = "blue"),
        radioButtons(ns("format"), "Choose Download Format", choices = c("PDF", "PNG", "JPG", "SVG")),
        numericInput(ns("width"), "Width (in inches)", value = 7, min = 1, max = 20),
        numericInput(ns("height"), "Height (in inches)", value = 5, min = 1, max = 20),
        downloadButton(ns("downloadPlot"), "Download Plot"),
        tags$hr(),
        helpText("Select the columns for x and y axes, adjust titles, colors, size, and format, and download the plot.")
      ),
      mainPanel(
        plotOutput(ns("plot"))
      )
    )
  )
}
#' Correlation Analysis Server Logic
#' @description Defines the server logic for the correlation analysis module in the Shiny application.
#' @param input, output, session Standard {shiny} server function parameters.
#' @import shiny
#' @import ggplot2
#' @importFrom shiny NS reactive renderUI renderPlot downloadHandler req validate need
#' @name corxy_server
#' @noRd

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

#' UI Function for OPLS-DA Module
#' @name OPLS_DA_ui

OPLS_DA_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("OPLS-DA Analyse"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("datafile"), "Upload Metabolomics Data (CSV)", accept = ".csv"),
        fileInput(ns("groupfile"), "Upload Group Data (CSV)", accept = ".csv"),
        hr(),
        tableOutput(ns("result_table")),
        colourpicker::colourInput(ns("group1_col"), "Group1 color", value = "#F27FB2"),
        colourpicker::colourInput(ns("group2_col"), "Group2 color", value = "#A3C8F7"),
        textInput(ns("xlable"), "X Label:", value = ""),
        textInput(ns("ylable"), "Y Label:", value = "Count"),
        numericInput(ns("width"), "Plot Width (inches):", value = 8, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
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
}
#' Server Function for OPLS-DA Module
#' @import ropls
#' @import ggplot2
#' @name OPLS_DA_server
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
    
    # Run OPLS-DA
    ATR_oplsda <- ropls::opls(t(df), 
                              group, 
                              predI = 1,
                              orthoI = 1,
                              crossvalI = 6,
                              log10L = TRUE)
    
    # Render main plot
    output$OPLS_DA_Plot <- renderPlot({
      OPLS_defentu <- data.frame(ATR_oplsda@scoreMN,
                                 ATR_oplsda@orthoScoreMN)
      OPLS_defentu$Group = group
      OPLS_defentu$label = rownames(OPLS_defentu)
      
      # 设置适合科研的背景色
      theme_set(ggprism::theme_prism(border = TRUE))
      ggplot(OPLS_defentu, aes(x = p1, y = o1, label = label)) +
        geom_point(aes(colour = Group), shape = 16, size = 3) +
        labs(x = paste0('T score[1] (', ATR_oplsda@modelDF$R2X[1] * 100, '%)'),
             y = paste0('Orthogonal T score[1] (', ATR_oplsda@modelDF$R2X[2] * 100, '%)')) +
        stat_ellipse(aes(fill = Group),
                     type = "norm", geom = "polygon",
                     alpha = 0.2, color = NA, show.legend = FALSE) +
        scale_fill_manual(values = c(input$group1_col, input$group2_col)) +
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
      
      # Run OPLS-DA
      ATR_oplsda <- ropls::opls(t(df), 
                                group, 
                                predI = 1,
                                orthoI = 1,
                                crossvalI = 6,
                                log10L = TRUE)
      
      # Create plot
      OPLS_defentu <- data.frame(ATR_oplsda@scoreMN,
                                 ATR_oplsda@orthoScoreMN)
      OPLS_defentu$Group = group
      OPLS_defentu$label = rownames(OPLS_defentu)
      
      # 设置适合科研的背景色
      theme_set(ggprism::theme_prism(border = TRUE))
      p <- ggplot(OPLS_defentu, aes(x = p1, y = o1, label = label)) +
        geom_point(aes(colour = Group), shape = 16, size = 3) +
        labs(x = paste0('T score[1] (', ATR_oplsda@modelDF$R2X[1] * 100, '%)'),
             y = paste0('Orthogonal T score[1] (', ATR_oplsda@modelDF$R2X[2] * 100, '%)')) +
        stat_ellipse(aes(fill = Group),
                     type = "norm", geom = "polygon",
                     alpha = 0.2, color = NA, show.legend = FALSE) +
        scale_fill_manual(values = c(input$group1_col, input$group2_col)) +
        scale_color_manual(values = c(input$group1_col, input$group2_col))
      
      print(p)  # 输出图表到PDF文件
      dev.off()  # 关闭PDF设备
    }
  )
}

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)

# UI函数
pie_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Pie Chart Generator"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        actionButton(ns("generate"), "Run"),
        
        numericInput(ns("width"), "Download Width (inches):", value = 8, min = 1),
        numericInput(ns("height"), "Download Height (inches):", value = 6, min = 1),
        
        uiOutput(ns("colorSelectors")),
        
        downloadButton(ns("downloadPlot"), "Download Plot"),
        downloadButton(ns("downloadData"), "Download Data")
      ),
      
      mainPanel(
        plotOutput(ns("pieChart")),
        DTOutput(ns("table"))
      )
    )
  )
}

# Server函数
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



library(shiny)
library(ggplot2)
library(colourpicker)

# UI function
boxplot_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Boxplot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        uiOutput(ns("xcol_ui")),  # X-axis column selection
        uiOutput(ns("ycol_ui")),  # Y-axis column selection
        colourpicker::colourInput(ns("box_color"), "Box color:", value = "black"),
        colourpicker::colourInput(ns("box_fill"), "Box fill color:", value = "red"),
        textInput(ns("x_label"), "Enter the X-axis label:", value = "Group"),
        textInput(ns("y_label"), "Enter the Y-axis label:", value = "Value"),
        numericInput(ns("width"), "Plot Width (inches):", value = 8, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 8, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("boxPlot"))  # Correct plotOutput id
      )
    )
  )
}

# Server function
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


library(shiny)
library(ggplot2)
library(ggridges)

# UI
ridgePlot_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Ridge Plot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        uiOutput(ns("xcol_ui")),  # 动态生成X轴选择
        uiOutput(ns("ycol_ui")),  # 动态生成Y轴选择
        uiOutput(ns("fillcol_ui")),  # 动态生成填充选择
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("RidgePlot"))  # 显示生成的图表
      )
    )
  )
}

# Server
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
#' GO Bar Plot UI
#'
#' Creates the user interface for the GO Bar Plot application, allowing users to upload a CSV file,
#' select colors for different categories, and customize plot labels and dimensions.
#' @import shiny
#' @name GO_bar_class_ui
GO_bar_class_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("GO Bar Plot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        colourpicker::colourInput(ns("bp"), "Biological process color", value = "#F27FB2"),
        colourpicker::colourInput(ns("cc"), "Cellular component color", value = "#A3C8F7"),
        colourpicker::colourInput(ns("mf"), "Molecular function color", value = "#ED8000"),
        textInput(ns("xlable"), "X Label:", value = ""),
        textInput(ns("ylable"), "Y Label:", value = "Count"),
        textInput(ns("group"), "Group Label:", value = "Group"),
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("goBarPlot"))  # 显示生成的图表
      )
    )
  )
}
#' GO Bar Plot Server
#'
#' Handles the server-side logic for the GO Bar Plot application, including reading uploaded data,
#' generating the plot based on user inputs, and enabling PDF downloads with specified dimensions.
#' @import ggplot2
#' @name GO_bar_class_server
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

#' UI for CCA Module
#'
#' Creates the user interface for the CCA (Canonical Correlation Analysis) module.
#'
#' @name CCA_ui
CCA_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("CCA Analyse"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Upload CSV File1", accept = ".csv"),
        fileInput(ns("file2"), "Upload CSV File2", accept = ".csv"),
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("CCA_Plot"))  # Display the generated plot
      )
    )
  )
}

#' Server Function for CCA Module
#'
#' Handles server-side logic for the CCA (Canonical Correlation Analysis) module.
#' @import CCA
#' @name CCA_server
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

library(shiny)
library(clusterProfiler)
library(dplyr)
library(DT)

# UI部分
KEGG_ann_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("KEGG Pathway Annotation"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        actionButton(ns("run"), "Run"),
        downloadButton(ns("download"), "Download Results"),
        helpText("Please upload a CSV file with a column named 'index' containing KEGG IDs.")
      ),
      mainPanel(
        DT::dataTableOutput(ns("results"))  # 显示结果的DT表格
      )
    )
  )
}

# Server部分
KEGG_ann_server <- function(input, output, session) {
  observeEvent(input$run, {
    req(input$file)  # 确保文件已上传
    
    # 显示进度条
    withProgress(message = "Processing...", value = 0, {
      # 读取CSV文件
      kegg_data <- tryCatch({
        read.csv(input$file$datapath)
      }, error = function(e) {
        showNotification("Error reading CSV file. Please check the file format.", type = "error")
        return(NULL)
      })
      
      if (is.null(kegg_data)) return()  # 如果读取失败，停止后续处理
      
      incProgress(0.2)  # 更新进度条
      
      # 检查是否存在名为"index"的列
      if (!"index" %in% colnames(kegg_data)) {
        showNotification("The uploaded file must contain a column named 'index'.", type = "error")
        return()
      }
      
      # 提取KEGG ID
      k <- kegg_data$index  # 使用上传文件中的"index"列
      
      # 进行KEGG转换并替换
      x <- bitr_kegg(k, "kegg", "Path", "ko") %>%
        mutate(Path = gsub("ko", "map", Path))
      incProgress(0.5)  # 更新进度条
      
      # 获取KO名称
      y <- ko2name(x$Path)
      incProgress(0.2)  # 更新进度条
      
      # 将结果与KO名称合并，去掉第三列
      results <- cbind(x, KO_Name = y) %>%
        select(-KO_Name.ko)  # 去掉不需要的列
      
      # 将结果存储为反应式
      output$results <- DT::renderDataTable({
        datatable(results, options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      # 下载结果
      output$download <- downloadHandler(
        filename = function() {
          paste("KEGG_Annotation_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results, file, row.names = FALSE)
        }
      )
    })
  })
}

kmeans_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Kmeans analyse"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        selectInput("dropdown", "Data transformation", 
                    choices = c("scale","log2", "log10", "no")),
        textOutput("selected"),
        numericInput("num_input", "Cluster number：", value = 6, min = 2),
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("KmeansPlot"))  # 显示生成的图表
      )
    )
  )
}

kmeans_server <- function(input, output, session) {
  ns <- session$ns
  
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read.csv(input$file$datapath, row.names = 1)  # Read the CSV file
  })
  
  observeEvent(input$run, {
    output$KmeansPlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      
      # 生成 K-means 绘图并保存到变量
      plot_result <- KmeansTrendAnalyzer::KmeansR(df, centers = 6, table = TRUE)
      return(plot_result)  # 确保返回绘图对象
    })
  })
  
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Kmeans_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # 用户输入的宽度
      height <- input$height # 用户输入的高度
      
      pdf(file, width = width, height = height)  # 设置 PDF 尺寸
      req(uploaded_data())  # 确保数据存在
      df <- uploaded_data()
      
      # 生成并绘制 K-means 图表
      KmeansTrendAnalyzer::KmeansR(df, centers = 6, table = TRUE)
      dev.off()  # 关闭 PDF 设备
    }
  )
}


#' Application User Interface for Mfuzz Clustering
#'
#' Defines the user interface for the Mfuzz clustering module in a Shiny application.
#' 
#' @param id A unique identifier for the module.
#' @import shiny
#' @import shinythemes
#' @return A Shiny UI object for the Mfuzz clustering module.
#' @name mfuzz_ui
library(Mfuzz)
library(tidyverse)
library(Biobase)
library(VIM)

# UI function
mfuzz_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Mfuzz Soft Clustering"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        numericInput(ns("clusters"), "Number of Clusters", value = 6, min = 1),
        numericInput(ns("fuzziness"), "Fuzziness (m)", value = 1.25, min = 1, step = 0.01),
        actionButton(ns("run"), "Run Clustering"),
        br(),br(),
        downloadButton(ns("download"), "Download Cluster table"),
        br(),br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("mfuzzPlot")),
        textOutput(ns("clusterSummary"))
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
#' @param id A unique identifier for the module.
#' @import Mfuzz
#' @import tidyverse
#' @import Biobase
#' @import VIM
#' @return A Shiny server function that handles clustering and data processing.
#' @name mfuzz_server
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
    output$clusterSummary <- renderText({
      req(yeastF_clean)
      paste("Clusters assigned to", nrow(Biobase::exprs(yeastF_clean)), "genes.")
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

library(shiny)
library(DT)  # 用于数据表格
library(readr)  # 用于CSV下载

# UI部分
randomNum_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Random Number Generator"),
    
    sidebarLayout(
      sidebarPanel(
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
        ),
        
        actionButton(ns("generate"), "Generate"),
        downloadButton(ns("downloadData"), "Download CSV")
      ),
      
      mainPanel(
        DTOutput(ns("resultsTable"))  # 显示结果表格
      )
    )
  )
}

# Server部分
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
# 加载所需的库
library(shiny)
library(readr)
library(dplyr)
library(DT)

# UI 函数
Grouping_sorting_ui <- function(id) {
  ns <- NS(id)  # 使用命名空间
  fluidPage(
    titlePanel("Grouping and Sorting Statistics"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
        selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
        selectInput(ns("summary_func"), "Select summary function:", 
                    choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),
        
        selectInput(ns("sort_order"), "Select sort order:", 
                    choices = c("Ascending" = "asc", "Descending" = "desc")),
        
        actionButton(ns("run"), "Run Summary"),
        downloadButton(ns("downloadData"), "Download Summary")  # 添加下载按钮
      ),
      
      mainPanel(
        h3("Grouped and Sorted Summary"),
        DTOutput(ns("summaryTable"))
      )
    )
  )
}

# 服务器部分
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

# 加载所需的库
library(shiny)
library(readr)
library(dplyr)
library(DT)

# UI 函数
Grouping_statistics_ui <- function(id) {
  ns <- NS(id)  # 使用命名空间
  fluidPage(
    titlePanel("Grouping Statistics"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
        selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
        selectInput(ns("summary_func"), "Select summary function:", 
                    choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),
        
        actionButton(ns("run"), "Run Summary")
      ),
      
      mainPanel(
        h3("Grouped Summary"),
        DTOutput(ns("summaryTable"))
      )
    )
  )
}

# 服务器部分
Grouping_statistics_server <- function(input, output, session) {
  
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
}

# data_transform_module.R
library(shiny)
library(dplyr)
library(DT)
# UI Function for Data Transformation Module
#' Data Transformation and Normalization UI Module
#'
#' @param id Module ID
#' @return A fluidPage containing the UI elements for data transformation and normalization
#' @name dataTransform_ui
dataTransform_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Data Transformation and Normalization"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("datafile"), "Upload Data File (CSV)", accept = ".csv"),
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
                                "Center" = "center")),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("download"), "Download Table")
      ),
      
      mainPanel(
        DTOutput(ns("dataTable"))  # Use DTOutput for interactive table
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

#' UI for Correlation Analysis
#'
#' @param id Namespace ID
#'
#' @description
#' Creates the UI for correlation analysis
#' @import shiny
#' @name cor_ui
cor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Correlation Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("data_file"), "Upload CSV data"),
          actionButton(ns("run_analysis"), "Run Correlation Analysis"),
          hr(),
          sliderInput(ns("plot_width"), "Plot Width (inches):", min = 5, max = 20, value = 10),
          sliderInput(ns("plot_height"), "Plot Height (inches):", min = 5, max = 20, value = 10),
          sliderInput(ns("fontsize"), "Font Size:", min = 10, max = 30, value = 15),
          radioButtons(ns("color_scheme"), "Color Scheme:",
                       choices = list("Red-Blue" = "RdBu", "Green-Blue" = "GnBu", "Heat" = "heat")),
          radioButtons(ns("download_format"), "Select download format:",
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
          div(style = "margin-bottom: 10px;", downloadButton(ns("download_plot"), "Download Correlation Heatmap")),
          div(style = "margin-bottom: 10px;", downloadButton(ns("download_table"), "Download Correlation Table")),
          div(style = "margin-bottom: 10px;", downloadButton(ns("download_p_table"), "Download P-Value Table"))
        ),
        mainPanel(
          plotOutput(ns("heatmap_plot")),
          htmlOutput(ns("cor_table_title")),
          DT::DTOutput(ns("cor_table")),
          htmlOutput(ns("pvalue_table_title")),
          DT::DTOutput(ns("pvalue_table"))
        )
      )
    )
  )
}
#' Server for Correlation Analysis
#'
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object.
#' @importFrom stats cor.test
#' @importFrom grDevices colorRampPalette
#' @description
#' Handles the server-side logic for correlation analysis
#' @name cor_server
cor_server <- function(input, output, session) {
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, row.names = 1)
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
    color_scheme <- switch(input$color_scheme,
                           "RdBu" = colorRampPalette(c("blue", "white", "red"))(100),
                           "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                           "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
    pheatmap::pheatmap(cor_and_pval()$cor, 
                       display_numbers = TRUE, 
                       cluster_rows = FALSE, 
                       cluster_cols = FALSE,
                       fontsize = input$fontsize,
                       color = color_scheme)
  })
  
  output$cor_table_title <- renderUI({
    req(cor_and_pval())
    HTML(paste("<h4>Correlation Matrix</h4>"))
  })
  
  output$pvalue_table_title <- renderUI({
    req(cor_and_pval())
    HTML(paste("<h4>P-Value Matrix</h4>"))
  })
  
  output$cor_table <- DT::renderDT({
    req(cor_and_pval())
    cor_df <- as.data.frame(cor_and_pval()$cor)
    datatable(cor_df, rownames = TRUE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  output$pvalue_table <- DT::renderDT({
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
                             "RdBu" = colorRampPalette(c("blue", "white", "red"))(100),
                             "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                             "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
      ggsave(file, plot = pheatmap(cor_and_pval()$cor, 
                                   display_numbers = TRUE, 
                                   cluster_rows = FALSE, 
                                   cluster_cols = FALSE, 
                                   fontsize = input$fontsize,
                                   color = color_scheme,
                                   silent = TRUE)$gtable, 
             device = input$download_format,
             width = input$plot_width,
             height = input$plot_height)
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
if (!require('DT')) install.packages("DT")
#' UI for Descriptive Statistics
#'
#' @param id Namespace ID
#' @description
#' Creates the UI for descriptive statistics analysis using psych::describe
#' @import shiny
#' @import DT
#' @name describe_ui
describe_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Descriptive Statistics Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("data_file"), "Upload CSV data"),
          actionButton(ns("run_analysis"), "Run Descriptive Analysis"),
          hr(),
          downloadButton(ns("download_table"), "Download Descriptive Statistics")
        ),
        mainPanel(
          DTOutput(ns("desc_table"))
        )
      )
    )
  )
}
#' Server for Descriptive Statistics
#'
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object.
#' @description
#' Handles the server-side logic for descriptive statistics analysis using psych::describe
#' @name describe_server
#' @importFrom psych describe
#' @importFrom DT datatable
#' @importFrom utils read.csv
#' @noRd
describe_server <- function(input, output, session) {
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
    datatable(as.data.frame(desc_stats()), extensions = 'Buttons', options = list(
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

if (!require('DT')) install.packages("DT")
if (!require('ggplot2')) install.packages("ggplot2")
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
#' @noRd
deseq2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("DESeq2 Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("count_file"), "Upload count data (CSV)"),
          fileInput(ns("meta_file"), "Upload metadata (CSV)"),
          actionButton(ns("run_analysis"), "Run DESeq2 Analysis"),
          hr(),
          radioButtons(ns("download_format"), "Select download format:",
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
          downloadButton(ns("download_volcano_plot"), "Download Volcano Plot"),
          div(style = "margin-top: 20px;", downloadButton(ns("download_heatmap_plot"), "Download Heatmap")),
          div(style = "margin-top: 40px;", downloadButton(ns("download_results"), "Download Results"))
        ),
        mainPanel(
          DTOutput(ns("results_table")),
          plotOutput(ns("volcano_plot")),
          plotOutput(ns("heatmap_plot"))
        )
      )
    )
  )
}

#' DESeq2 Server Module
#' @description Server logic for DESeq2 analysis
#' @param input, output, session Standard shiny server arguments
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
#' @importFrom conflicted conflict_prefer
#' @noRd
conflicted::conflict_prefer("show", "SummarizedExperiment", "shinyjs")
if (getRversion() >= "2.15.1") utils::globalVariables(c("log2FoldChange"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("pvalue"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("color"))
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
    
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = count_data, colData = meta_data, design = ~ Condition)
    dds <- DESeq2::DESeq(dds)
    results <- DESeq2::results(dds)
    
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
      topVarGenes <- head(order(matrixStats::rowVars(SummarizedExperiment::assay(dds)), decreasing = TRUE), 20)
      mat <- SummarizedExperiment::assay(dds)[topVarGenes, ]
      mat <- mat - rowMeans(mat)
      pheatmap::pheatmap(mat, annotation_col = as.data.frame(SummarizedExperiment::colData(dds)[, "Condition", drop = FALSE]))
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
if (!require('colourpicker')) install.packages("colourpicker")
if (!require('DT')) install.packages("DT")
if (!require('ggplot2')) install.packages("ggplot2")
#' Draw a histogram
#' @description Creates a UI for drawing histograms.
#' @param id A time-series omics matrix.
#' @import shiny
#' @importFrom colourpicker colourInput
#' @import ggplot2
#' @import DT
#' @import elliptic
#' @import rlang
#' @import shinyWidgets
#' @import shinyjs
#' @import dashboardthemes
#' @importFrom utils read.csv
#' @importFrom DT dataTableOutput
#' @name Bar_ui
#' @noRd
bar_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Bar",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        # 文件上传控件
        fileInput(ns("file_upload"), "Upload CSV file",
                  accept = ".csv"),
        # 下拉菜单选择 x 和 y 轴变量
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
                                  value = "#03A9F4"),
        # 自定义下载尺寸输入框
        numericInput(ns("width"), "Image width (px):", 
                     value = 800, min = 100, step = 10),
        numericInput(ns("height"), "Image height (px):",
                     value = 600, min = 100, step = 10),
        # 添加下载格式选择下拉菜单
        selectInput(ns("format"), "Select download format:", 
                    choices = c("pdf", "png", "jpg", "svg","eps", 
                                "ps", "tex","jpeg","bmp","wmf")),
        # 添加 Run 按钮
        actionButton(ns("run_button"), "Run"),
        # 添加下载按钮
        downloadButton(ns("download_plot"), "download")
      ),
      mainPanel(
        # 使用 DT 渲染上传文件的内容
        DTOutput(ns("file_content")),
        # 显示图形
        plotOutput(ns("plot_output"))
      )
    )
  )
}

#' Bar Server Function
#'
#' @description Server logic for the Bar UI module.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @import shiny
#' @import ggplot2
#' @name Bar_server
#' @noRd
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
library(shiny)
library(VennDiagram)
library(shinyWidgets)
library(ggplot2)
library(grid)
library(tikzDevice)
if (!require('colourpicker')) install.packages("colourpicker")
if (!require('VennDiagram')) install.packages("VennDiagram")
#' Venn Diagram UI Module
#' @description UI for creating Venn diagrams
#' @import VennDiagram
#' @import shiny
#' @import shinyWidgets
#' @importFrom colourpicker colourInput
#' @import ggplot2
#' @import grid
#' @param id Module ID
#' @name Venn_ui
#' @noRd
Venn_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV file", accept = c(".csv")),
        uiOutput(ns("col_ui")),
        colourpicker::colourInput(ns("color1"), "Select Color 1", value = "red"),
        colourpicker::colourInput(ns("color2"), "Select Color 2", value = "blue"),
        colourpicker::colourInput(ns("color3"), "Select Color 3", value = "green"),
        colourpicker::colourInput(ns("color4"), "Select Color 4", value = "yellow"),
        colourpicker::colourInput(ns("color5"), "Select Color 5", value = "purple"),
        numericInput(ns("width"), "Width (inches)", value = 7),
        numericInput(ns("height"), "Height (inches)", value = 7),
        selectInput(ns("format"), "Select Format", choices = c("pdf", "png", "jpg", "svg", "eps", "ps", "tex", "jpeg", "bmp")),
        actionButton(ns("plot_venn"), "Plot Venn Diagram"),
        downloadButton(ns("downloadVenn"), "Download Venn Diagram")
      ),
      mainPanel(
        plotOutput(ns("venn_plot"))
      )
    )
  )
}

#' Venn Diagram Server Module
#' @description Server logic for creating Venn diagrams
#' @importFrom grDevices pdf png jpeg svg postscript bmp dev.off
#' @name Venn_server
#' @import grDevices
#' @noRd
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
                       bmp = bmp,
                       wmf = function(...) {
                         if (.Platform$OS.type == "windows") {
                           win.metafile(...)
                         } else {
                           stop("wmf format is only supported on Windows")
                         }
                       }
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
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('colourpicker')) install.packages("colourpicker")
#' 定义富集气泡图的用户界面
#' @description 创建用于生成富集气泡图的 Shiny 应用程序界面。
#' @param id 模块ID
#' @return 返回一个 Shiny UI 布局。
#' @import shiny
#' @name enrichment_bubble_ui
#' @noRd
enrichment_bubble_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Enrichment Bubble Plot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        numericInput(ns("num_entries"), "Number of Entries per Group", 10, min = 1),
        tags$hr(),
        checkboxInput(ns("header"), "Header", TRUE),
        textInput(ns("plot_title"), "Plot Title", "Enrichment Bubble Plot"),
        textInput(ns("x_label"), "X-axis Label", ""),
        textInput(ns("y_label"), "Y-axis Label", ""),
        numericInput(ns("num_colors"), "Number of Colors", 2, min = 2),
        uiOutput(ns("color_inputs")),
        tags$hr(),
        numericInput(ns("plot_width"), "Plot Width (inches)", 10, min = 1),
        numericInput(ns("plot_height"), "Plot Height (inches)", 8, min = 1),
        radioButtons(ns("file_type"), "File Type", choices = c("PNG" = "png", "SVG" = "svg", "PDF" = "pdf"), selected = "png"),
        downloadButton(ns("downloadPlot"), "Download Plot")
      ),
      mainPanel(
        plotOutput(ns("bubblePlot"))
      )
    )
  )
}

#' 定义富集气泡图的服务器逻辑
#'
#' @description 实现生成富集气泡图的服务器端逻辑。
#'
#' @param id 模块ID
#'
#' @return 无返回值。
#' @importFrom ggplot2 ggplot geom_point scale_color_gradientn theme_bw labs theme
#' @importFrom shiny NS moduleServer
#' @importFrom colourpicker colourInput
#' @importFrom dplyr group_by arrange slice_head ungroup
#' @importFrom readr read_csv
#' @importFrom ggplot2 ggsave
#' @name enrichment_bubble_server
#' @noRd
if (getRversion() >= "2.15.1") utils::globalVariables(c("Group"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("Pvalue"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("Description"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("Count_all"))
enrichment_bubble_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 动态生成颜色选择输入框
    output$color_inputs <- renderUI({
      num_colors <- input$num_colors
      color_inputs <- lapply(1:num_colors, function(i) {
        colourInput(ns(paste0("color", i)), paste0("Color ", i), value = ifelse(i == 1, "blue", "red"))
      })
      do.call(tagList, color_inputs)
    })
    
    output$bubblePlot <- renderPlot({
      req(input$file1)
      
      data <- readr::read_csv(input$file1$datapath, col_names = input$header)
      
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
  })
}
library(shiny)
library(ggplot2)
if (!require('colourpicker')) install.packages("colourpicker")

#' Volcano Plot User Interface
#' @description Defines the UI for the Volcano Plot module in the Shiny application.
#' @param id The module's ID.
#' @import shiny
#' @import colourpicker
#' @noRd
VolcanoPlot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "VolcanoPlot",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        # 文件上传控件
        fileInput(ns("file_upload"), "Upload CSV file:",
                  accept = c(".csv")),
        # User input: Select columns for p-value, log2 fold change, and VIP
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
        ),
        
        # Run button
        actionButton(ns("run_btn"), "Run"),
        br(),
        br(),
        # Download format selection
        radioButtons(ns("file_format"), "Choose file format:",
                     choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
                     selected = "png"),
        # Download button
        downloadButton(ns("download_plot"), "Download Plot")
      ),
      mainPanel(
        uiOutput(ns("output_ui")))
    )
  )
}

#' Volcano Plot Server Logic
#' @description Defines the server logic for the Volcano Plot module in the Shiny application.
#' @param input, output, session Internal parameters for `{shiny}`. Do not remove.
#' @param data A reactive expression that provides the data for the Volcano Plot.
#' @import shiny
#' @import ggplot2
#' @import colourpicker
#' @noRd
VolcanoPlot_server <- function(input, output, session, data) {
  # Reactive value to store the data
  data <- reactiveVal(NULL)
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    data(read.csv(input$file_upload$datapath))
    
    # Update select input choices based on data columns
    updateSelectInput(session, "pvalue_col", choices = names(data()))
    updateSelectInput(session, "log2fc_col", choices = names(data()))
    updateSelectInput(session, "vip_col", choices = names(data()))
  })
  
  # Generate the volcano plot based on user inputs
  plot_reactive <- reactive({
    req(input$pvalue_col, input$log2fc_col, input$vip_col)
    
    # Data cleaning
    data_clean <- data()
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
#' User Interface for Word Cloud Module
#'
#' This function creates a user interface for the Word Cloud module in a Shiny application.
#' It allows users to upload a CSV file and set parameters for generating a word cloud.
#'
#' @param id A unique identifier for the UI elements to handle namespaces in Shiny modules.
#' @importFrom shiny NS tabPanel sidebarLayout sidebarPanel fileInput tags checkboxInput
#' @importFrom shiny selectInput numericInput downloadButton mainPanel
#' @importFrom wordcloud2 wordcloud2Output
#' @return Returns a tab panel that includes a sidebar for inputs and a main panel for word cloud output.

wordcloud2_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Word Cloud with wordcloud2",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("textFile"), "Choose a CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput(ns("removeNumbers"), "Remove Numbers", TRUE),
        selectInput(ns("format"), "Select File Format", choices = c("PNG", "HTML")),
        numericInput(ns("imgWidth"), "Image Width", 800),
        numericInput(ns("imgHeight"), "Image Height", 600),
        downloadButton(ns("downloadImage"), "Download Word Cloud")
      ),
      mainPanel(
        wordcloud2::wordcloud2Output(ns("wordcloud"))
      )
    )
  )
}
#' Server Logic for Word Cloud Module
#'
#' This function contains the server-side logic for the Word Cloud module. It processes the uploaded
#' CSV file, generates a word cloud, and handles the download of the word cloud in PNG or HTML format.
#'
#' @param input Shiny server input.
#' @param output Shiny server output.
#' @param session Shiny server session.
#' @importFrom shiny reactive downloadHandler
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @return Does not return anything; it registers reactive values and outputs for a Shiny module.

wordcloud2_server <- function(input, output, session) {
  textData <- reactive({
    inFile <- input$textFile
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    if (input$removeNumbers) {
      df$word <- gsub('[0-9]+', '', df$word)
    }
    df <- df[!df$word == "",]
    df
  })
  
  output$wordcloud <- wordcloud2::renderWordcloud2({
    df <- textData()
    if (is.null(df))
      return(NULL)
    
    wordcloud2::wordcloud2(df, size = 0.5, color = 'random-light', shape = 'circle')
  })
  
  output$downloadImage <- downloadHandler(
    filename = function() {
      paste("wordcloud-", Sys.Date(), switch(input$format, "PNG" = ".png", "HTML" = ".html"))
    },
    content = function(file) {
      df <- textData()
      if (!is.null(df)) {
        tempFile <- tempfile(fileext = ".html")
        widget <- wordcloud2(df, size = 0.5, color = 'random-light', shape = 'circle')
        htmlwidgets::saveWidget(widget, tempFile, selfcontained = TRUE)
        
        if (input$format == "PNG") {
          webshot::webshot(tempFile, file, delay = 5, vwidth = input$imgWidth, vheight = input$imgHeight, zoom = 2)
        } else if (input$format == "HTML") {
          file.copy(tempFile, file)
        }
      }
    }
  )
}
#' Histogram UI
#'
#' This function creates the user interface for the Histogram module in a Shiny application.
#' It includes input options for file upload, column selection, bin number, and appearance settings.
#'
#' @param id The namespace identifier for UI elements to ensure they are unique.
#' @return A tabPanel containing the histogram plot UI components.
#' @import shiny
#' @import colourpicker
#' @import ggplot2
#' @noRd
histogram_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Histogram with Normal Distribution Curve",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        uiOutput(ns("selectUI")),
        numericInput(ns("bins"), "Number of bins:", 30, min = 1),
        colourpicker::colourInput(ns("lineColor"), "Select Line Color", value = "red"),
        colourpicker::colourInput(ns("fillColor"), "Select Bar Fill Color", value = "lightblue"),
        numericInput(ns("plotWidth"), "Plot Width (inches):", 7, min = 1),
        numericInput(ns("plotHeight"), "Plot Height (inches):", 5, min = 1),
        radioButtons(ns("format"), "Select Download Format:",
                     choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
        actionButton(ns("generate"), "Run"),
        downloadButton(ns("downloadPlot"), "Download")
      ),
      mainPanel(
        plotOutput(ns("histPlot")),
        textOutput(ns("errorMsg"))
      )
    )
  )
}
#' Histogram Server Logic
#'
#' Defines the server logic for the histogram plot. It handles data loading, dynamic UI updates,
#' plot rendering, and plot downloading based on user input.
#'
#' @param input, output, session Objects provided by Shiny server to handle reactive input and output.
#' @import shiny
#' @import ggplot2
#' @noRd
histogram_server <- function(input, output, session) {
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
#' PCA UI Module
#' 
#' @description This module creates the UI for the Principal Component Analysis (PCA) tab.
#' It includes file inputs for uploading the matrix and group files, color selector inputs, 
#' and controls for downloading the plot and data.
#' 
#' @param id The module's unique identifier.
#' 
#' @return A Shiny UI layout for the PCA module.
#' 
#' @importFrom shiny NS fluidPage titlePanel sidebarLayout sidebarPanel
#' @importFrom shiny fileInput uiOutput actionButton br textInput radioButtons
#' @importFrom shiny downloadButton mainPanel tabsetPanel tabPanel h3 tags plotOutput
#' @importFrom DT dataTableOutput
pca_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Principal Component Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Matrix File", multiple = FALSE),
        fileInput(ns("group"), "Upload Group File", multiple = FALSE),
        uiOutput(ns("colorSelectors")),
        actionButton(ns("Run"), "Run"),
        br(), br(),
        textInput(ns("width"), "Plot Width (inches):", "8"),
        textInput(ns("height"), "Plot Height (inches)", "6"),
        radioButtons(ns("format"), "Select Download Format:",
                     choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
        downloadButton(ns("downloadBtn2"), "Download"),  
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download Table")
      ),
      mainPanel(
        h3("Input data"),
        tags$hr(style = "border-color: black; border-width: 5px;"),
        tabsetPanel(
          tabPanel("Matrix File", DT::dataTableOutput(ns("contentsFile"))),
          tabPanel("Group File", DT::dataTableOutput(ns("contentsGroup")))
        ),
        h3("Output image"),
        tags$hr(style = "border-color: black; border-width: 5px;"),
        plotOutput(ns("resultPlot1"))
      )
    )
  )
}
#' PCA Server Module
#' 
#' @description This module handles the server-side logic for the Principal Component Analysis (PCA) tab.
#' It includes reactive expressions for handling file inputs, generating the PCA plot, and 
#' allowing users to download the resulting plot and data.
#' 
#' @param input, output, session Standard Shiny server arguments.
#' 
#' @return None. This function is used for its side effects.
#' 
#' @importFrom shiny reactive req renderPlot renderUI observeEvent
#' @importFrom shiny downloadHandler
#' @importFrom ggplot2 ggplot geom_point aes xlab ylab theme theme_bw scale_colour_manual scale_fill_manual
#' @importFrom colourpicker colourInput
#' @importFrom DT renderDataTable
#' @importFrom utils write.csv
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
    `%||%` <- function(a, b) {
      if (!is.null(a)) a else b
    }
    
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
#' Correlation Analysis User Interface
#' @description Defines the user interface for the correlation analysis module in the Shiny application.
#' @param id A string that specifies the namespace for the module.
#' @import shiny
#' @import colourpicker
#' @importFrom shiny NS tabPanel sidebarLayout sidebarPanel mainPanel fileInput uiOutput textInput radioButtons numericInput downloadButton plotOutput helpText 
#' @name corxy_ui
#' @noRd
#' 
corxy_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Correlation Analysis of Two Traits",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
        uiOutput(ns("xcol")),  # Dropdown for selecting x-axis column
        uiOutput(ns("ycol")),  # Dropdown for selecting y-axis column
        textInput(ns("xaxis"), "X Axis Title", "X Axis"),
        textInput(ns("yaxis"), "Y Axis Title", "Y Axis"),
        colourpicker::colourInput(ns("pointColor"), "Choose Point Color", value = "black"),
        colourpicker::colourInput(ns("lineColor"), "Choose Line Color", value = "blue"),
        radioButtons(ns("format"), "Choose Download Format", choices = c("PDF", "PNG", "JPG", "SVG")),
        numericInput(ns("width"), "Width (in inches)", value = 7, min = 1, max = 20),
        numericInput(ns("height"), "Height (in inches)", value = 5, min = 1, max = 20),
        downloadButton(ns("downloadPlot"), "Download Plot"),
        tags$hr(),
        helpText("Select the columns for x and y axes, adjust titles, colors, size, and format, and download the plot.")
      ),
      mainPanel(
        plotOutput(ns("plot"))
      )
    )
  )
}
#' Correlation Analysis Server Logic
#' @description Defines the server logic for the correlation analysis module in the Shiny application.
#' @param input, output, session Standard {shiny} server function parameters.
#' @import shiny
#' @import ggplot2
#' @importFrom shiny NS reactive renderUI renderPlot downloadHandler req validate need
#' @name corxy_server
#' @noRd

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
#' User Interface for UpSetR Visualization
#'
#' This function defines the UI components for the UpSetR visualization tab,
#' including file input, numeric inputs for plot dimensions, and download buttons.
#'
#' @param id A unique identifier for this UI component.
#' @return A `shiny::tabPanel` object containing the UI elements for UpSetR visualization.
#' @name upset_ui
upset_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "UpSetR Visualization",
    sidebarLayout(
      sidebarPanel(
        # File input for CSV file upload
        fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
        helpText("The uploaded CSV file should have at least two columns."),
        
        # Numeric inputs for plot dimensions
        numericInput(ns("width"), "Width (in inches)", value = 12, min = 1),
        numericInput(ns("height"), "Height (in inches)", value = 6, min = 1),
        
        # Download buttons
        downloadButton(ns("downloadPlot"), "Download Plot"),
        downloadButton(ns("downloadData"), "Download Data"),
        tags$hr(),
        helpText("Upload the CSV file and customize the download options.")
      ),
      mainPanel(
        plotOutput(ns("plot"))
      )
    )
  )
}

#' Server logic for UpSetR Visualization
#'
#' This function contains the server logic for rendering the UpSetR plot and 
#' handling file uploads and downloads. It processes the uploaded CSV data, 
#' generates the UpSetR plot, and provides options to download both the plot 
#' and the processed data.
#'
#' @param input A list of input values from the UI.
#' @param output A list of output values to be rendered in the UI.
#' @param session The Shiny session object.
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @return NULL
#' @name upset_server
upset_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    validate(need(ncol(df) >= 2, "The CSV file must have at least two columns"))
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
#' Define the user interface for sequence extraction
#'
#' @description Create a Shiny application UI for sequence extraction.
#'
#' @param id Module ID
#'
#' @return Returns a Shiny UI layout.
#' @name Sequence_extract_ui
#' @noRd
Sequence_extract_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Sequence Extraction"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("id_file"), "Upload ID File (CSV)"),
        fileInput(ns("fasta_file"), "Upload FASTA File"),
        numericInput(ns("nbchar"), "Sequence Length per Line (bp)", value = 60, min = 1),
        actionButton(ns("run_btn"), "Run"),
        downloadButton(ns("download_fasta"), "Download Matched Sequences FASTA File", disabled = TRUE)
      ),
      mainPanel(
        verbatimTextOutput(ns("status"))
      )
    ),
    tags$script(
      'Shiny.addCustomMessageHandler("enableButton", function(message) {
         $("#" + message.id).prop("disabled", false);
      });'
    )
  )
}

#' Define the server logic for sequence extraction
#'
#' @description Implement the server logic for sequence extraction.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return No return value.
#' @name Sequence_extract_server
#' @importFrom seqinr read.fasta
#' @importFrom seqinr write.fasta
#' @noRd
Sequence_extract_server <- function(input, output, session) {
  temp_file <- NULL
  
  observeEvent(input$run_btn, {
    req(input$id_file, input$fasta_file)
    
    # Read ID file
    id_data <- readr::read_csv(input$id_file$datapath)
    ids <- id_data[[1]]
    
    # Read FASTA file
    fasta_data <- seqinr::read.fasta(input$fasta_file$datapath, forceDNAtolower = FALSE)
    
    # Extract matching sequences
    matched_ids <- ids[ids %in% names(fasta_data)]
    select_list <- lapply(matched_ids, function(id) grep(id, names(fasta_data), ignore.case = TRUE))
    select_transcript <- fasta_data[unlist(select_list)]
    
    # Write to temporary FASTA file
    temp_file <<- tempfile(fileext = ".fa")
    seqinr::write.fasta(sequences = select_transcript,
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
#' Define the user interface for reverse complement functionality
#'
#' @description Create a Shiny UI for getting complement sequences (forward and reverse).
#'
#' @param id Module ID
#'
#' @return Returns a Shiny UI layout.
#' @name reverse_complement_ui
#' @noRd
#' 
reverse_complement_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Get Complement Sequence"),
    sidebarLayout(
      sidebarPanel(
        textAreaInput(ns("input_seq"), "Enter DNA sequence", rows = 10, placeholder = "Enter DNA sequence...", 
                      value = "ATTTATCCTTCAGCCCCTCTAGGCTTAGCAGTCAGCTTGACTTGAGGAGGAAAAAAAAACAACACACCACACTGCATTAATAATGGGGCGATCACCATGCTGTGAGAAGATTGGACTGAAGAAAGGTCCATGGACGCCGGAGGAGGATGAGAAGCTGCTTGCCTTCGTTGAGGAACATGGACACGGGAGCTGGCGGGCATTACCTGCGAAGGCAGGCTTGCAGAGGTGTGGGAAGAGCTGCAGGTTGAGGTGGACAAACTACCTGAGGCCGGACATCAAGAGGGGCAAGTTCAGCTTGCAAGAAGAACAGACCATCATCCAGCTTCACGCTCTTTTAGGCAACAGGTGGTCGGCCATTGCAACACATCTACCAAATCGCACGGACAACGAGATCAAGAACCACTGGAACACGCACCTCAAGAAAAGGCTGGCCAAGATTGGGATCGATCCTGTCACCCACAAATCTACCTGTGGCACTCTCACTGGCACCACGAACGACAGATCAGCCAAGGCCGCGGCAAGCCTCAGCCACATGGCACAATGGGAGAATGCCCGCCTCGAGGCTGAGGCACGGCTGGCTCGAGAATCGAAGACACGAACAGCAACACCGACGCCATCTGCACTCCATGCGCAGCCAATGGATCTACCTGCCTCTGCTGCTTCTCCATGGCTTGACGTGTTGCATGCTTGGCAGGGTGCAAAGATAGACCTGGAGTCACCTACCTCCACACTGACGTTTACAGGGAGCAATGGTGGCATGCTGCCAACCCCCAGGACCAACGGACCAGAGGTATCAGAAAGCAACTCCGCGATGTCGCATTATCAGATGAGCGATGAGTTGGAGGGTGAAGAAACCTATTGGCAGATCTTCAGCAAGCACCAAGTGCCGGAAGTGGACAGCAAGGAGAGTGAAGATGACTTCATTGGCTGTGAGGAGCCGTGGTTCTCAGGGATGGCTGGGGTTGGAGCTGGCATGCTGCTTGATGTATCCAATGAGCATGAGCTATCAGAATGCTGGGGTGAGTCCAGCAGTGGCCAAACTGTTGAGCACAGCAAGCAAGCATCCGATAAGGAGGACAAGGATTATTGGAATTGGGTCCTTGACAGAGTAAACTCAGAGCTGACAGCACAGTCGCCTTCCTTGGTCTAAAATACAGCCCCCCCCCCCCCCCCCCCCCCCCCTGCAGTTTTTTTTTCAACGACTCCTTAGCAAAATCTATTGATCATCTTAAGTCAAAAGAGAAACAGAACATACGAGAAGTACAAATAAATTCTACAGAAGGATGTTTTTTGTCCATGCATGTGAGTGAAATAATTAGCTGTTGTGTGTATGTAGTAAATATTTTTTTCCCTGATTTCTGAATGTATGTTAAGCCTCCAGGACAACAGCAGGGACACTACATTAATGGTCCAAGGACTTTACAGTTGTTAGTTCAACGGCCAAGTTGTCTTGTCTCCCTCTCTTTTTGAATCCATCCCAGAATGAGTAAGAA"),
        radioButtons(ns("complement_type"), "Choose complement type:",
                     choices = list("Forward Complement" = "forward", "Reverse Complement" = "reverse"),
                     selected = "reverse"),
        actionButton(ns("run_btn"), "Get Complement Sequence"),
        downloadButton(ns("download_fasta"), "Download")
      ),
      mainPanel(
        verbatimTextOutput(ns("output_seq"))
      )
    )
  )
}
#' Define the server logic for reverse complement functionality
#'
#' @description Implement the server logic to get complement sequences.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @import Biostrings
#' @return No return value.
#' @name reverse_complement_server
#' @noRd
reverse_complement_server <- function(input, output, session) {
  complement_seq <- eventReactive(input$run_btn, {
    req(input$input_seq)
    
    # Remove whitespace characters
    cleaned_input_seq <- gsub("\\s+", "", input$input_seq)
    input_seq <- Biostrings::DNAString(cleaned_input_seq)
    
    if (input$complement_type == "reverse") {
      Biostrings::reverseComplement(input_seq)
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
      seq_set <- Biostrings::DNAStringSet(complement_seq())
      writeXStringSet(seq_set, filepath = file, format = "fasta")
    }
  )
}
#' UI for DNA Sequence Logo Module
#'
#' @param id Unique identifier for the Shiny module
#' @param title Title of the tab panel
#' @return Returns a tab panel with user interface elements for DNA sequence logo generation.
seqLogo_ui <- function(id, title = "Sequence Logo") {
  ns <- NS(id)
  tabPanel(
    title,
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose DNA Sequence File", accept = c(".fa", ".fasta")),
        tags$hr(),
        h5("Upload a FASTA file containing DNA sequences to generate a sequence logo."),
        radioButtons(ns("format"), "Select Download Format:",
                     choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
        numericInput(ns("width"), "Width of the Image:", value = 8, min = 5, max = 20),
        numericInput(ns("height"), "Height of the Image:", value = 6, min = 5, max = 20),
        downloadButton(ns("downloadBtn"), "Download")
      ),
      mainPanel(
        plotOutput(ns("logoPlot"))
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
seqLogo_server <- function(input, output, session) {
  ns <- session$ns
  
  # Render DNA sequence logo plot
  output$logoPlot <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    
    dna_seqs <- Biostrings::readDNAStringSet(inFile$datapath)
    dna_seqs <- as.character(dna_seqs)
    
    if (length(dna_seqs) > 0) {
      ggseqlogo::ggseqlogo(dna_seqs, seq_type = "dna")
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
      dna_seqs <- Biostrings::readDNAStringSet(inFile$datapath)
      dna_seqs <- as.character(dna_seqs)
      plot <- ggseqlogo::ggseqlogo(dna_seqs, seq_type = "dna")
      
      ggsave(file, plot = plot, device = input$format, width = input$width, height = input$height)
    }
  )
}
#' UI for SVG conversion
#'
#' @param id Namespace ID
#' @description
#' Creates the UI for SVG conversion
#' @name svg_ui
#' @noRd
#'
svg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      theme = shinytheme("spacelab"),
      titlePanel("SVG File Converter"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("file"), "Upload SVG File"),
          radioButtons(ns("format"), "Select Output Format:",
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg")),
          actionButton(ns("convertBtn"), "Convert File"),
          hr(),
          downloadButton(ns("downloadBtn"), "Download Converted File")
        ),
        mainPanel(
          plotOutput(ns("plotOutput"), height = "600px", width = "100%")
        )
      )
    )
  )
}
#' Server for SVG conversion
#'
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object
#' @description
#' Handles the server-side logic for SVG conversion
#' @import rsvg
#' @import jpeg
#' @import png
#' @import graphics
#' @name svg_server
#' @noRd
svg_server <- function(input, output, session) {
  
  svg_file <- reactive({
    req(input$file)
    input$file$datapath
  })
  
  converted_file <- reactiveVal(NULL)
  
  observeEvent(input$convertBtn, {
    req(svg_file())
    
    output_format <- input$format
    temp_file <- tempfile(fileext = paste0(".", output_format))
    
    if (output_format == "pdf") {
      rsvg_pdf(svg_file(), temp_file)
    } else if (output_format == "png") {
      rsvg_png(svg_file(), temp_file)
    } else if (output_format == "jpeg") {
      # Convert SVG to PNG first
      temp_png <- tempfile(fileext = ".png")
      rsvg_png(svg_file(), temp_png)
      # Convert PNG to JPEG
      img <- readPNG(temp_png)
      writeJPEG(img, temp_file)
    }
    
    converted_file(temp_file)
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste0("converted_file.", input$format)
    },
    content = function(file) {
      file.copy(converted_file(), file)
    }
  )
  
  output$plotOutput <- renderPlot({
    req(svg_file())
    plot.new()
    rasterImage(rsvg::rsvg(svg_file()), 0, 0, 1, 1)
  }, res = 96)
}

library(shiny)
library(shinythemes)
library(DiagrammeR)

library(shiny)
library(shinythemes)
library(DiagrammeR)

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
          tags$li("Format Conversion: Allows users to convert SVG files into multiple formats to meet different needs.")
        ),
        p("With Ait V.1.0.0, users can efficiently process and analyze complex data, enhancing research productivity and supporting scientific discoveries.")
    ),
    
    # Add Flowchart Section above the image
    div(style = "text-align: center; margin-bottom: 20px; max-width: 800px; margin: auto;",
        grViz(flowchart_graph)  # Render the flowchart here
    )
  )
}

# Create circular flowchart data with "Home" changed to "Ait"
flowchart_graph <- "
digraph flowchart {
  graph [layout = circo, rankdir = LR]

  node [shape = ellipse, style = filled, fontname = 'Arial', fontsize = 12]
  
  Ait [label = 'Ait', fillcolor = '#FFDDC1']  // Changed 'Home' to 'Ait'
  DataProcessing [label = 'Data Processing', fillcolor = '#CFE2F3']
  Analysis [label = 'Analysis', fillcolor = '#D9EAD3']
  Plot [label = 'Plot', fillcolor = '#F9CB9C']
  Sequence [label = 'Sequence', fillcolor = '#EAD1DC']
  Convert [label = 'Convert', fillcolor = '#F6BBE2']
  Web [label = 'Web', fillcolor = '#E2EFDA']
  Help [label = 'Help', fillcolor = '#C3D9D8']

  edge [color = '#000000', arrowhead = normal, arrowsize = 0.8]

  // Main menu items
  Ait -> DataProcessing
  Ait -> Analysis
  Ait -> Plot
  Ait -> Sequence
  Ait -> Convert
  Ait -> Web
  Ait -> Help

  // Data Processing submenu items
  DataProcessing -> MergeFile [label = 'Merge File']
  DataProcessing -> DataTransform [label = 'Data Transform']
  DataProcessing -> LongWideDataTransformation [label = 'Long-Wide Data Transformation']
  DataProcessing -> GroupingStatistics [label = 'Grouping Statistics']
  DataProcessing -> GroupingSorting [label = 'Grouping Sorting']
  DataProcessing -> DescriptiveStatistics [label = 'Descriptive Statistics']
  DataProcessing -> RandomNumberGenerator [label = 'Random Number Generator']

  // Analysis submenu items
  Analysis -> CorrelationAnalysis [label = 'Correlation Analysis']
  Analysis -> CorrelationAnalysisTwoTraits [label = 'Correlation Analysis of Two Traits']
  Analysis -> DESeq2 [label = 'DESeq2']
  Analysis -> MfuzzClustering [label = 'Mfuzz Clustering']
  Analysis -> KmeansAnalyse [label = 'K-means Analysis']
  Analysis -> KEGGPathwayAnnotation [label = 'KEGG Pathway Annotation']
  Analysis -> PCA [label = 'PCA']
  Analysis -> CCAAnalyse [label = 'CCA Analysis']
  Analysis -> OPLSDAAnalyse [label = 'OPLS-DA Analysis']

  // Plot submenu items
  Plot -> BarPlot [label = 'Bar Plot']
  Plot -> VennPlot [label = 'Venn Diagram']
  Plot -> EnrichmentBubblePlot [label = 'Enrichment Bubble Plot']
  Plot -> VolcanoPlot [label = 'Volcano Plot']
  Plot -> WordcloudPlot [label = 'Wordcloud Plot']
  Plot -> Histogram [label = 'Histogram']
  Plot -> UpSetRVisualization [label = 'UpSetR Visualization']
  Plot -> GOBarPlot [label = 'GO Bar Plot']
  Plot -> RidgePlot [label = 'Ridge Plot']
  Plot -> Boxplot [label = 'Boxplot']
  Plot -> Pie [label = 'Pie Chart']

  // Sequence submenu items
  Sequence -> SequenceExtract [label = 'Sequence Extract']
  Sequence -> ReverseComplement [label = 'Reverse Complement']
  Sequence -> SequenceLogo [label = 'Sequence Logo']

  // Convert submenu items
  Convert -> SVGConverter [label = 'SVG to...']
  
  // Web and Help sections
  Web -> WebSection [label = 'Web']
  Help -> HelpSection [label = 'Help']
}
"

# If running this in a Shiny app, ensure to call the homepage_ui function as part of your main UI.



Web_ui <- function(id) {
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

#' Draw a histogram
#'
#' @description Creates a UI for providing help information about various statistical analysis methods.
#'
#' @param id A unique identifier for the UI component.
#' @import shiny
#' @name help_ui
#' @noRd
help_ui <- function(id) {
  ns <- NS(id)  # 创建命名空间以避免ID冲突
  tabPanel(
    title = "Help",
    fluidPage(
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
