rm(list = ls())

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)

source('~/Desktop/SCMsimulation/ScmSimulation/SCM_Function.R')
source('~/Desktop/SCMsimulation/ScmSimulation/SCM_PlotFunction.R')


ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(
    title = "Synthetic Control Method simulation for comparable case study",
    titleWidth = 850,
    dropdownMenu(
      type = "messages",
      messageItem(from = "YujiaoLi",
                  message = "I lived in locus"),
      messageItem(
        from = "Do you live in locus?",
        message = "Do you feel nosiy?",
        icon = icon("question"),
        time = "24:01"
      ),
      messageItem(
        from = "Support",
        message = "When Locus could be quiet?",
        icon = icon("life-ring"),
        time = "2016-04-04"
      )
    )
  ),
  
  
  
  
  
  # 2. sidebar
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("3D-Plot", tabName = "3Dplot", icon = icon("area-chart")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      
      menuItem(
        "Source code", icon = icon("file-code-o"),
        href = "https://github.com/liyujiao1026/SCM_simulation"
      )
    ),
    
    # 2.1 control condition
    sliderInput(
      width = 300,
      inputId = "timeLength",
      label = "timeLength",
      value = 30, min = 1, max = 1000 , step = 1
    ),
    
    sliderInput(
      inputId = "intervention_Time",
      label = "intervention_Time",
      value = 15, min = 1, max = 1000 , step = 1
    ),
    
    sliderInput(
      inputId = "controlNumber",
      label = "controlNumber",
      value = 40, min = 1, max = 1000 , step = 1
    ),
    
    
    
    # 2.2 parameter
    sliderInput(
      inputId = "tau",
      label = "tau",
      value = 0.85, min = 0, max = 1 , step = 0.05
    ),
    
    sliderInput(
      inputId = "omiga",
      label = "omiga",
      value = 0.65, min = 0, max = 1 , step = 0.05
    ),
    
    sliderInput(
      inputId = "alpha",
      label = "alpha",
      value = 1 , min = -10, max = 10 , step = 0.1
    )
    
  ),
  
  
  
  
  
  
  # 3. dashboardBody
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    tabItems(
      
      tabItem(
        tabName = "dashboard",
        fluidRow(
          
          box(width = 9,
              plotOutput("SCM.plot")),
          
          
          box(width = 3,
              verbatimTextOutput("alpha_value")),
          
          
          box(width = 3,
              sliderInput(
                inputId = "seed.treat",
                label = "seed.treat",
                value = 20, min = 0, max = 10000, step = 1
              ),
              
              sliderInput(
                inputId = "seed.control",
                label = "seed.control",
                value = 30, min = 0, max = 10000, step = 1
              )
              
          )
          
        )
      ),
      
      
      
      tabItem(
        tabName = "data",
        
        fluidRow(
          h2("data"),
          #box(column(12, dataTableOutput('table')))
          dataTableOutput('table')                                
        )
      ),
      
      
      tabItem(
        tabName = "3Dplot",
        fluidRow(
         
          #box(column(12, dataTableOutput('table')))
          #box(column(12, plotlyOutput("tdPlot")) )
           plotlyOutput("tdPlot")
          
          
        )
      )
      
    )#tabItems
  )#dashboardBody
)#UI




server <- function(input, output) {
  
  data <- reactive({
    data.scm(
      timeLength = input$timeLength ,
      intervention_Time = input$intervention_Time,
      controlNumber = input$controlNumber,
      
      tau = input$tau    ,
      omiga = input$omiga ,
      alpha = input$alpha ,
      
      seed.treat = input$seed.treat,
      seed.control = input$seed.control
    )
  })
  
  
  
  output$alpha_value <- renderPrint({
    c(data()$alpha.post_1,data()$alpha.post_n)
  })
  
  
  
  output$SCM.plot <- renderPlot({
    data <- data()$data.scm
    plot.scm(data, intervention_Time = input$intervention_Time)
  })
  
  
  
  output$table <- renderDataTable({
    data()$data.scm
  })
  
  
  #############    
  output$tdPlot <- renderPlotly({
    alpha.TO()$an
  })
  
  
}




shinyApp(ui = ui, server = server)
