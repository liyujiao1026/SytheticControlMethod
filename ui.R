
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)


header <- dashboardHeader(
            title = "Synthetic Control Method simulation for comparable case study",
            titleWidth = 850,
            dropdownMenu(
                        type = "messages",
                        messageItem(from = "kca &",
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
)



sidebar <- dashboardSidebar(
            
            sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("3D-Plot", tabName = "3Dplot", icon = icon("area-chart")),
                        menuItem("Data", tabName = "data", icon = icon("table")),
                        
                        menuItem(
                                    "Source code", icon = icon("file-code-o"),
                                    href = "https://github.com/liyujiao1026/SytheticControlMethod"
                        )
            ),
            
            # UI-2-1 control condition
            sliderInput(
                        width = 300,
                        inputId = "timeLength",
                        label = "timeLength",
                        value = 30, min = 1, max = 200 , step = 1
            ),
            
            sliderInput(
                        inputId = "intervention_Time",
                        label = "intervention_Time",
                        value = 15, min = 1, max = 200 , step = 1
            ),
            
            sliderInput(
                        inputId = "controlNumber",
                        label = "controlNumber",
                        value = 40, min = 1, max = 200 , step = 1),   
            
            sliderInput(
                        inputId = "seed.sample",
                        label = "seed.sample",
                        value = 20, min = 0, max = 100, step = 1), 
            
            
            sliderInput(
                        inputId = "sample.percent",
                        label = "sample.percent",
                        value = 0.5, min = 0, max = 1, step = 0.1)
            
)

body <- dashboardBody(
            
            tabItems(
                        
                        # UI-3-1: No.1 tab, corresponding to the sidebar of UI-2 
                        tabItem(
                                    tabName = "dashboard",
                                    
                                    
                                    # First row
                                    fluidRow( box(width = 8,
                                                  plotOutput("SCM.plot", height = "500px")),
                                              
                                              column(width = 4,
                                                     
                                                     box(width = NULL,
                                                         sliderInput(
                                                                     inputId = "tau",
                                                                     label = "tau",
                                                                     value = 0.85, min = 0, max = 1 , step = 0.05), 
                                                         
                                                         
                                                         sliderInput(
                                                                     inputId = "omiga",
                                                                     label = "omiga",
                                                                     value = 0.65, min = 0, max = 1 , step = 0.05),
                                                         
                                                         sliderInput(
                                                                     inputId = "alpha",
                                                                     label = "alpha",
                                                                     value = 0.4 , min = -5, max = 5 , step = 0.05),
                                                         
                                                         #seed.control
                                                         sliderInput(
                                                                     inputId = "seed.control",
                                                                     label = "seed.control",
                                                                     value = 20, min = 0, max = 100, step = 1
                                                         )
                                                     )
                                              )#column
                                    ), #fluidRow 1
                                    
                                    
                                    
                                    
                                    # Second row  
                                    fluidRow(  
                                                box(width = 8,
                                                    plotOutput("SCM_sample.plot", height = "500px")) ,
                                                
                                                
                                                column(width = 4,       
                                                       
                                                       
                                                       box(width = NULL,
                                                           title = "alpha.Hat_1", 
                                                           status = "warning", 
                                                           solidHeader = TRUE,
                                                           background = "red",
                                                           verbatimTextOutput(c("alpha_value","mse_value"))),
                                                       
                                                       box(width = NULL,
                                                           plotOutput("plot.weight" ))
                                                       
                                                       
                                                )#column
                                    ),#fluidRow 2
                                    
                                    
                                    
                                    
                                    
                                    # Third row  
                                    fluidRow(  
                                                
                                                box(width = 8,
                                                    plotOutput("hist_rep.plot", height = "500px")),
                                                
                                                column(width = 4,    
                                                       box(width = NULL,   
                                                           
                                                           sliderInput(
                                                                       inputId = "rep.times",
                                                                       label = "rep.times",
                                                                       value = 30, min = 30, max = 1000, step = 10),
                                                           
                                                           
                                                           
                                                           
                                                           
                                                           
                                                           sliderInput(
                                                                       inputId = "seed.replicate",
                                                                       label = "seed.replicate",
                                                                       value = 10, min = 0, max = 1000, step = 10) ,
                                                           
                                                           box(width = NULL,
                                                               title = "variance", 
                                                               #status = "warning", 
                                                               #solidHeader = TRUE,
                                                               background = "purple",
                                                               verbatimTextOutput(c("variance1")))
                                                           )
                                                )#column
                                    ), #fluidRow 3
                                    
                                    
                                    # 4th row  
                                    fluidRow(  
                                                
                                                box(width = 8,
                                                    plotOutput("hist_rep_raw.plot", height = "500px")),
                                                
                                                column(width = 4,    
                                                       box(width = NULL,   
                                                           
#                                                            
#                                                           sliderInput(
#                                                                        inputId = "seed.replicate",
#                                                                        label = "seed.replicate",
#                                                                        value = 10, min = 0, max = 1000, step = 10) ),
                                                       
                                                       
                                                            sliderInput(
                                                                       inputId = "rep.times_raw",
                                                                       label = "rep.times_raw",
                                                                       value = 30, min = 30, max = 1000, step = 10) ,
                                                            box(width = NULL,
                                                                title = "variance", 
                                                                #status = "warning", 
                                                                #solidHeader = TRUE,
                                                                background = "lime",
                                                                verbatimTextOutput(c("variance2")))

                                                            )
                                               )#column
                                    )#fluidRow 4
                                    
                                    
                                    
                        ),#tabItem1
                        
                        
                        
                        # UI-3-2: No.2 tab, corresponding to the sidebar of UI-2 
                        tabItem(
                                    tabName = "data",
                                    fluidRow(column(12,dataTableOutput('table')))
                        )#, #tabItem2
                        
                        
                        # UI-3-3: No.3 tab, corresponding to the sidebar of UI-2 
#                         tabItem(
#                                     tabName = "3Dplot",
#                                     fluidRow(
#                                                 
#                                                 box(plotlyOutput("TOPlot_1")),
#                                                 box(plotlyOutput("TOPlot_n")),
#                                                 
#                                                 box(plotlyOutput("TAPlot_1")),
#                                                 box(plotlyOutput("TAPlot_n")),
#                                                 
#                                                 
#                                                 box(plotlyOutput("AOPlot_1")),
#                                                 box(plotlyOutput("AOPlot_n"))
#                                     )
#                         ) #tabItem3
                        
            ) #tabItems
) #dashboardBody




dashboardPage(header, sidebar, body, skin = "red")
