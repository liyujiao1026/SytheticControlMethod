
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)

source('global.R', local = TRUE)
# source('SCM_Function.R',local = TRUE)
# #source('~/Desktop/SCMsimulation/ScmSimulation/SCM_PlotFunction.R')
# source('SCM_EstimateFunction.R', local = TRUE)
# source('SCM_PlotTrendFunction.R', local = TRUE)
# source('SCM_ReplicationFunction.R', local = TRUE)
# source('g.R', local = TRUE)

shinyServer(function(input, output) {
            # SEVER(1): data for the whole environment------------------------------#
            data <- reactive({
                        data.scm(
                                    timeLength = input$timeLength ,
                                    intervention_Time = input$intervention_Time,
                                    controlNumber = input$controlNumber,
                                    
                                    tau = input$tau    ,
                                    omiga = input$omiga ,
                                    alpha = input$alpha ,
                                    
                                    seed.control = input$seed.control
                        )
            })
            
            
            result.SCM <- reactive({
                        SCM.sample(data(), seed.sample = input$seed.sample,
                                   sample.percent = input$sample.percent,  
                                   intervention_Time = input$intervention_Time)
            })
            
            
            data.rep <- reactive({
                                  rep.scm(rep.times = input$rep.times, 
                                            seed.replicate = input$seed.replicate,
                                            data_scm = data(), 
                                            sample.percent = input$sample.percent,  
                                            intervention_Time = input$intervention_Time)
            })
            
            data.rep_raw <- reactive({
                             repli_alpha_raw(   time = input$rep.times_raw,
                                                #seed.control.rep=10, # this seed is default as 10, we can change surely
                                                seed.sample = input$seed.sample,
                                                sample.percent = input$sample.percent,
                                                
                                                timeLength        = input$timeLength,
                                                intervention_Time = input$intervention_Time,
                                                controlNumber     = input$controlNumber ,
                                                
                                                
                                                tau    = input$tau,
                                                omiga  = input$omiga,
                                                alpha  = input$alpha
                        )
            })
            ##############################################################
            
            #SEVER(1.1): extract the alpha for showing in the UI
            output$alpha_value <- renderPrint({
                        round(result.SCM()$alpha1.Hat_SCM , digits = 3)
            })
            
            output$variance1 <- renderPrint({
                         round( var( data.rep()), digits = 6)
                        
            })
            
            output$variance2 <- renderPrint({
                        round( var( data.rep_raw()), digits = 6)
                        
            })
            
            output$mse_value <- renderPrint({
                        result.SCM()$MSE.Pretime_SCM %>% as.numeric
            })
            
            # SEVER(1.2): plot the trend
            output$SCM.plot <- renderPlot({
                        data <- data()
                        plot.scm(data, intervention_Time = input$intervention_Time)
            })
            
            output$SCM_sample.plot <- renderPlot({
                        data.scm <- result.SCM()$sample.data_SCM
                        
                        plot.scm(data.scm, intervention_Time = input$intervention_Time)
            })
            
            
            # plot the hist
            output$hist_rep.plot <- renderPlot({
                        hist(data.rep(), col = "pink",xlim = c(-1,1),
                             xlab = "Replicate times", ylab = "alpha1.Hat", main = "alpha1 histogram_bootstrap")
                        
                        abline(v = input$alpha, col = "red", lwd = 2)
                        text(paste0(input$alpha), x = input$alpha, y = -0.03, col = "red", cex = 1)
                        lines(density(data.rep())) 
                        
                        
                        data1 <- data.rep()
                        lower <- round( quantile(data1 , 0.025), digits = 4)
                        upper <- round( quantile(data1 , 0.925), digits = 4)
                        abline(v = c(lower,upper), col = "blue",  lwd = 2, lty = 2)
                        
                        text(paste0("2.5%Q\n",lower), x = lower, y = -0.03, col = "blue", cex = 1)
                        text(paste0("97.5%Q\n",upper), x = upper, y = -0.03, col = "blue", cex = 1)
                        
                      
              
            })
            
     
            
            output$hist_rep_raw.plot <- renderPlot({
                        
                       hist(data.rep_raw(), 
                            main = "alpha.hat raw generated",
                            col = "lightgreen",
                            xlab = 'replicate times',
                            ylab = 'alpha.hat',
                            xlim = c(-1,1)
                            )
                        
                       lines(density(data.rep_raw()))
                       abline(v = input$alpha, col = "red", lwd = 2)
                       text(paste0(input$alpha), x = input$alpha, y = -0.03, col = "red", cex = 1)
                       
                       data.2 <- data.rep_raw()
                       lower.2 <- round( quantile( data.2, 0.025), digits = 4)
                       upper.2 <- round( quantile( data.2, 0.925), digit = 4)
                       abline(v = c(lower.2,upper.2), col = "blue",  lwd = 2, lty = 2)
                       
                       text(paste0("2.5% Q\n",lower.2), x = lower.2, y = -0.03, col = "blue", cex = 1)
                       text(paste0("97.5% Q\n",upper.2), x = upper.2, y = -0.03 , col = "blue", cex = 1)


            })
            
            
            
            
            
            
            # SEVER(1.3): output the data table
            output$table <- renderDataTable(
                        {round( data(), digits = 3) },
                        options = list(autoWidth = TRUE)
            )
            
            
            
            output$plot.weight <- renderPlot(
                        plot(result.SCM()$w.Hat_SCM %>% sort, main = "Units' weight estimation",
                             xlab = "control units", ylab = "weights estimated", pch = 10, col = "purple")
            )
            # SEVER section 1 is done----------------------------------------------------------------#
   
            
            

})
##############################################
