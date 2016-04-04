# script SCM_Plot_Prototype.R
#
# the purpose of this script is to plot the 3d for parameter sensitivity
#
# first version: 160402
# this version:  160404
# last change by: Yujiao Li


library("plotly")
source('~/Desktop/SCMsimulation/ScmSimulation/SCM_Function.R')

############ test original function ###############
# data.scm.test <- data.scm(
#   timeLength        = 30,
#   intervention_Time = 15,
#   controlNumber     = 10,
#   
#   tau    = 0.5,
#   omiga  = 0.7,
#   alpha  = 2,
#   
#   seed.treat   = 100,
#   seed.control = 2 )
# 
# 
# data.scm.test$alpha.post_1
###################################################




#--- Funcion: alpha.TO () ----------------------------------------------------------------------------------#
# alpha.TO () generate the plot of alpha, where x and y are Tau and Omiga


alpha.TO <- function() {
  
  Tau   <- seq(0.05,1,0.05)
  Omiga <- seq(0.05,1,0.05)
  
  m <- length(Tau)
  w <- length(Omiga)
  
  a1.TO <- matrix(NA,m,w)
  an.TO <- matrix(NA,m,w)
  
  for (i in 1:m) {
    for (j in 1:w) {
      result <- mapply(
        FUN = data.scm,
        
        timeLength        = 30,
        intervention_Time = 15,
        controlNumber     = 10,
        
        tau    = Tau[i],
        omiga  = Omiga[j],
        alpha  = 2,
        
        seed.treat   = 100,
        seed.control = 2
      )
      
      a1.TO[i,j] <- result[2] %>% as.numeric
      an.TO[i,j] <- result[3] %>% as.numeric
      
    }
  }
  
  
  p.a1.TO <- plot_ly(z = a1.TO, x = Tau, y = Omiga, type = "surface")
  p.an.TO <- plot_ly(z = an.TO, x = Tau, y = Omiga, type = "surface")
  
  return(list("a1" = p.a1.TO, "an" = p.an.TO))
  
}

#TEST
# alpha.TO()$a1
# alpha.TO()$an

#--------------------------------------------------------------------------------------------------------#








#--- Funcion: alpha.TA () ----------------------------------------------------------------------------------#
# alpha.TA () generate the plot of alpha, where x and y are Tau and Alpha

alpha.TA <- function() {
  
  Tau   <- seq(0.05,1,0.05)
  Alpha <- round(seq(-10,10,length.out = 20), digits = 2)
  
  m <- length(Alpha)
  w <- length(Tau)
  
  a1.TA <- matrix(NA,m,w)
  an.TA <- matrix(NA,m,w)
  
  for (i in 1:m) {
    for (j in 1:w) {
      result <- mapply(
        FUN = data.scm,
        
        timeLength        = 30,
        intervention_Time = 15,
        controlNumber     = 10,
        
        tau    = Tau[i],
        omiga  = 0.5,
        alpha  = Alpha[j],
        
        seed.treat   = 100,
        seed.control = 2
      )
      
      a1.TA[i,j] <- result[2] %>% as.numeric
      an.TA[i,j] <- result[3] %>% as.numeric
      
    }
  }
  
  
  p.a1.TA <- plot_ly(z = a1.TA, x = Tau, y = Alpha, 
                     type = "surface",colors = rainbow(100))
  p.an.TA <- plot_ly(z = an.TA, x = Tau, y = Alpha, 
                     type = "surface",colors = rainbow(100))
  
  return(list("a1" = p.a1.TA, "an" = p.an.TA))
  
}

#TEST
# alpha.TA()$a1
# alpha.TA()$an
#--------------------------------------------------------------------------------------------------------#









#--- Funcion: alpha.AO () ----------------------------------------------------------------------------------#
# alpha.AO () generate the plot of alpha, where x and y are Alpha and Omiga

alpha.AO <- function() {
  
  Alpha <- round(seq(-10,10,length.out = 20), digits = 2)
  Omiga <- seq(0.05,1,0.05)
  
  m <- length(Alpha)
  w <- length(Omiga)
  
  a1.AO <- matrix(NA,m,w)
  an.AO <- matrix(NA,m,w)
  
  for (i in 1:m) {
    for (j in 1:w) {
      result <- mapply(
        FUN = data.scm,
        
        timeLength        = 30,
        intervention_Time = 15,
        controlNumber     = 10,
        
        tau    = 0.5,
        omiga  = Omiga[i],
        alpha  = Alpha[j],
        
        seed.treat   = 100,
        seed.control = 2
      )
      
      a1.AO[i,j] <- result[2] %>% as.numeric
      an.AO[i,j] <- result[3] %>% as.numeric
      
    }
  }
  
  
  p.a1.AO <- plot_ly(z = a1.AO, x = Alpha, y = Omiga, 
                     type = "surface",colors = topo.colors(100))
  p.an.AO <- plot_ly(z = an.AO, x = Alpha, y = Omiga, 
                     type = "surface",colors = topo.colors(100))
  
  return(list("a1" = p.a1.AO, "an" = p.an.AO))
  
}


#TEST
# alpha.AO()$a1
# alpha.AO()$an
#--------------------------------------------------------------------------------------------------------#












