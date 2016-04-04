# script SCM_Function.R
#
# the purpose of this script is to simulate the Synthetic Control Method
#
# first version: 160331
# this version:  160402
# last change by: Yujiao Li


library(ggplot2)
library(tidyr)

#--- Funcion: rw.sim() ----------------------------------------------------------------------------------#
# rw.sim() generate the random walk
# Random Walk model: Y_t = tau * (Y_t-1) + et
# @timeLength: the length of time series
# @seed: the random seed

rw.sim <- function(timeLength, seed, tau) {
  n <- timeLength
  set.seed(seed)
  e <- rnorm(n)
  y <- c()
  y[1] <- e[1]
  
  for (i in 2:n) {
    y[i] <- tau *y[i - 1] + e[i]
  }
  
  return(ts(y))
  
}

# test
# rw.sim(timeLength = 10,tau=0.5,seed = 2)
# sapply(c(1,2,3), rw.sim, tau=1, timeLength=10)
#--------------------------------------------------------------------------------------------------------#











#--- Funcion: dataRW.sim() -----------------------------------------------------------------------------#
# dataRW.sim() generate the controlUnits with the model of Random Walk

# Model: Zt = Z0 + Yt,
#        Z0 is the common fixed random walk for each Zt,
#        Yt is random walk varied by different seed ,
# return: Zt

# @controlNumber: the number of control units
# @timeLength: the length of time period

dataRW.sim <- function(controlNumber, timeLength, 
                       tau, omiga,  seed.control) {
  
  set.seed(seed.control)
  seed.control.series <- sample(1:100000 , controlNumber )
  
  Z0 <- rw.sim(seed = seed.control, tau = tau, timeLength = timeLength)
  Yt <- sapply(seed.control.series , rw.sim, timeLength = timeLength, tau = tau)
  Zt <- apply(Yt, 2, function(x) {
    omiga * Z0 + (1 - omiga) * x
  })
  data <- data.frame(Zt)
  round(data, digits = 4)
  rownames(data) <- paste("Time", 1:timeLength , sep = "_")
  colnames(data) <- paste("ControlUnit", 1:controlNumber ,sep = "_")
  
  return(data)
}


#test
dataRW <- dataRW.sim( controlNumber = 5, timeLength = 10, tau = 0.5, omiga = 0.3, seed.control = 1 )
#--------------------------------------------------------------------------------------------------------#












#--- Funcion: y.obs() ----------------------------------------------------------------------------------#
# y.obs() generate post-intervention observation given the pre-intervention data

# Model: Y_t = (Y_t-1 + e_t) * (1+alpha) ,
#        alpha is the assumed caused effect from intervention

# return: y.obs

# @ y.hat: the equally weighted average of other control units
# @ intervention_Time : which time point is the intervention time
# @ alpha : the caused effect
# @ seed.post : random walk for the post-intervention observations

y.obs <- function(y.hat, intervention_Time, alpha, seed.treat) {
  
  t.int <- intervention_Time
  y.observation <- c()
  
  # (1)==> ( y[1], y[2], ..., y[t] ) ------------#
  # before the intervention, y.hat==y.observation
  y.observation[1:t.int] <- y.hat[1:t.int]
  
  # after the intervention, y.hat[t] = (y.hat[t-1] + e ) + alpha
  # alpha is the effect from intervention
  # the following random walk also needs te be set the seed for reproducible research
  l <- length(y.hat)  
  set.seed(seed.treat)
  ee <- rnorm(l - t.int)
  
  
  # (2)==> y[t+1] -----------------------------------#
  # y.observation : only 1 year after the intervetion 
  y.observation[t.int + 1] <- (y.hat[t.int] + ee[1]) * (1 + alpha)
  
  
  # (3)==> (y[t+2], y[t+3], y[t+4], ....) -------------#
  # y.observation : several years after the intervetion   
  for (j in (t.int + 2):l) {
    y.observation[j] <- y.observation[j - 1] + ee[j - t.int]
  }
  
  return(y.observation)
}

#test
# dataRW <-dataRW.sim(controlNumber = 5, timeLength = 10, omiga = 0.3, seed.control=0)
# y.Hat <- apply(dataRW, 1, mean)
# y.obs(y.hat=y.Hat , intervention_Time=5, alpha=1, seed.treat=100)
#------------------------------------------------------------------------------------------------------#








#--- Funcion: data.scm() -----------------------------------------------------------------------------#
# data.scm() (1)generate the dataframe which is simulated random walk
#            (2)calculate the difference: alpha = y.Hat - y.Obs
#               (2.1) y.Hat ==> Synthetic value (weighted average) and
#               (2.2) y.Obs ==> True value (follow its own time series value)
#
# return: (1) data.scm as a dataframe
#         (2) alpha.post_1  ==> After the intervention, what is the alpha of the following ONE year
#         (3) alpha.post_n  ==> After the intervention, what is the AVERAGE of alpha from the following SEVERAL years 


data.scm <- function(timeLength , intervention_Time , controlNumber,
                     tau, omiga , alpha,
                     seed.treat,seed.control) {
  
  # 1. simulate the data
  data <- dataRW.sim(controlNumber = controlNumber, timeLength = timeLength , 
                     tau = tau, omiga = omiga, seed.control = seed.control)
  
  y.Hat <- apply(data, 1, mean)
  
  y.Obs <- y.obs(y.hat = y.Hat, intervention_Time = intervention_Time, 
                 alpha = alpha, seed.treat = seed.treat )
  
  
  
  # 2. alpha calculation
  
  a.start <- intervention_Time + 1
  a.end <- timeLength
  
  alpha.post_1 <- y.Hat[a.start] - y.Obs[a.start]
  alpha.post_n <- mean(y.Hat[a.start:a.end] - y.Obs[a.start:a.end])
  
  names(alpha.post_1) = "alpha.one"
  names(alpha.post_n) = "alpha.mean"
  
  # 3. Merge as the dataframe
  data.scm.frame <- data.frame("Time" = 1:timeLength, data, "Treat.Hat" = y.Hat, "Treat.Obs" = y.Obs)
  
  
  
  return(list("data.scm" = data.scm.frame, 
                "alpha.post_1" = alpha.post_1,
                "alpha.post_n" = alpha.post_n) )
}

# test
# data.scm.test <- data.scm(
#   timeLength        = 30,
#   intervention_Time = 20,
#   controlNumber     = 10,
#   
#   tau    = 0.5,
#   omiga  = 0.7,
#   alpha  = 2,
#   
#   seed.treat   = 10,
#   seed.control = 2 )


#--------------------------------------------------------------------------------------------------------#








#--- Funcion: plot.scm() -------------------------------------------------------------------------------#
# plot.scm() generate the plot of how the control units and treated unit varied
# return: plot


plot.scm <- function(data.scm, intervention_Time){   
  
  data.plot <- gather(data.scm, Unit, Value,-Time)
  
  plot <- ggplot(data.plot, aes(x = Time, y = Value, colour = Unit,group = Unit)) +
    geom_line(lwd = 1, alpha = 0.4,linetype = 1) +
    
    geom_line(
      data = data.plot[data.plot$Unit == "Treat.Obs",],
      aes(x = Time, y = Value), colour = "red",
      lwd = 1.5, linetype = 1 ) +
    
    geom_line(
      data = data.plot[data.plot$Unit == "Treat.Hat",],
      aes(x = Time, y = Value), colour = "black",
      lwd = 1.5,linetype = 1 ) +
    
    geom_point(
      data = data.plot[data.plot$Unit == "Treat.Obs",],
      aes(x = Time, y = Value), colour = "black",size = 3 ) +
    
    geom_point(
      data = data.plot[data.plot$Unit == "Treat.Hat",],
      aes(x = Time, y = Value), colour = "black",size = 3 ) +
    
    geom_vline(xintercept = intervention_Time,linetype = "longdash") +
    
    ggtitle("Synthetic Control Method\nSimulation") +
    
    annotate(
      "text", label = paste0("Intervention \n  =  ",intervention_Time),
      x = intervention_Time, y = (0.05 + min(data.plot$Value)),
      size = 4, colour = "red") +
    theme(legend.position = "none")
  return(plot)
  
}

#Test
# data.plot.test <- data.scm.test$data.scm
# plot.scm(data.plot.test, intervention_Time=10)

##----------------------------------------------------------------------------------------------------#
##====================================================================================================##








##====================================================================================================##
# TEST (All)

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
# data.scm.test
# data.plot.test <- data.scm.test$data.scm
# plot.scm(data.plot.test, intervention_Time = 15)
# 




