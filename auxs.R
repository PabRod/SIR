library(deSolve)
library(dplyr)
library(ggplot2)

dSIR <- function(time, y, parms = SIR_default_parms()) {
  
  with(as.list(c(y, parms)), {
    
    dS <- -r*S*I
    dI <- +r*S*I - a*I
    
    return(list(c(dS, dI)))
  })
  
}

SIR <- function(ts, y0, parms = SIR_default_parms(), ...) {
  
  # Solve
  sol <- ode(y = y0,
             func = dSIR,
             times = ts,
             parms = parms,
             ...)
  
  # Transform into data frame
  sol <- as.data.frame(sol)
  
  # Calculate the recovered
  sol <- mutate(sol, R = 1 - S - I)
  
}

SIR_default_parms <- function() {
  
  parms = c(r = 1,
            a = 0.25)
  
}