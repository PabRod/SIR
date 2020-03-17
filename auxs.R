library(deSolve)
library(dplyr)
library(ggplot2)
library(reshape2)

#' Kermack-McKendrick (SIR) model flow
#'
#' @param time The time
#' @param y The state
#' @param parms Model's parameters (optional)
#'
#' @return The flow (right hand side of the differential equation)
#'
#' @author Pablo Rodríguez-Sánchez (\url{https://pabrod.github.io})
#' @export
#'
#' @references
#' Murray, J.D
#' Mathematical biology
#' ISBN 978-0-387-22437-4
#' 
#' @examples
#' \dontrun{
#' t <- 0
#' y <- c(S = 0.99, I = 0.01, R = 0.00)
#' dy <- dSIR(t, y)
#' }
dSIR <- function(time, y, parms = SIR_default_parms()) {
  
  with(as.list(c(y, parms)), {
    
    dS <- -r*S*I
    dI <- +r*S*I - a*I
    # R can be calculated as total - S - I
    
    return(list(c(dS, dI)))
  })
  
}

#' Solve Kermack-McKendrick (SIR) model
#'
#' @param ts Vector of times
#' @param y0 Initial condition
#' @param parms Model's parameters (optional)
#'
#' @return Results of the simulation, including times and states
#'
#' @author Pablo Rodríguez-Sánchez (\url{https://pabrod.github.io})
#' @export
#'
#' @references
#' Murray, J.D
#' Mathematical biology
#' ISBN 978-0-387-22437-4
#' 
#' @examples
#' \dontrun{
#' ts <- seq(0, 100, by = 0.1)
#' y0 <- c(S = 0.99, I = 0.01, R = 0.00)
#' ys <- SIR(t, y)
#' }
SIR <- function(ts, y0, parms = SIR_default_parms(), ...) {
  
  # Solve
  sol <- ode(y = y0,
             func = dSIR,
             times = ts,
             parms = parms,
             ...)
  
  # Transform into data frame
  sol <- as.data.frame(sol)
  
  # Calculate the recovered as total - S - I, and append
  sol <- mutate(sol, R = 1 - S - I)
  
}

#' Default parameters for Kermack-McKendrick (SIR) model
#'
#' @return The default parameters for Kermack-McKendrick (SIR) model
#'
#' @export
#'
#' @author Pablo Rodríguez-Sánchez (\url{https://pabrod.github.io})
#'
#' @references
#' Murray, J.D
#' Mathematical biology
#' ISBN 978-0-387-22437-4
#' 
SIR_default_parms <- function() {
  
  parms = c(r = 1,
            a = 0.25)
  
}