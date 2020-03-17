#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kermack-McKendrick (SIR) epidemiological model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            withMathJax(),
            p("This applet simulates the Kermack-McKendrick epidemic model. It contains three states: S (the susceptible population), 
              I (the infected population) and R (the recovered population)."),
            p("The differential equation is given below."),
            p("$$\\begin{align}
                 \\ \\frac{dS}{dt} &= -rSI \\\\
                 \\ \\frac{dI}{dt} &= +rSI - aI \\\\
                 \\ \\frac{dR}{dt} &= aI
                 \\end{align}$$"),
            p("And the parameter values can be controlled in the menu below:"),
            sliderInput("a",
                        "Recovery rate (a):",
                        min = 0,
                        max = 1,
                        value = 0.25),
            sliderInput("r",
                        "Infection rate (r):",
                        min = 0,
                        max = 1.5,
                        value = 1.0,
                        step = 0.1),
            p("This simulation was initialized with $$(S_0, I_0, R_0) = (0.99, 0.01, 0.00)$$")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("sirPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    source('auxs.R')

    output$sirPlot <- renderPlot({
        # Solve the problem numerically
        ts <- seq(0, 100, by = 0.1)
        y0 <- c(S = 0.99, I = 0.01)
        sol <- SIR(ts, y0, parms = c(r = input$r, a = input$a))
        
        # Plot the results
        sol <- melt(sol, id = c("time"))
        ggplot(sol) + geom_line(aes(x=time, y=value, colour=variable), size = 2) +
            scale_colour_manual(values=c("blue", "red", "green")) + 
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
