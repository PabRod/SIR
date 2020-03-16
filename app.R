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
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("a",
                        "Constant a:",
                        min = 0,
                        max = 1,
                        value = 0.25),
            sliderInput("r",
                        "Constant r:",
                        min = 0,
                        max = 4,
                        value = 1.0,
                        step = 0.1),
            sliderInput("S0",
                        "Initial S:",
                        min = 0,
                        max = 1,
                        value = 0.99,
                        step = 0.01)
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
        y0 <- c(S = input$S0, I = 1 - input$S0)
        sol <- SIR(ts, y0, parms = c(r = input$r, a = input$a))
        
        # Plot the results
        ggplot(sol, aes(x=ts)) +
            geom_line(aes(y = S), color = "blue") + 
            geom_line(aes(y = I), color="red") +
            geom_line(aes(y = R), color = "green")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
