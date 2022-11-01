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
   titlePanel("Rescorla Wagner Model Simulation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("lambda",
                    "lambda: Reward value of US (0 = US not presented)",
                    
                    min = 0,
                    max = 1,
                    value = 1),
        sliderInput("alpha1",
                    "alpha1: Salience of CS1 (0 = CS1 not presented)",
                    min = 0,
                    max = 1,
                    value = .5),
        sliderInput("alpha2",
                    "alpha2: Salience of CS2 (0 = CS2 not presented)",
                    min = 0,
                    max = 1,
                    value = 0),
        sliderInput("beta",
                    "beta: Learning rate for US",
                    min = 0,
                    max = 1,
                    value = .3),
        actionButton("nextTrial", "Press to run next learning trial")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("rwPlot"),
         HTML("Imagine a version of the Pavlov experiment in which a tone (CS1 - conditioned stimulus #1) and a light (CS2 - conditioned stimulus #2) are both presented right before the dog is given food (US - unconditioned stimulus).  The association strength on the Y axis represents the amount of response to a stimulus; you could think of it as the amount of salivation.  Lambda (the green line) is the strength of the response to the US - the amount of salivation resulting from the food.  This represents the UR (unconditioned response) and is the maximum total associative strength possible.  CR1 (the red line) is the conditioned response to CS1 - the amount of salivation resulting from the tone.  CR2 (the blue line) is the conditioned response to CS2 - the amount of salivation resulting from the light.")
      )
   )
)

# define function to update RW association values on each trial:
# from http://compcogscisydney.org/psyr/programming.html
update_RW <- function(value, alpha=.3, beta=.3, lambda=1) {
  value_compound <- sum(value)                    # value of the compound 
  prediction_error <- lambda - value_compound     # prediction error
  value_change <- alpha * beta * prediction_error # change in strength
  value <- value + value_change                   # update value
  return(value)
}

server <- function(input, output) {
  values <- reactiveValues(rwValues = matrix(rep(0, 2), nrow = 1, ncol = 2))
  observeEvent(input$nextTrial,  {
    trial <- nrow(values$rwValues)
    newValues <- update_RW(values$rwValues[trial,], alpha=c(input$alpha1, input$alpha2), beta=input$beta, lambda=input$lambda)
    values$rwValues <- rbind(values$rwValues, newValues)     
  })
  output$rwPlot <- renderPlot({
    plot(values$rwValues[,1], type="o", pch="X", col="red", ylab="Association Strength", 
         xlab="Trial",   ylim=c(-1,1), 
         main="Response strength for the US (green line), CS1 (red), and CS2 (blue) on each learning trial ")
    lines(values$rwValues[,2], type="b", col="blue")
    abline(h=input$lambda, col = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

