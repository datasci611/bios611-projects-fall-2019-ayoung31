library(shiny)
library(tidyverse)
source("helper_functions.R")

raw <- read()
df <- clean(raw)
weeklyVals <- makeWeekly(df)
DailyVals <- makeDaily(df)
new <- weekpred()

ui = fluidPage(
  
   titlePanel("Predicted Values of Items by Day of the Week"),
  
   sidebarLayout(
     sidebarPanel(radioButtons("option1", h3("Choose value to predict:"),
                               choices = c("Pounds of Food", "Number of Clothes", "Proportion of Large Families"))),
     mainPanel(plotOutput(outputId = "dailyPlot"), dataTableOutput(outputId="dailyTab"))
   )
)
server <- function(input, output){
  
  output$dailyPlot <- renderPlot({
    plotDaily(DailyVals, input$option1, new)
  })
  output$dailyTab <- renderDataTable({
                                      predicted <- prediction(DailyVals, input$option1, new)
                                      predicted[,2] <- round(x = predicted[,2],digits = 2)
                                      predicted[,3] <- round(x = predicted[,3],digits = 2)
                                      predicted[,4] <- round(x = predicted[,4],digits = 2)
                                      predicted
                                     },
                     
                     options=list(
                     paging = FALSE,
                     searching = FALSE)
                     )
  
}

shinyApp(ui = ui, server = server)