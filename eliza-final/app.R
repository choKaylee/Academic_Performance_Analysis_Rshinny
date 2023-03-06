library(shiny)
library(tidyverse)

student <- read_delim("student-por.csv")

ui <- fluidPage(
    titlePanel("Eliza Seelenfreund Sections, Intro/Conclusion,
               free/study/out time correlation"),
    tabsetPanel(type = "tabs",
                tabPanel("Introduction", 
                ),
                tabPanel("Time Spent Correlation",
                         sidebarLayout(
                           sidebarPanel(
                             
                           ),
                           mainPanel(
                             
                           )
                         )
                ),
                tabPanel("Conclusion",
                )
    )
)

server <- function(input, output) {
  
  
}
shinyApp(ui = ui, server = server)
