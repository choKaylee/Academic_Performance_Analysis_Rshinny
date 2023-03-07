library(shiny)
library(tidyverse)

student <- read_delim("student-por.csv")

ui <- fluidPage(
    titlePanel("Eliza Seelenfreund, Intro/Conclusion"),
    tabsetPanel(type = "tabs",
                tabPanel("Introduction", 
                         sidebarPanel(
                           h4("Sources"),
                           p("My group and I found the data set published on Kaggle, 
                             a platform designed for data scientists and machine learning
                             practices to publish data. However, the data set originates
                             from UC Irvine Machine Learning Repository and was collected
                             by Prof. Paulo Cortez at the Department of Information Systems."),
                         ),
                         mainPanel(
                           h3("Purpose and Importance"),
                           p("The data set describes the academic performance of students at two 
                           Portuguese schools. More specifically, the data sets measures student 
                           performance in two distinct categories: mathematics and Portuguese language. 
                           The sampling specifically targets two secondary schools: Gabriel Periera (GP) 
                           and Mousinho da Silveira (MS)."),
                         ),
                         img(src="student-performance2.jpg", height = 300, width = 400,
                             alt="Student Performance Image 2"),
                         img(src="student-performance.jpg", height = 300, width = 400,
                             alt="Student Performance Image 1"),
                ),
                tabPanel("Conclusion",
                )
    )
)

server <- function(input, output) {
}
shinyApp(ui = ui, server = server)
