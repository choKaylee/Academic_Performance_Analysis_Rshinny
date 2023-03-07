#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)

student_por <- read_delim("student-por.csv")
student_por


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Student dataset",
  tabPanel("About the dataset", 
           h1("About the Student Performance Dataset"),
           p("The dataset describes the academic performance of students at two portuguese schools."),
           p("It measures sutdent performance in two distinct categories: mathematics and portuguese language"),
           p("This dataset was originated from UC Irvine Machine Learning Repository."),
           p("Data was collected by:",
           strong("prof. Paulo Cortez")),
           mainPanel(tableOutput("student"))),
  
  tabPanel("Plot",
           h4("Does parent's job influence student's academic performance? [Mjob: mother's job | Fjob: father's job]"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("age", "Select the age group:",
                            choices = unique(student_por$age),
                            selected = 15)
               ),
             mainPanel(plotOutput("plot"), plotOutput("plot2"), textOutput("jobcount"), tableOutput("eachjob")))),
  tabPanel("Table",
           h4("Avg grade of the students by gender"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("sex", "Select the gender:",
                            choices = unique(student_por$sex),
                            selected = F)
               ),
             mainPanel(tableOutput("grades"), textOutput("sexcount")))),
  )
)
## text

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$student <- renderTable ({
        # Table for 'About the dataset' page
        student_por %>% 
        head(10)
    })
    selection <- reactive ({
      student_por %>% 
        filter(age %in% input$age)
    })
    output$plot <- renderPlot ({
      # Plot for 'student grade vs. (Mjob)'
      avg_grade <- selection() %>% 
        mutate(avg = (G1+G2+G3)/n())
      ggplot(avg_grade, aes(Mjob, avg)) +
        geom_col()
    })
    output$plot2 <- renderPlot ({
      # Plot for 'student grade vs. (Fjob)'
      grade_avg <- selection() %>% 
        mutate(avg = (G1+G2+G3)/n())
      ggplot(grade_avg, aes(Fjob, avg)) +
        geom_col()
    })
    output$jobcount <- renderText ({
      count_total <- count(selection())
      paste("students in age of", input$age, "has", (count_total - 1), "other students in the same age")
    })
    output$eachjob <- renderTable ({
      total_count <- selection() %>% 
        summarize(Mjob_count = n_distinct(Mjob), Fjob_count = n_distinct(Fjob)) 
    })
    output$grades <- renderTable ({
      # Table for 'avg grade by each gender' page
      avg_grade <- selected() %>% 
        select(G1, G2, G3,sex) %>% 
        mutate(avg = (G1+G2+G3)/n()) %>% 
        summarize(sex, avg, avg_grade_by_gender = (sum(avg)/n()) * 100)
    })
    selected <- reactive ({
      student_por %>% 
        filter(sex %in% input$sex)
    })
    output$sexcount <- renderText ({
      total_count <- count(selected())
      paste("There are", total_count, "amount of", input$sex, "students in this dataset. [F stands for female and M stands for male.]")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

