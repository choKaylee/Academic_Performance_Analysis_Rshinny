# I was helped by Sarah Tran

library(shiny)
library(tidyverse)

sp <- read_delim("student-por.csv")

ui <- fluidPage(
    fluidRow(navbarPage("Academic Performance for Students from 2 Schools in Portugese",
      tabPanel("Home", 
              p("This app uses student academic performance at two Portuguese
              schools data published on Kaggle, measuring student performance 
              in two distinct categories: ", 
              em("Math and Portuguese,"), 
              " from two secondary schools: ",
              strong("Gabriel Periera (GP) and Mousinho da Silveira (MS)."), 
              "The data set originates from UC Irvine Machine Learning 
              Repository and was collected by Prof. Paulo Cortez at the 
              Department of Information Systems."),
              cat("\n"),
              p("There are ", nrow(sp), "observations and ", 
                ncol(sp), "variables in the data set."),
              cat("\n"),
              p("Here is a small random sample of data:"),
              mainPanel(
                tableOutput("sample")
              )),
      tabPanel("Plot",
               sidebarLayout(
                 sidebarPanel(
                   p("Here is a bar plot on how extra support relates to 
                   student's average grades and their health. You can select
                     age group you are interested in and see what it looks
                     like."),
                   column(6,
                          radioButtons("age", "Choose age",
                                       choices = c("15", "16", "17",
                                                    "18", "19", "20",
                                                      "21", "22"))
                   ),
                   column(6,
                          radioButtons("color1", "Palette",
                                       choices = c("skyblue", "green", "red",
                                                            "purple", "gold")),
                          radioButtons("color2", "Palette",
                                       choices = c("skyblue", "green", "red",
                                                            "purple", "gold"))
                   )
                 ),
                 mainPanel(
                   plotOutput("plot"),
                   textOutput("plotText")
                 )
               )), 
      tabPanel("Table", 
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                     column(6,
                            uiOutput("checkboxSex")
                     )
                   )
                ),
                mainPanel(
                  tableOutput("table"),
                  textOutput("text")
                ))
               )
             )
    )
)

server <- function(input, output) {
    
    output$sample <- renderTable({
        sp %>% 
          select(sex, age, G1, G2, G3) %>% 
          sample_n(5)
    })
    
    
    
    ageSample <- reactive({
      sexDiff <- sp %>%
        filter(age %in% input$age)
    })
    
    output$plot <- renderPlot({
      ageSample() %>% 
        group_by(health, schoolsup) %>% 
        summarize(avgGrades = mean(G1 + G2 + G3)/3) %>% 
        ggplot(aes(health, avgGrades, fill = schoolsup)) + 
        geom_col(position = "dodge") + 
        labs(title = "Health vs. Average Grades with and without Academic Support",
             x = "Health", y = "Average Grades", fill = "Extra Support") +
        scale_fill_manual(values = c(input$color1, input$color2))
    })
    
    output$plotText <- renderText({
      paste("This is the how ", input$age," years old students perform
            and their health with and without extra school support")
    })
    
    
    
    output$checkboxSex <- renderUI({
      checkboxGroupInput("sex", "Choose sex",
                         choices = unique(sp$sex)
      )
    })
    
    sample <- reactive({
      sexDiff <- sp %>%
        filter(sex %in% input$sex)
    })
    
    output$table <- renderTable({
      sample() %>% 
        group_by(sex, age) %>% 
        summarize(avgGrade = mean(G1 + G2 + G3)/3)
    })
    
    output$text <- renderText({
      if(nrow(sample()) == 0)
        "Please select gender"
      else
        paste("Table of ", str_flatten(input$sex, " and "), ", their
              respective ages and average grades per period, for a
              total of three periods.")
    })
}

shinyApp(ui = ui, server = server)
