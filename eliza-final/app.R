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
                         fluidRow(
                           column(4,
                             h3("Notable Insights"),
                             p("The data set that we choose has many elements. Because of the extensive
                               amount of variables, it is difficult to draw strong conclusions. Overall there
                               are some correlations between average grade and outside support. The students
                               that recieved outside suport seemed to have lower overall grades. This is
                               suprising seeing that extra support is meant to help kids recieve higher remarks 
                               on their assignments. Another insight was based on the average grades of students
                               compared to their gender. Overall females on average were recieving better grades
                               than the males. This was especially evident in the younger groups. Also, as predicted,
                               students that study more than others tend to have overall higher average grades."),
                             h3("Broader Implications"),
                             p("The implications of these insights are that, educators may want to look further
                               into resources for students that are looking for the extra help. The students that
                               were getting help should in theory have higher grades than the students that are 
                               not reaching out. Therefore there may need to be revisions on how students are
                               being assisted when they ask for help.")
                           ),
                           column(8,
                             h3("Pattern Insight"),
                             sidebarPanel(
                               uiOutput("gender")
                             ),
                             mainPanel(
                               plotOutput("plot")
                             )
                           )
                         ),
                         fluidRow(
                           column(6,
                             h3("Data quality"),
                             p("Overall, the data quality of this set was clear and easy to work with. 
                               The only downside of the data set was that there were almost too many variables
                               for a small team to be able to evaluate them all and find coclusive evidence 
                               of correlations between the variables.")
                           ),
                           column(6,
                             h3("Future Ideas"),
                             p("This research could provide helpful insight into how schools
                               and childrens lives affect their grades. In order to fully evaluate this, we
                               would need to evaluate each of the variables. In the future, this data set should
                               be assesed thurougly to provide helpful insight into students lives.")
                           )
                           
                         )
                )
    )
)

server <- function(input, output) {
  output$gender <- renderUI({
    checkboxGroupInput("sex", "Choose sex",
                       choices = unique(student$sex)
    )
  })
  
  sample <- reactive({
    student %>%
      filter(sex %in% input$sex)
  })
  
  output$plot <- renderPlot({
    sample() %>% 
      group_by(sex, age) %>% 
      summarize(avgGrades = mean(G1 + G2 + G3)/3) %>%
      ggplot(aes(age, avgGrades)) +
      geom_line(aes(col = factor(sex))) +
      labs(x = "Age", y = "Average Grade", title = "Age vs Average Grade by Sex",
           col = "Sex")
  })
  
}
shinyApp(ui = ui, server = server)
