library(shiny)	
library(tidyverse)	
library(dplyr)	
student_por <- read_delim("student-por.csv")	
# Define UI for application that draws a histogram	
ui <- fluidPage(	
  
  ## KAYLEE'S SECTION	
  navbarPage("Student dataset",	
             tabsetPanel(type = "tabs",	
                         tabPanel("Introduction", 	
                                  sidebarPanel(	
                                    h4("Sources"),	
                                    p("My group and I found the data set published on Kaggle, a platform designed for data scientists and machine learning practices to publish data. However, the data set originates from UC Irvine Machine Learning Repository and was collected by Prof. Paulo Cortez at the Department of Information Systems.")	
                                  ),	
                                  mainPanel(	
                                    h3("Purpose and Importance"),	
                                    p("The data set describes the academic performance of students at two Portuguese schools. More specifically, the data sets measures student performance in two distinct categories: mathematics and Portuguese language. The sampling specifically targets two secondary schools: Gabriel Periera (GP) and Mousinho da Silveira (MS)."),	
                                    img(src="student-performance2.jpg", height = 300, width = 400, alt="Student Performance Image 2"),	
                                    img(src="student-performance.jpg", height = 300, width = 400, alt="Student Performance Image 1")        	
                                  )	
                         ),	
                         tabPanel("Plot",	
                                  h4("Does parent's job influence student's academic performance? [Mjob: mother's job | Fjob: father's job]"),	
                                  sidebarLayout(	
                                    sidebarPanel(	
                                      radioButtons("student_age", "Select the age group:",	
                                                   choices = unique(student_por$age),	
                                                   selected = 15)	
                                    ),	
                                    mainPanel(plotOutput("kayplot"), plotOutput("kayplot2"), textOutput("jobcount"), tableOutput("eachjob"))	
                                  )	
                         ),	
                         tabPanel("Plot",	
                                  sidebarLayout(	
                                    sidebarPanel(	
                                      p("Here is a bar plot on how extra support relates to student's average grades and their health. You can select age group you are interested in and see what it looks like."),	
                                      column(6,	
                                             radioButtons("age", "Choose age",	
                                                          choices = c("15", "16", "17", "18", "19", "20", "21", "22"))	
                                      ),	
                                      column(6,	
                                             radioButtons("color1", "Palette",	
                                                          choices = c("skyblue", "green", "red", "purple", "gold")),	
                                             radioButtons("color2", "Palette",	
                                                          choices = c("skyblue", "green", "red", "purple", "gold"))	
                                      )	
                                    ),	
                                    mainPanel(	
                                      plotOutput("leoplot"),	
                                      textOutput("leoplotText")	
                                    )	
                                  )	
                         ), 	
                         
                         ## SARAH'S SECTION	
                         tabPanel("Academic Performance Based on Grade",	
                                  sidebarLayout(	
                                    sidebarPanel(	
                                      checkboxGroupInput("study", "Select how much the student studies:", 	
                                                         choices = c("1 = less than 2 hours" = 1,	
                                                                     "2 = 2 to 5 hours" = 2,	
                                                                     "3 = 3 to 5 hours" = 3,	
                                                                     "4 = greater than 10 hours" = 4),	
                                                         selected = 1)	
                                    ),	
                                    mainPanel(	
                                      plotOutput("sarahplot")	
                                    )	
                                  )	
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
                                            checkboxGroupInput("sex", "Choose sex",
                                                               choices = unique(student_por$sex),
                                                               selected = "F")
                                          ),
                                          mainPanel(
                                            plotOutput("eseelplot")
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
       ) 
# Define server logic required to draw a histogram
server <- function(input, output) {

## KAYLEE'S SECTION
  selection <- reactive ({
    student_por %>% 
      filter(student_age %in% input$age)
  })
  output$kayplot <- renderPlot ({
    # Plot for 'student grade vs. (Mjob)'
    avg_grade <- selection() %>% 
      mutate(avg = (G1+G2+G3)/n())
    ggplot(avg_grade, aes(Mjob, avg)) +
      geom_col()
  })
  output$kayplot2 <- renderPlot ({
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

##LEO'S SECTION
  })
  ageSample <- reactive({
    sexDiff <- student_por %>%
      filter(age %in% input$age)
  })
  
  output$leoplot <- renderPlot({
    ageSample() %>% 
      group_by(health, schoolsup) %>% 
      summarize(avgGrades = mean(G1 + G2 + G3)/3) %>% 
      ggplot(aes(health, avgGrades, fill = schoolsup)) + 
      geom_col(position = "dodge") + 
      labs(title = "Health vs. Average Grades with and without Academic Support",
           x = "Health", y = "Average Grades", fill = "Extra Support") +
      scale_fill_manual(values = c(input$color1, input$color2))
  })
  
  output$leoplotText <- renderText({
    paste("This is the how ", input$age," years old students perform
            and their health with and without extra school support")
  })
  
##SARAH'S SECTION
    sample <- reactive({
      student_por %>% 
        filter(studytime %in% input$study)
    })
    
    output$sarahplot <- renderPlot ({
      data <- sample() %>% 
        group_by(age, studytime) %>% 
        summarise(avg_grade = mean(G1 + G2 + G3)/3) 
      ggplot(data, aes(age,avg_grade, col=factor(studytime))) +
        geom_point() + 
        geom_line() +
        ylim(5, 20) +
        scale_x_continuous(breaks = 15:22) +
        labs(title = "The Effect of Study Time on Average Grade by Age", x = "age", y = "Average Grade")
    })
    
## ESEEL SECTION
    
    eseelsample <- reactive({
      student_por %>% 
        filter(sex %in% input$sex)
    })
    
    output$eseelplot <- renderPlot ({
      eseeldata <- eseelsample() %>% 
        group_by(age, sex) %>% 
        summarise(avg_grd = mean(G1 + G2 + G3)/3) 
      ggplot(eseeldata, aes(age,avg_grd, col=factor(sex))) +
        geom_point() + 
        geom_line() +
        labs(x = "Age", y = "Average Grade", title = "Age vs Average Grade by Sex",
             col = "Sex")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
