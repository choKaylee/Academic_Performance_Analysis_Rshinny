library(shiny)	
library(tidyverse)	
library(dplyr)	
student_por <- read_delim("student-por.csv")	
# Define UI for application that draws a histogram	
ui <- fluidPage(	
  
  ## KAYLEE'S SECTION	
  navbarPage("INFO 201: Student Academic Performance",	
             ##The overview tab - (ESEEL)
             tabsetPanel(type = "tabs",	
                         tabPanel("Project Overview", 	
                                  sidebarPanel(	
                                    h4("Sources"),	
                                    HTML("We will be working with the <a href= 'https://www.kaggle.com/datasets/larsen0966/student-performance-data-set'> Student Performance Data Set </a> from the UC 
                                      Irvine Department of Information Systems published on Kaggle.
                                      The data set describes the academic performance of students at two Portuguese schools. More specifically, the data sets measure student 
                                      performance in two distinct categories: mathematics and Portuguese language. The sampling specifically targets two secondary schools: 
                                      Gabriel Periera (GP) and Mousinho da Silveira (MS).")	
                                  ),	
                                  mainPanel(	
                                    h3("Purpose and Importance"),	
                                    p("Education is undeniably one of the most important pillars in a child's life. Despite the fact
                                    that current literature recognizes that student academic success is often the determining factor 
                                    in positive life outcomes, the variables that indicate student success are widely disagreed upon. With the
                                    final results, we hope to build upon existing work by identifying the relationship between different variables and student grades."),	
                                    br(),
                                    p("The audience is interested in seeing how various factors, both inside and outside of the home, affect student performance. 
                                    Understanding these various factors in isolation will demonstrate which ones are indicators of academic performance. Given that 
                                    the dataset revolves around student performance, the audiences interested in the dataset include students, parents, educators, 
                                    and school administrators."),
                                    img(src="student-performance2.jpg", height = 300, width = 400, alt="Student Performance Image 2"),	
                                    img(src="student-performance.jpg", height = 300, width = 400, alt="Student Performance Image 1")        	
                                  )	
                         ),	
                         ##Creating the tabpanel for the parent's occuaption plot - (KAYLEE)
                         tabPanel("Parent's Occupation",	
                                  h4("Does parent's job influence student's academic performance? [Mjob: mother's job | Fjob: father's job]"),	
                                  sidebarLayout(	
                                    sidebarPanel(	
                                      radioButtons("age", "Select the age group:",	
                                                   choices = c(15, 16, 17, 18, 19, 20, 21, 22),	
                                                   selected = 15)	
                                    ),	
                                    mainPanel(plotOutput("kayplot"), plotOutput("kayplot2"), textOutput("jobcount"), tableOutput("eachjob"),
                                              br(),
                                              p("The bar plot illustrates the average grades of students grouped by the occupation of their parents. 
                                              The occupation catatories include: teacher, health care related, civil services, at home, and other. 
                                              Based on the plot results two conclusions can be draw. First, students in the “Others” category always
                                              had the maximum average value in student academic performance within the age range of 15 to 22. 
                                              Secondly, the “Health” category was actually always the lowest average score or on the 4th pace from the lowest."),
                                              br())	
                                  )	
                         ),	
                         ##Creating the tabpanel for the student health and extra support plot - (LEO)
                         tabPanel("Student Health",	
                                  sidebarLayout(	
                                    sidebarPanel(	
                                      p("Here is a bar plot on how extra support relates to student's average grades and their health. You can select age group you are interested in and see what it looks like."),	
                                      column(6,	
                                             radioButtons("age1", "Choose age",	
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
                                      textOutput("leoplotText"),
                                      br(),
                                      p("The bar plot measures 3 variables: student average grade, extra support received, and health. 
                                      The plot demonstrates the average grade of students catagorized by ages, along with the corresponding health
                                      states and addtional educational support. The graph indicates that students within the age range of 15 to 22 
                                      who received extra support were not doing as well in classes and the extra support does not seem to affect whether
                                      the students will have better or poor health.")
                                    )	
                                  )	
                         ), 	
                         
                         ##Creating the plot measuring study time - (SARAH)
                         tabPanel("Study Time",	
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
                                      plotOutput("sarahplot"),
                                      textOutput("sarahplotText"),
                                      
                                      fluidRow(
                                        br(),
                                        p("The line plot illustrates the average grade of students grouped by 
                                      their age and the amount of time they dedicate to studying. As age increases, 
                                      it becomes clear that students that study for 3 (5 - 10 hrs) or 4 (>10 hrs) consistently 
                                      score higher than students who study less.")
                                      )
                                    )	
                                  )	
                         ), 	
                         
                         ##Final Takeaways - (ESEEL)
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
                                  column(6,
                                         h3("Insight Table"),
                                         sidebarPanel(
                                           fluidRow(
                                             column(6,
                                                    uiOutput("checkboxSex")
                                             )
                                           )
                                         ),
                                         mainPanel(
                                           tableOutput("leotable"),
                                           textOutput("leotext")
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
  
  ## KAYLEE'S SECTION - plot of parent's occupation
  selection <- reactive ({
    student_por %>% 
      filter(age %in% input$age)
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
  ##interactive text
  output$jobcount <- renderText ({
    count_total <- count(selection())
    paste("students in age of", input$age, "has", (count_total - 1), "other students in the same age")
  })
  
  output$eachjob <- renderTable ({
    total_count <- selection() %>% 
      summarize(Mjob_count = n_distinct(Mjob), Fjob_count = n_distinct(Fjob)) 
  })
  
  
  ##LEO'S SECTION - plot of student health 
  ageSample <- reactive({
    student_por %>%
      filter(age %in% input$age1)
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
  
  ##interactive text
  output$leoplotText <- renderText({
    paste("This is the how ", input$age," years old students perform
            and their health with and without extra school support")
  })
  
  ##Data Table in the conclusion
  output$checkboxSex <- renderUI({
    checkboxGroupInput("sex1", "Choose sex",
                       choices = unique(student_por$sex)
    )
  })
  
  sexSample <- reactive({
    student_por %>%
      filter(sex %in% input$sex1)
  })
  
  output$leotable <- renderTable({
    sexSample() %>% 
      group_by(sex, age) %>% 
      summarize(avgGrade = mean(G1 + G2 + G3)/3)
  })
  
  output$leotext <- renderText({
    if(nrow(sexSample()) == 0)
      "Please select gender"
    else
      paste("Table of ", str_flatten(input$sex, " and "), ", their
              respective ages and average grades per period, for a
              total of three periods.")
  })
  
  ##SARAH'S SECTION - plot of study time 
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
  ##interactive text
  output$sarahplotText <- renderText({
    paste("The line chart displays the mean grades achieved by students belonging to different age groups who have recieved a",
          input$study, "for study time.")
  })
  
  ## ESEEL SECTION - additional plot of student sex
  
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