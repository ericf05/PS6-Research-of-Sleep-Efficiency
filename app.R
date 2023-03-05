library(shiny)
library(tidyverse)
library(plotly)

data <- read.csv("Sleep_Efficiency.csv")

ui <- fluidPage(
  titlePanel("INFO 201 PS6: Research of Sleep Efficiency by Yixiang Fang"),
  mainPanel(
    tabsetPanel(
      tabPanel("Overview",
               p("The dataset was downloaded from ", strong("kaggle"), "."),
               p("The dataset contains information about a group of test subjects and their sleep patterns. 
          Each test subject is identified by a unique 'Subject ID' and their age and gender are also recorded. 
          The 'Bedtime' and 'Wakeup time' features indicate when each subject goes to bed and wakes up each day,
          and the 'Sleep duration' feature records the total amount of time each subject slept in hours. 
          The 'Sleep efficiency' feature is a measure of the proportion of time spent in bed that is actually spent asleep. 
          The 'REM sleep percentage', 'Deep sleep percentage', and 'Light sleep percentage' features indicate the 
          amount of time each subject spent in each stage of sleep. The 'Awakenings' feature records the number of 
          times each subject wakes up during the night. Additionally, the dataset includes information about each 
          subject's caffeine and alcohol consumption in the 24 hours prior to bedtime, their smoking status, 
          and their exercise frequency."),
               plotlyOutput("plot1")
      ),
      tabPanel("Plot",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("gender", "Select the gender:", choices = c("Both Gender", "Female", "Male"),
                               selected = "Both Gender"),
                   checkboxInput("trend", "Display Trend")
                 ),
                 mainPanel(
                   plotlyOutput("plot2"),
                   verbatimTextOutput("text")
                 )
               )         
      ),
      tabPanel("Tables",
               sidebarLayout(
                 sidebarPanel(
                   em("Calculate the average sleep efficiency by the group variable"),
                   selectInput("variable", "Select the group variable:", choices = c("Gender", "Smoking.status"),
                               selected = "Gender")
                 ),
                 mainPanel(
                   tableOutput("dt")
                 )
               )             
      )
    ),
    width=12
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    ggplot(data=data, aes(x=Sleep.efficiency)) + geom_histogram(color="white") +
      labs(title="Histogram of sleep efficiency")
  })
  
  output$plot2 <- renderPlotly({
    if(input$gender=="Both Gender"){
      df <- data
    }else if(input$gender=="Female"){
      df <- data[data$Gender=="Female",]
    }else{
      df <- data[data$Gender=="Male",]
    }
    p <- ggplot(df, aes(x=Deep.sleep.percentage,y=Sleep.efficiency)) + geom_point()
    if(input$trend){
      p <- p + geom_smooth(method="lm", se=F, color="red")
    }
    p
  })
  
  output$text <- renderText({
    if(input$gender=="Both Gender"){
      df <- data
    }else if(input$gender=="Female"){
      df <- data[data$Gender=="Female",]
    }else{
      df <- data[data$Gender=="Male",]
    }
    paste0("The number of observations is ", nrow(df), "\n")
  })
  
  output$dt <- renderTable({
    if(input$variable=="Gender"){
      data %>% 
        group_by(Gender) %>% 
        summarize(Avg.Sleep.efficiency=mean(Sleep.efficiency), n=n())
    }else{
      data %>% 
        group_by(Smoking.status) %>% 
        summarize(Avg.Sleep.efficiency=mean(Sleep.efficiency), n=n())
    }
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)