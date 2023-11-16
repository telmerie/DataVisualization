#install packages
install.packages("shiny")
install.packages("DT")

#import libraries
library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)

#import data set
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
data <- read.csv("Earthquakes.csv", header=TRUE, sep=",")

#remove all rows where the year is 2014
data <- subset(data, as.numeric(format(as.Date(Date), "%Y")) != 2014)
#set row counter to 0 after rows from 2014 were removed
rownames(data) <- NULL
#remove unused column "X"
data <- data[, !(names(data) == "X")]

#Edit tables for "Evolution of earthquakes"
#Create a new column called "Year" where the date is only a year
evolutionOfEarthquakes <- data %>% mutate(
  Year = as.integer(format(as.Date(Date), "%Y"))
)

#Calculate the mean of each year
evolutionOfEarthquakes <- evolutionOfEarthquakes %>% 
  group_by(Year) %>% 
  mutate(Mean = mean(mag))

#Create a new table called "meanOfEarthquakes" where the mean for each year is shown
meanOfEarthquakes <- evolutionOfEarthquakes %>%
  group_by(Year, Mean) %>%
  summarise()

#Create a new column in the data set "evolutionOfEarthquakes". The new column contains the number of earthquakes each year
evolutionOfEarthquakes <- evolutionOfEarthquakes %>%
  group_by(Year) %>%
  mutate(Count = n())

#Creates new table called "numberOfEarthquakes" where the number of earthquakes each year is shown 
numberOfEarthquakes <- evolutionOfEarthquakes %>%
  group_by(Year, Count) %>%
  summarise()


#UI
ui <- navbarPage("Group 14: Earthquakes from 1900 - 2013",
                 tabPanel("Data set",
                          titlePanel("Data set"),
                          mainPanel(
                            DT::dataTableOutput("data")
                          )
                 ),
                 tabPanel("Evolution of Earthquakes",
                          titlePanel("Number of earthquakes each year"),
                          plotOutput("numberOfEarthquakesEachYear"),
                          titlePanel("Scatter plot for the mean of magnitudes"),
                          plotOutput("scatterPlot"),
                          titlePanel("Time series plot over the mean of magnitudes"),
                          plotOutput("timeSeriesPlot"),
                          titlePanel("Histogram for the mean of magnitudes"),
                          plotOutput("magnitude")
                          ),
                 tabPanel("Report", tags$iframe(style = "height:600px; width:100%; scrolling=yes", src = "Report.pdf")),
                 )
                 


#Server
server <- function(input, output){
  
  output$data <- DT::renderDataTable({
    datatable(data, options = list(pageLength = 10)) %>%
      formatStyle('Date', whiteSpace = 'nowrap') %>%
      formatStyle('updated', whiteSpace = 'nowrap')
  })
  
  #create "ggPlot"
  output$scatterPlot <- renderPlot({
    ggplot(data = evolutionOfEarthquakes, aes(x = Year, y = Mean)) + geom_point() +
    theme(panel.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(color = "grey")) 
      
  })
  
  #create "Histogram for magnitude"
  output$magnitude <- renderPlot({
    barplot(meanOfEarthquakes$Mean, 
            names.arg = meanOfEarthquakes$Year,  
            xlab = "Year",        
            ylab = "Mean",        
            col = ifelse(meanOfEarthquakes$Mean > 6.5, "#DC267F", "#FFB000"),
            width = 0.01)
    #add legend (description of colors)
    legend("right", legend = c("Mean over 6.5", "Mean under 6.5"), fill = c("#DC267F", "#FFB000"))
  })
  
  #create histogram for "Number of earthquakes each year" 
  output$numberOfEarthquakesEachYear <- renderPlot({
    barplot(numberOfEarthquakes$Count, 
            names.arg = numberOfEarthquakes$Year,  
            xlab = "Year",        
            ylab = "Number of earthquakes",   
            col = "#FFB000"
    )
  })
  
  #create time series plot over "Mean of magnitudes"
  output$timeSeriesPlot <- renderPlot({
  ggplot(data = evolutionOfEarthquakes, aes(x = Year, y = Mean)) +
    geom_line() +
    labs(
         x = "Year",
         y = "Mean Value") +
      scale_x_continuous(breaks = seq(min(evolutionOfEarthquakes$Year), max(evolutionOfEarthquakes$Year), by = 6))
  })
  
}
  
#Create app
shinyApp(ui, server)