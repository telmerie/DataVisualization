#install packages
install.packages("shiny")
install.packages("DT")

#import libraries
library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
#library(dplyr)

#import data set
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
data <- read.csv("Earthquakes.csv", header=TRUE, sep=",")

#remove all rows where the year is 2014
data <- subset(data, as.numeric(format(as.Date(Date), "%Y")) != 2014)


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
                          mainPanel(
                            DT::dataTableOutput("data")
                          )
                 ),
                 tabPanel("Evolution of Earthquakes",
                          titlePanel("Scatter plot for the mean of magnitudes"),
                          plotOutput("scatterPlot"),
                          titlePanel("Histogram for the mean of magnitudes"),
                          plotOutput("magnitude"),
                          titlePanel("Number of earthquakes each year"),
                          plotOutput("numberOfEarthquakesEachYear"),
                          titlePanel("Time series plot over the mean of magnitudes"),
                          plotOutput("timeSeriesPlot")
                          ),
                 tabPanel("Report", tags$iframe(style = "height:600px; width:100%; scrolling=yes", src = "Report.pdf")),
                 )
                 


#Server
server <- function(input, output){
  
  output$data = DT::renderDataTable({data})
  
  #Create "ggPlot"
  output$scatterPlot <- renderPlot({
    ggplot(data = evolutionOfEarthquakes, aes(x = Year, y = Mean)) + geom_point() +
    theme(panel.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(color = "grey")) 
      
  })
  
  #Create "Histogram for magnitude"
  output$magnitude <- renderPlot({
    barplot(meanOfEarthquakes$Mean, 
            names.arg = meanOfEarthquakes$Year,  
            xlab = "Year",        
            ylab = "Mean",        
            col = ifelse(meanOfEarthquakes$Mean > 6.5, "#DC267F", "#FFB000"),
            width = 0.01)
    # Add legend (description of colors)
    legend("right", legend = c("Mean over 6.5", "Mean under 6.5"), fill = c("#DC267F", "#FFB000"))
  })
  
  #Create histogram for "Number of earthquakes each year" 
  output$numberOfEarthquakesEachYear <- renderPlot({
    barplot(numberOfEarthquakes$Count, 
            names.arg = numberOfEarthquakes$Year,  
            xlab = "Year",        
            ylab = "Number of earthquakes",   
            col = "#FFB000"
    )
  })
  
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