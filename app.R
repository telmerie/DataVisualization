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
  mutate(Mean = mean(mag),
         MeanDepth = mean(depth))

#Create a new table called "meanOfEarthquakes" where the mean for each year is shown
meanOfEarthquakes <- evolutionOfEarthquakes %>%
  group_by(Year, Mean) %>%
  summarise()

#Create a new table called "meanOfDepths" where the mean for each year is shown
meanOfDepths <- evolutionOfEarthquakes %>%
  group_by(Year, MeanDepth) %>%
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
                          titlePanel("ggPlot for magnitude"),
                          plotOutput("ggPlot"),
                          titlePanel("Histogram for magnitude"),
                          plotOutput("magnitude"),
                          titlePanel("Number of earthquakes each year"),
                          plotOutput("numberOfEarthquakesEachYear"),
                          titlePanel("depth of earthquake"),
                          plotOutput("depth"),
                          titlePanel("Scatterplot: Depth vs. Magnitude"),
                          plotOutput("scatterplotDepthMagnitude")
                 
                          ),
                 tabPanel("Report", tags$iframe(style = "height:600px; width:100%; scrolling=yes", src = "Report.pdf")),
                 )

#Server
server <- function(input, output){
  
  output$data = DT::renderDataTable({data})
  
  #Create "ggPlot"
  output$ggPlot <- renderPlot({
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
            main = "Mean of magnitudes of earthquakes each year",  
            col = ifelse(meanOfEarthquakes$Mean > 6.5, "#DC267F", "#FFB000"),
            width = 0.01)
    # Add legend (description of colors)
    legend("right", legend = c("Mean over 6.5", "Mean under 6.5"), fill = c("#DC267F", "#FFB000"))
  })
  
  #Create "Bar plot for depth"
  output$depth <- renderPlot({
    barplot(meanOfDepths$MeanDepth, 
            names.arg = meanOfDepths$Year,  
            xlab = "Year",        
            ylab = "Mean",        
            main = "Mean of depths of earthquakes each year",  
            col = "#DC267F",
            width = 0.01)
  })
  
  #Create histogram for "Number of earthquakes each year" 
  output$numberOfEarthquakesEachYear <- renderPlot({
    barplot(numberOfEarthquakes$Count, 
            names.arg = numberOfEarthquakes$Year,  
            xlab = "Year",        
            ylab = "Number of earthquakes",   
            main = "Number of earthquakes each year", 
            col = "#FFB000"
    )
  })
  output$scatterplotDepthMagnitude <- renderPlot({
    ggplot(data = evolutionOfEarthquakes, aes(x = Mean, y = MeanDepth)) +
      geom_point() +
      labs(x = "Mean Magnitude", y = "Mean Depth", title = "Scatterplot: Depth vs. Magnitude") +
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(color = "grey")) 
  })
}
  
#Create app
shinyApp(ui, server)