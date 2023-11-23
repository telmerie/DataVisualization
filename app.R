#install packages
install.packages("shiny")
install.packages("DT")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("sf")
install.packages("mapview")
install.packages("leaflet")
install.packages("lubridate")



#import libraries
library(shiny)
library(DT)
library(tidyverse)
library(sf)
library(ggplot2)
library(mapview)
library(leaflet)
library(lubridate)






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


#world_map <-  mapview(data, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)



# Assuming your date column is in character format, convert it to Date format
data$Date <- as.Date(data$Date)

# Create a new column called "month"
data$Month <- month(data$Date)

# Create a new column called "season"
data$season <- ifelse(data$Month %in% c(12, 1, 2), "Winter",
                      ifelse(data$Month %in% c(3, 4, 5), "Spring",
                             ifelse(data$Month %in% c(6, 7, 8), "Summer",
                                    ifelse(data$Month %in% c(9, 10, 11), "Autumn", NA))))

season_counts <- table(data$season)

# Create a new dataframe with the season counts
season_counts_df <- data.frame(Season = names(season_counts), Count = as.numeric(season_counts))

# Display the season counts dataframe
print(season_counts_df)

















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
                          titlePanel("Scatter plot over the mean of magnitudes"),
                          plotOutput("scatterPlot"),
                          titlePanel("Time series plot over the mean of magnitudes"),
                          plotOutput("timeSeriesPlot"),
                          titlePanel("Histogram over the mean of magnitudes"),
                          plotOutput("magnitude")
                 ),
                 tabPanel("Consistency between Earthquakes",
                          titlePanel("Depth of earthquake"),
                          plotOutput("depth"),
                          titlePanel("Scatterplot: Depth vs. Magnitude"),
                          plotOutput("scatterplotDepthMagnitude")
                 ),
                 tabPanel("Season of Earthquakes",
                          titlePanel("Number of earthquakes each season"),
                          plotOutput("season")),
                 tabPanel("World Map",
                          sliderInput("years",
                                      "Year:",
                                      min = 1990, 
                                      max = 2014,
                                      value = 2000,
                                      step = 1,
                                      animate = TRUE
                                      ),
                        
                          fluidRow(
                            leafletOutput("mapplot","100%"),
                          )
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
  
  #Create "Bar plot for depth"
  output$depth <- renderPlot({
    barplot(meanOfDepths$MeanDepth, 
            names.arg = meanOfDepths$Year,  
            xlab = "Year",        
            ylab = "Mean",        
            col = "#DC267F",
            width = 0.01)
  })

  output$scatterplotDepthMagnitude <- renderPlot({
    ggplot(data = evolutionOfEarthquakes, aes(x = Mean, y = MeanDepth)) +
      geom_point() +
      labs(x = "Mean Magnitude", y = "Mean Depth") +
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(color = "grey")) 
  })
  
  output$mapplot <- renderLeaflet({
    years <-  input$years 
    
    quakeYear <- dplyr::filter(data, grepl(years , Date))
    
   
    m <- mapview(quakeYear, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)
    m@map
  })
  
  output$season <- renderPlot({
    # Set the color
    bar_color <- "#FFB000"
    
    # Create a bar plot with the specified color
    barplot(season_counts_df$Count, 
            names.arg = season_counts_df$Season, 
            col = bar_color, 
            xlab = "Season", 
            ylab = "Number of earthquakes")
    
    # Define custom labels
    custom_labels <- c("December \nJanuary \nFebruary", 
                       "March \nApril \nMay", 
                       "June \nJuly \nAugust", 
                       "September \nOctober \nNovember")
    
    # Add custom labels inside each bar with the same color
    text(x = barplot(season_counts_df$Count, col = bar_color, add = TRUE), 
         y = season_counts_df$Count - 0.1 * max(season_counts_df$Count), 
         label = custom_labels,
         pos = 1, cex = 0.8, col = "black", font = 2)
  })
}
  
#Create app
shinyApp(ui, server)