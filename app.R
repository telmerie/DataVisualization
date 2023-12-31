#install packages
# install.packages("shiny")
# install.packages("DT")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("lubridate")

# library(conflicted)  

#import libraries
# library(shiny)
library(DT)
library(tidyverse)
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
# library(sf)
library(ggplot2)
library(leaflet)
library(lubridate)


#import data set
# path <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(path)
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

#Create new table called "numberOfEarthquakes" where the number of earthquakes each year is shown 
numberOfEarthquakes <- evolutionOfEarthquakes %>%
  group_by(Year, Count) %>%
  summarise()

#Create a new table called "seasonOfEarthquakes" and copy existing columns from data
seasonOfEarthquakes <- data

#Add a new column called "month" to seasonOfEarthquakes
seasonOfEarthquakes$Month <- month(data$Date)

#Add a new column called "season" to seasonOfEarthquakes
seasonOfEarthquakes$season <- ifelse(seasonOfEarthquakes$Month %in% c(12, 1, 2), "Winter",
                                     ifelse(seasonOfEarthquakes$Month %in% c(3, 4, 5), "Spring",
                                            ifelse(seasonOfEarthquakes$Month %in% c(6, 7, 8), "Summer",
                                                   ifelse(seasonOfEarthquakes$Month %in% c(9, 10, 11), "Autumn", NA))))

#Count occurrences of each season in seasonOfEarthquakes
season_counts <- table(seasonOfEarthquakes$season)

#Create a new dataframe with the season counts
season_counts_df <- data.frame(Season = names(season_counts), Count = as.numeric(season_counts))







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
                          titlePanel("Barchart over the mean of magnitudes"),
                          plotOutput("magnitude")
                 ),
                 tabPanel("World Map",
                          fluidRow(
                            leafletOutput("mapplot","100%"),
                          ),
                          sliderInput("years",
                                      "Year:",
                                      min = 1990, 
                                      max = 2013,
                                      value = 2000,
                                      step = 1,
                                      width = '100%',
                                      sep = "",
                                      animate =  animationOptions(interval = 300, loop = TRUE,playButton = icon("play", "fa-1,5x"), pauseButton = icon("pause", "fa-1,5x"))
                          )
                 ),
                 tabPanel("Consistency between Earthquakes",
                          titlePanel("Depth of earthquake"),
                          plotOutput("depth"),
                          titlePanel("Scatterplot: Depth vs. Magnitude"),
                          plotOutput("scatterplotDepthMagnitude")
                 ),
                 tabPanel("Season of Earthquakes",
                          titlePanel("Number of earthquakes each season"),
                          plotOutput("season", width = "800px", height = "600px")),
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
  
  #create "Barchart for magnitude"
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
  
  #create barchart for "Number of earthquakes each year" 
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
  

  m <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 1) %>%
    setMaxBounds(lng1 = -150, lat1 = -80, lng2 = 150, lat2 = 80)
  
  output$mapplot <- renderLeaflet({
    m
  })
  
  observeEvent(input$years, {
    years <- input$years
    quakeYear <- dplyr::filter(data, grepl(years, Date))
    
    leafletProxy("mapplot") %>%
      clearMarkers() %>%
      addCircleMarkers(data = quakeYear, lng = ~longitude, lat = ~latitude,
                       radius = 3)
  })
  
  #pie_color <- c("#FFB000", "#DC267F", "#785EF0", "#648FFF")
  
  output$season <- renderPlot({
    # Set the color
    pie_color <- c("#FFB000", "#DC267F", "#785EF0", "#648FFF")
    
    # Calculate percentages
    percentages <- round((season_counts_df$Count / sum(season_counts_df$Count)) * 100, 1)
    
    # Create a pie chart with the specified colors
    pie(season_counts_df$Count, 
        labels = paste(season_counts_df$Season, percentages, "%"), 
        col = pie_color)
    
    # Add a legend with months for each season, using Unicode space
    legend("topright", 
           legend = paste(season_counts_df$Season, "months:", 
                          c("Sep, Oct, Nov", "Mar, Apr, May", "Jun, Jul, Aug", "Dec, Jan, Feb")), 
           fill = pie_color, title = "Season", 
           text.font = 1)  # Use bold text for separation
  })
  
  
  
  
  
  
}
  
#Create app
shinyApp(ui, server)