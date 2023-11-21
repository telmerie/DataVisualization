#install packages
#install.packages("shiny")
#install.packages("DT")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("mapview")
#install.packages("leaflet")

#import libraries
library(shiny)
library(DT)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)



#import dataset
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
data <- read.csv("Earthquakes.csv", header=TRUE, sep=",")


#world_map <-  mapview(data, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)





#UI
ui <- navbarPage("Earthquakes from 1900 - 2013",
                 tabPanel("Dataset",
                          mainPanel(
                            DT::dataTableOutput("data")
                          )
                          ),
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
                          )
                 )


#Server
server <- function(input, output){
  
  output$data = DT::renderDataTable({data})
  #renderLeaflet(mapview::mapview2leaflet(data, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)) 

  
 
  
  output$mapplot <- renderLeaflet({
    years <-  input$years 
    
    quakeYear <- dplyr::filter(data, grepl(years , Date))
    
   
    m <- mapview(quakeYear, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)
    m@map
  })
  
  

}
  
#Create app
shinyApp(ui, server)