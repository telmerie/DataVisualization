#install packages
install.packages("shiny")
install.packages("DT")
install.packages("tidyverse")
install.packages("sf")
install.packages("mapview")
install.packages("leaflet")

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

world_map <-  mapview(data, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)





#UI
ui <- navbarPage("Earthquakes from 1900 - 2013",
                 tabPanel("Dataset",
                          mainPanel(
                            DT::dataTableOutput("data")
                          )
                          ),
                 tabPanel("two",
                          mainPanel(
                            
                          )
                          
  
                        
                          )
                 )


#Server
server <- function(input, output){
  
  output$data = DT::renderDataTable({data})
  renderLeaflet(mapview::mapview2leaflet(data, xcol = "longitude", ycol = "latitude",crs = 4269, grid = FALSE)) 
  
}
  
#Create app
shinyApp(ui, server)