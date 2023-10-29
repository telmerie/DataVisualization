#install packages
install.packages("shiny")
install.packages("DT")

#import libraries
library(shiny)
library(DT)

#import dataset
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
data <- read.csv("Earthquakes.csv", header=TRUE, sep=",")


#UI
ui <- navbarPage("Earthquakes from 1900 - 2013",
                 tabPanel("Dataset",
                          mainPanel(
                            DT::dataTableOutput("data")
                          )
                          ),
                 tabPanel("two")
                 )


#Server
server <- function(input, output){
  
  output$data = DT::renderDataTable({data})
  
}
  
#Create app
shinyApp(ui, server)