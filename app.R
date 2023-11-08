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
ui <- navbarPage("Group 14: Earthquakes from 1900 - 2013",
                 tabPanel("Data set",
                          mainPanel(
                            DT::dataTableOutput("data")
                          )
                 ),
                 tabPanel("Report", tags$iframe(style = "height:600px; width:100%; scrolling=yes", src = "Report.pdf"))
                 )


#Server
server <- function(input, output){
  
  output$data = DT::renderDataTable({data})
  
}
  
#Create app
shinyApp(ui, server)