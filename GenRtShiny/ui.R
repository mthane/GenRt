#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(keys)
library(caret)

library(visNetwork)
# Define UI for application that draws a histogram



hotkeys <- c(
    "left", 
    "right"
)

shinyUI(
    
    dashboardPage(
        dashboardHeader(title = "Personalized Arts"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "home"),
                menuItem("Model", tabName = "model")
            )
            
        ),
        dashboardBody(
            useKeys(),
            keysInput("keys", hotkeys),
            tabItems(
                tabItem("home",
                        fluidRow(
                            
                            plotOutput("artPlot"),
                            
                            
                            
                        )#,
                        # wellPanel(
                        #     actionButton("shuffle","Shuffle")
                        # )
                        
                ),
                
                tabItem("model",
                        fluidRow(
                            
                            titlePanel("artRating"),    
                            
                            textOutput("accuracy"),    
                            plotOutput("confusionmatrix"),
                            visNetworkOutput("rfprint"),
                            textOutput("artID"),
                            DTOutput("dataset")
                            
                        )
                )
            )
            
            
            
            
            
            
            
        )
        
        
    )
    
)
