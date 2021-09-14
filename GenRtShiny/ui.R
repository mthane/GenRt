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
library(bslib)
library(visNetwork)
# Define UI for application that draws a histogram



hotkeys <- c(
    "left", 
    "right"
)

shinyUI(
    
    dashboardPage(
        skin="black",
        dashboardHeader(title = "GenRt"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "home"),
                menuItem("Explore", tabName = "explore"),
                
                menuItem("Model", tabName = "model")
            )
            
        ),
        dashboardBody(
            useKeys(),
            keysInput("keys", hotkeys),
            tabItems(
                tabItem("home",
                        fluidRow(
                            uiOutput("confirmUI"),
                            plotOutput("artPlot"),
                        )
                        
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
                ),
                tabItem("explore",
                        
                        fluidRow(
                            plotOutput("responses"),
                            plotOutput("histogram_ngroups")
                            
                        )
                        )
            )
            
            
            
            
            
            
            
        )
        
        
    )
    
)
