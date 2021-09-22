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

hotkeys <- c("left",
             "right")

shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(title = "GenRt"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "GenRt",
            menuSubItem('Rate', tabName = 'rate'),
            menuSubItem('Create', tabName = 'create')
        ),
        menuItem("Statistics",
                 tabName = "stats"),
        menuItem("About",
                 tabName = "about")
        
    )),
    dashboardBody(
        useKeys(),
        keysInput("keys", hotkeys),
        tabItems(tabItem(
            "rate",
            fluidRow(uiOutput("confirmUI"),
                     plotOutput("artPlot"),)
            
        ),
        
        tabItem(
            "stats",
            tabsetPanel(
                type = 'tabs',
                tabPanel("Data",
                         DTOutput("dataset")),
                tabPanel("Distributions",
                         
                         
                         plotOutput("histogram_ngroups")),
                tabPanel("Progression",
                         
                         plotOutput("responses"),),
                tabPanel("Model",
                         fluidRow(
                             column(6,
                                    
                                    plotOutput("confusionmatrix")),
                             column(6,
                                    plotOutput("plot_vimp")),
                             
                             textOutput("artID"),
                             
                         ))

            )

        ))
    )
    
    
))
