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
library(shinyWidgets)

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
                     plotOutput("artPlot"))
            
        ),
        tabItem(
            "create",
            fluidRow(
                column(4,
                       sliderInput("ngroup","Number of groups",2,50,5),
                       sliderInput("N","N",50,2000,1000),

                       sliderInput("xmean","x mean",1,5,3),
                       sliderInput("ymean","y mean",1,5,3),
                       sliderInput("zmean","z mean",1,5,3),

                       sliderInput("xvar","x variance",1,5,3),
                       sliderInput("yvar","y variance",1,5,3),
                       sliderInput("zvar","z variance",1,5,3),
                       checkboxGroupInput("geom",
                                          "Geometrics",
                                          choices = c("col",
                                                      "tile",
                                                      "area",
                                                      "point",
                                                      "spoke",
                                                      "line"
                                                      )),
                       checkboxInput("polar","Polar coordinates",F),
                
                       sliderInput("size","Size",1,30,5),

                       sliderInput("alpha","Opacity",0.05,1,0.5),
                       numericInput("ncolor","Number of colors",2,8),
                       selectInput("colorscale","Color scale",
                                   choices = 
                                       c(
                           "Set1",
                           "Set2",
                           "Set3",
                           "Pastel1",
                           "Pastel2",
                           "Paired",
                           "Dark2",
                           "Accent",
                           "Blues",
                           "Greys",
                           "BuGn",
                           "Reds",
                           "Oranges",
                           "Greens"
                       )
                       )

                       ),
                column(8,
                       plotOutput("artPlotCreated",height=900)
                       )

            )

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
                         plotOutput("responses")),
                tabPanel("Model",
                         fluidRow(
                             column(6,
                                    
                                    plotOutput("confusionmatrix")),
                             column(6,
                                    plotOutput("plot_vimp"))
                             
                             
                         ))

            )

        ))
    )
    
    
))
