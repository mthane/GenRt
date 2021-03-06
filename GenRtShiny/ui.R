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
library(knitr)
library(plotly)
hotkeys <- c("left",
             "right")

shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(title = "GenRt",
                    
                    dropdownMenu(
                        type = "notifications", 
                        icon = icon("question-circle"),
                        badgeStatus = NULL,
                        headerText = "See also:",
                        
                        notificationItem("GitHub", icon = icon("file"),
                                         href = "https://github.com/mthane/GenRt"),
                        notificationItem("RMarkdown Notebook", icon = icon("file"),
                                         href = "https://htmlpreview.github.io/?https://github.com/mthane/GenRt/blob/main/GenRtShiny/GenRt.html")
                    )
                   
                    ),
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
        tabItems(
            tabItem("rate",
                    fluidRow(
                        uiOutput("confirmUI"),
                        plotOutput("artPlot")
                    )),
            tabItem("create",
                    wellPanel(
                        fluidRow(
                            column(
                                2,
                                sliderInput("ngroup", "Number of groups", 2, 50, 5),
                                sliderInput("N", "N", 50, 2000, 1000),
                                
                                sliderInput("xmean", "x mean", 1, 5, 3),
                                sliderInput("ymean", "y mean", 1, 5, 3),
                                sliderInput("zmean", "z mean", 1, 5, 3),
                                
                                sliderInput("xvar", "x variance", 1, 5, 3),
                                sliderInput("yvar", "y variance", 1, 5, 3),
                                sliderInput("zvar", "z variance", 1, 5, 3))
                            ,
                            column(2,
                                   checkboxGroupInput(
                                       "geom",
                                       "Geometrics",
                                       choices = c("col",
                                                   "tile",
                                                   "area",
                                                   "point",
                                                   "spoke",
                                                   "line"),
                                       selected = c("col","area")
                                   ),
                                   checkboxInput("polar", "Polar coordinates", F),
                                   
                                   sliderInput("size", "Size", 1, 30, 5),
                                   
                                   sliderInput("alpha", "Opacity", 0.05, 1, 0.5),
                                   numericInput("ncolor", "Number of colors", 2, 8),
                                   selectInput(
                                       "colorscale",
                                       "Color scale",
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
                                   plotOutput("artPlotCreated", height = 900))
                            
                        )
                        
                    )

                    
                    ),
            tabItem(
                "stats",
                tabsetPanel(
                    type = 'tabs',
                    tabPanel("Data",
                             wellPanel(
                                 div("Select a row to see the art work! You can sort the table by clicking on the column."),
                                 DTOutput("dataset"),
                                 plotOutput("selectedArtPlot")
                             )
                             
                             ),
                    tabPanel("Distributions",
                             wellPanel(
                                 fluidRow(
                                     column(4,
                                            
                                            plotlyOutput("barchart_ngroups", height = 900)
                                     ),
                                     column(4,
                                            plotlyOutput("barchart_colorscale", height = 900)
                                     ),
                                     column(4,
                                            plotlyOutput("boxplots_ngroups", height = 900)
                                     )
                                 )
                             )

                             ),
                    tabPanel("Progression",
                             plotOutput("responses"),
                             
                             plotOutput("accs")
                             
                             ),
                    tabPanel("Model",
                             fluidRow(
                                 column(6,
                                        
                                        plotOutput("confusionmatrix")),
                                 column(6,
                                        plotOutput("plot_vimp"))
                                 
                                 
                             ))
                    
                )
                
            ),
            tabItem("about",
                    fluidRow(column(
                        6,
                        wellPanel(
                            h2("GenRt"),
                            div('For additional information on how the source code works check:'),
                            tags$ul(
                                tags$a(href = "https://github.com/mthane/GenRt", "GitHub"),
                                tags$a(href = "https://htmlpreview.github.io/?https://github.com/mthane/GenRt/blob/main/GenRtShiny/GenRt.html", "RMarkdown")
                                ),
                            h3("Generative art with ggplot2"),
                            div(
                                'There is a great potential for creating digital art by using R’s ggplot2 library and 
                                create visualizations that look like real art works. Mainly those approaches use data sets
                                which are based on mathematical formulas and can be manipulated as desired. 
                                My approach of creating digital art using R is quite similar. 
                                First I created a data set that is based on three random variables “X”, “Y” and “Z”
                                and one grouping variable “Group”. 
                                These distributions are based on a normal distribution with a given mean and standard deviation.
'
                            ),
                            div('Using this data one can now plot an “art work”. 
                            I was trying to find attributes that would make the art works look quite different,
                            but that can later also been used to find out which kind of art is preferred by the user.
                            To find these attributes I was playing around with the outcome a bit,
                            and it seemed that these attributes create various different visualizations. 
                            Some of these visualizations looked rather boring to me and some I enjoyed looking at.
                            These visualizations will be used for the recommendation in the next step.
                            The attributes are the following:'),
                            tags$ol(
                                tags$li('ngroups (Number of different groups)'),
                                tags$li('N (Number of samples)'),
                                tags$li('xmean, ymean, zmean (means of the distributions)'),
                                tags$li('xsd, ysd, zsd (standard deviations of the distributions)'),
                                tags$li('geom (what kind of geometric is used in the art work)'),
                                tags$li('alpha (opacity value)'),
                                tags$li('polar (using polar coordinate system)'),
                                tags$li('size (size of the geometric objects)'),
                                tags$li('colorscale (which color scale is being used)')
                            ),
                            hr(),
                            h3("How does it work?"),
                            div('
                                   The algorithm works by building a model of what the user likes and what not. 
                                   First I had the idea of making the algorithm like a genetic algorithm which creates crossovers of art works that are rated with 1 (like) and adds it to the population in each step.
                                   I feared that this idea has the disadvantage of landing in a sort of local optimum, where art works do not really change much over time.
                                   Another idea was to create a semisupervised learning method called self learning,
                                   where a learned model predicts the unrated images and will recommend art works that are predicted as a 1 (like).
                                   I endet up using a mixture of both ideas. 
                                   First of all a start population is being created and the user randomly gets to see some of the art works. After 5 rated art works a model is being created.
                                   The art works that are rated as 1 will perform a crossover operation which is simply an average value of all the features to create new art works.
                                   This new art works are then added to the unrated population in each step.
                                   The random forest predicts all the unrated art works and suggests the user one of the art works,
                                   that the classification algorithm rated as 1.
                                   '),
                            hr(),
                            tags$img(src='Recommendation.png'),
                        )
                    ),
                    column(6,
                           wellPanel(
                               

                               h3("Evaluation"),
                               h4("Statistics"),
                               div('
                               In the tab "Data" one can see a table of all the configurations that produce the art works. When you select a row than you can see the plot in the lower panel.
                               For seeing the difference between liked or unliked posts I created statistics to compare the distribution of several attributes. This way
                                   one can evaluate how different the preferences are. This can be found in the tab "Distributions". 
                                   Also it is possible to see the responses in the tap "Progression".
                                   Additionaly one can look at the model performance and the features importance of the random forest model.'),
                               
                               h4("User feedback"),
                               div('
                                   The main goal of this project was to find out how to use R in order to create aesthetic plots. My own impression was that some of the plots look quite aesthetic
                                   and others look too overcrowded.
                                   In order to evaluate this I showed the project to some of my friends and fellow students and asked for the feedback and if the recommendations become realistic.
                                   I did not perform a well defined user study, but my conclusion was that based on this feedback there was no clear improvement in the recommendations.
                                   The reason for this might be, that the start population is to large or there was simply not enough training data, but this would have to be investigated further.
                                   
                                  '),
                               h4("Future ideas"),
                               tags$ul(
                                   tags$li('Using libraries for generative code that are already there'),
                                   tags$li('Find a larger variety of features and different plots'),
                                   tags$li('Create a user study to find out what can be improved'),
                                   tags$li('Using a data base to capture the various user data in an anonymous way'),
                                   tags$li('Improve the model and try different ML methods'),
                                   tags$li('Capture the reaction times and find out how it effects the preference')
                               ),
                           )
                           
                           )
                    ))
        )
    )
    
    
))
