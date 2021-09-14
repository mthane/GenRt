#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("genrt_functions.R")
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(dplyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(visNetwork)
library(yardstick)

NPOP = 10000


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    observeEvent(rvalues$start, {
        showModal(modalDialog(
            title = "Rate art works",
            "Welcome to GenRt! To start rating the art just use the right or left arrow on your keyboard.
            Use the right arrow if you like what you see and the left arrow if you don't like it!",
            easyClose = T
        ))
        rvalues$start=F
        rvalues$responses = c()
        showNotification("Press left or right to start!")
    })
    


    rvalues <- reactiveValues(data = generate_config(NPOP),
                              start=T,
                              model_not_built=T
                              )
    
    observeEvent(input$keys , {

        idx <- which(rvalues$data$id == currentID())
        if (input$keys == "right") {
            rvalues$data[idx, "score"] <- 1
            
            rvalues$responses = c(rvalues$responses, 1)
        } else{
            rvalues$data[idx, "score"] <- 0
            
            rvalues$responses = c(rvalues$responses, 0)
        }
        rvalues$data[idx, "rated"] <- T
        if (!is.na(treeModel()[1])) {
            pred <- predict(treeModel(),
                            rvalues$data,
                            type = "class")
            rvalues$data$pred_score <- pred
            
        }
    })
    
    
    configure <- eventReactive(input$keys, {
        like <- which(rvalues$data$pred_score == 1)
        if (nrow(modeldata()) > 20 & length(like) > 0) {
            row <- sample_n(rvalues$data %>% filter(pred_score == 1),
                            1,
                            replace = T)
        } else{
            row <- sample_n(rvalues$data , 1, replace = T)
        }
        row
    })
    
    
    currentID <- reactive({
        configure()$id
    })
    
    
    modeldata <- reactive({
        modeldata <- rvalues$data %>%
            filter(rated == T)
        
        modeldata$score <- as.factor(modeldata$score)
        modeldata
    })
    
    treeModel <- eventReactive(input$keys, {
        
        if (nrow(modeldata()) > 20) {
            if(rvalues$model_not_built){
                
                showNotification("Now there is enough data and a model has been created. Check out the 'Model' section to see how your model looks like!")
                rvalues$model_not_built=F
            }
            tree <-
                rpart(score ~ ngroups + N + alpha + polar + size + ncolor + colorscale,
                      data = modeldata())
            
            tree
        } else{
            NA
        }
        
    })
    
    output$dataset <- DT::renderDataTable(rvalues$data)
    
    output$artID <- renderText(paste0("ID: ", currentID()))

    output$artPlot <- renderPlot({
        conf <- configure()
        gendata <-
            generate_data(
                conf$N,
                conf$ngroup,
                conf$xmean,
                conf$xvar,
                conf$ymean,
                conf$yvar,
                conf$zmean,
                conf$zvar
            )
        plot <-
            generate_plot(
                gendata,
                colorscale = conf$colorscale,
                size = conf$size,
                alpha = conf$alpha,
                geom = conf$geom,
                polar = conf$polar
            )
        plot
    }, height = 900)
    
    ##### Data Exploration
    
    
    output$responses <- renderPlot({
        resp <- data.frame(n =seq(1,length(rvalues$responses)), resp = rvalues$responses)

        
        ggplot(aes(n,resp),data = resp)+
            geom_line()+
            geom_smooth()+
            theme_bw()
        
    })
    
    output$histogram_ngroups <- renderPlot({
        rvalues$data %>%
            filter(rated==T)%>%
            mutate(score=as.factor(score))%>%
        ggplot(aes(x=ngroups,fill=score))+
            geom_histogram()+
            scale_fill_discrete()
        
    })
    
    
    ##### Model Evaluation 
    
    output$rfprint <- renderVisNetwork({
        if (!is.na(treeModel())) {
            visTree(treeModel())
        }
    })
    
    output$confusionmatrix <- renderPlot({
        if (!is.na(treeModel())) {
            pred <-
                as.factor(modeldata()$pred_score)#predict(treeModel(),modeldata(),type="class")
            ref <- modeldata()$score
            table <- data.frame(confusionMatrix(pred, ref)$table)
            
            plotTable <- table %>%
                mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
                group_by(Reference) %>%
                mutate(prop = Freq / sum(Freq))
            ggplot(
                data = plotTable,
                mapping = aes(
                    x = Reference,
                    y = Prediction,
                    fill = prop
                )
            ) +
                geom_tile() +
                geom_text(
                    aes(label = Freq),
                    vjust = .5,
                    fontface  = "bold",
                    alpha = 1
                ) +
                scale_fill_viridis_c() +
                theme_bw() +
                xlim(rev(levels(table$Reference)))
        }
    })
    
    output$accuracy <- renderPrint({
        if (!is.na(treeModel())) {
            pred <- predict(treeModel(), modeldata(), type = "class")
            print(confusionMatrix(pred, modeldata()$score)$overall[1])
        }
    })
    
    
})
