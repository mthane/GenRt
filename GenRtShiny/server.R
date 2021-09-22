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
library(tidyr)
library(caret)
NPOP = 1000

COLUMNS = c(
    "ngroups",
    "N",
    "xmean",
    "xvar",
    "ymean",
    "yvar",
    "zmean",
    "zvar",
    "col",
    "tile",
    "area",
    "point",
    "spoke",
    "line",
    "polar",
    "size",
    "alpha",
    "ncolor",
    "colorscale"
    
)




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
                              start2=T,
                              model_not_built=T
                              )
    
    observeEvent(input$keys , {

        idx <- which(rvalues$data$id == currentID())
        if(!rvalues$start2){
            if (input$keys == "right") {
                rvalues$data[idx, "score"] <- 1
                rvalues$responses = c(rvalues$responses, 1)
            } else{
                rvalues$data[idx, "score"] <- 0
                rvalues$responses = c(rvalues$responses, 0)
            }
            
            rvalues$data[idx, "rated"] <- T
        }
        rvalues$start2=F

        if(length(which(rvalues$data$score==1))>0){
            new_art <- crossover(rvalues$data %>%filter(score==1),id=max(rvalues$data$id)+1)
            rvalues$data <- rbind(rvalues$data,new_art)
        }
        
        # if(length(which(rvalues$data$pred_score==0))>0){
        #     
        #     rvalues$data <- rvalues$data[-which(rvalues$data$spred_score==0)[1]]
        # }
        
        if (!is.na(treeModel()[1])) {
            
            idx_not_rated = which(rvalues$data$rated==FALSE)
            
            pred <- predict(treeModel(),
                            rvalues$data[idx_not_rated,],
                            type = "raw")
            print(pred)
            
            rvalues$data[idx_not_rated,]$pred_score <- as.numeric(pred)-1
            rvalues$data$pred_score <- as.factor(rvalues$data$pred_score)
            print(rvalues$data[idx_not_rated,]$pred_score)
        }
    })
    
    
    configure <- eventReactive(input$keys, {
        like <- which(rvalues$data$pred_score == 1)
        
        if (nrow(modeldata()) > 5 & length(like) > 0 & runif(1)>0.3) {
            row <- sample_n(rvalues$data %>% filter(pred_score == 1),
                            1,
                            replace = T)
        } else{
            row <- sample_n(rvalues$data , 1, replace = T)
        }
        row <- sample_n(rvalues$data , 1, replace = T)
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
        
        sampled <-  upSample(x = modeldata(),
                             y = modeldata()$score)%>%
            select(!Class)%>%
            select(-c("pred_score","id","rated"))
        
        if (nrow(modeldata()) > 5 &
            length(unique(sampled$score))>1 &
            length(rvalues$data$responses)%%10==0
            ) {
            if(rvalues$model_not_built){
                
                showNotification("Now there is enough data and a model has been created.
                                 Check out the 'Model' section to see how your model looks like!")
                rvalues$model_not_built=F
            }
            print("update model")
            
            # train_control <- trainControl(method = "repeatedcv",  
            #                               number = 5,
            #                               repeats = 3)
            model <- train(score ~ ., 
                           data = sampled
                           )
            model
        } else{
            NA
        }
        
    })
    
    output$dataset <- DT::renderDataTable(rvalues$data,
                                          options=list(scrollX=T))
    
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
                col = conf$col,
                tile = conf$tile,
                area = conf$area,
                point = conf$point,
                line = conf$line,
                spoke = conf$spoke,
                size = conf$size,
                alpha = conf$alpha,
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
            pivot_longer(cols = COLUMNS)%>%
            mutate(score=as.factor(score))%>%
        ggplot(aes(y=value,fill=score))+
            geom_boxplot()+
            scale_fill_discrete()+
            facet_wrap(vars(name),scales = "free")
        
    })
    
    
    ##### Model Evaluation 
    

    
    output$plot_vimp <- renderPlot({
        req(treeModel())
        model <- treeModel()
        
        vimp <- data.frame(imp = varImp(model)$importance,
                           label = rownames(varImp(model)$importance))%>%
            arrange(-Overall)
        
        colnames(vimp) = c("Importance", "Variable")
        ggplot(vimp,aes(x=Importance,y=reorder(Variable,Importance)))+
            #geom_point()+
            geom_bar(stat = "identity",aes(fill=Importance))+
            scale_fill_viridis_c()+
            ylab("Variable")+
            theme_classic()
    })
    
    
    output$confusionmatrix <- renderPlot({
        req(treeModel())
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
            pred <- predict(treeModel(), modeldata(), type = "raw")
            print(confusionMatrix(pred, modeldata()$score)$overall[1])
        }
    })
    
    
})
