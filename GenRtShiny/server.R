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

library(yardstick)
library(tidyr)
library(caret)
library(plotly)
library(e1071)

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

scales <- c(
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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    globalValues <- reactiveValues(data = generate_config(NPOP),
                              start=T,
                              start2=T,
                              model_not_built=T,
                              accuracies = c()
    )
    
    # observeEvent(globalValues$start, {
    #     
    #     globalValues$start=F
    #     showModal(modalDialog(
    #         title = "Rate art works",
    #         "Welcome to GenRt! To start rating the art just use the right or left arrow on your keyboard.
    #         Use the right arrow if you like what you see and the left arrow if you don't like it!",
    #         easyClose = T
    #     ))
    #     
    #     globalValues$responses = c()
    #     #showNotification("Press left or right to start!")
    # })
    trigger <- reactive({
        if(length(input$keys)==0){
            T
        }else{
            input$keys
        }
    })
    

    ##### KEY OBSERVER 
    # this section is executed when one of the keys is pressed
    observeEvent(trigger(), {
        idx <- which(globalValues$data$id == currentID())
        if(!globalValues$start2){
            if (input$keys == "right") {
                globalValues$data[idx, "score"] <- 1
                globalValues$responses = c(globalValues$responses, 1)
            } else{
                globalValues$data[idx, "score"] <- 0
                globalValues$responses = c(globalValues$responses, 0)
            }
            
            globalValues$data[idx, "rated"] <- T
        }
        globalValues$start2=F
            
        if(length(which(globalValues$data$score==1))>0){
            new_art <- crossover(globalValues$data %>%filter(score==1),id=max(globalValues$data$id)+1)
            globalValues$data <- rbind(globalValues$data,new_art)
        }

        if (!is.na(randomForestModel()[1])) {
            
            idx_not_rated = which(globalValues$data$rated==FALSE)
            
            pred <- predict(randomForestModel(),
                            globalValues$data[idx_not_rated,],
                            type = "raw")
            globalValues$data[idx_not_rated,]$pred_score <- as.numeric(pred)-1
            globalValues$data$pred_score <- as.factor(globalValues$data$pred_score)
            
            #adding accuracy
            accuracy = globalValues$data %>%
                filter(rated==T)%>%
                mutate(hits = score==pred_score)%>%
                summarise(accuracy = sum(hits,na.rm=T)/n())%>%
                select(accuracy)%>%
                unlist()
            globalValues$accuracies <- c(globalValues$accuracies,accuracy) 
        }
    })
    
    ##### CONFIGURATION #####
    # this reactive event is choosing the configuation that is needed for the art work
    configure <- eventReactive(trigger(), {
        like <- which(globalValues$data$pred_score == 1)
        
        if (nrow(modeldata()) > 5 & length(like) > 0 & runif(1)>0.3) {
            row <- sample_n(globalValues$data %>% filter(pred_score == 1),
                            1,
                            replace = T)
        } else{
            row <- sample_n(globalValues$data , 1, replace = T)
        }
        row <- sample_n(globalValues$data , 1, replace = T)
        row
    })
    
    
    currentID <- reactive({
        configure()$id
    })
    

    # this data will be used for the model
    modeldata <- reactive({
        modeldata <- globalValues$data %>%
            filter(rated == T)
        
        modeldata$score <- as.factor(modeldata$score)
        modeldata
    })
    
    ##### MODEL CREATION #####
    # in this section the model is created
    randomForestModel <- eventReactive(input$keys, {
        
        sampled <-  upSample(x = modeldata(),
                             y = modeldata()$score)%>%
            select(!Class)%>%
            select(-c("pred_score","id","rated"))
        
        if (nrow(modeldata()) > 5 &
            length(unique(sampled$score))>1
            ) {
            if(globalValues$model_not_built){
                
                showNotification("Now there is enough data and a model has been created.
                                 Check out the 'Model' section to see how your model looks like!")
                globalValues$model_not_built=F
            }
            model <- train(score ~ ., 
                           data = sampled
                           )
            model
        } else{
            NA
        }
        
    })
    
    
    ##### ART
    
    # output of the plot to be rated 
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
    
    
    #output of the created plot
    
    output$artPlotCreated <- renderPlot({

        gendata <-
            generate_data(
                input$N,
                input$ngroup,
                input$xmean,
                input$xvar,
                input$ymean,
                input$yvar,
                input$zmean,
                input$zvar
            )
        
        geom = input$geom==c("col",
                             "tile",
                             "area",
                             "point",
                             "spoke",
                             "line")
        plot <-
            generate_plot(
                gendata,
                
                colorscale = which(scales ==input$colorscale),
                col = geom[1],
                tile = geom[2],
                area = geom[3],
                point = geom[4],
                line = geom[5],
                spoke = geom[6],
                size = input$size,
                alpha = input$alpha,
                polar = input$polar
            )
        plot
    }, height = 900)

    ##### STATISTICS
    # the data set
    output$dataset <- DT::renderDataTable(globalValues$data,
                                          selection = 'single',
                                          options=list(scrollX=T,
                                                       selected = 1))
    
    output$selectedArtPlot <- renderPlot({
        req(input$dataset_rows_selected)
        conf <- globalValues$data[input$dataset_rows_selected,]
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
    })
    
    
    
    
    output$boxplots_ngroups <- renderPlotly({
        p <- globalValues$data %>%
            filter(rated==T)%>%
            pivot_longer(cols = c(
                "ngroups",
                "N",
                "xmean",
                "xvar",
                "ymean",
                "yvar",
                "zmean",
                "zvar",
                "size",
                "alpha"
            ))%>%
            mutate(score=as.factor(score))%>%
        ggplot(aes(x=score,y=value,fill=score))+
            geom_boxplot()+
            scale_fill_discrete()+
            facet_wrap(vars(name),scales = "free")+
            labs(title="Other variables",
                 x = "Score",
                 y = "Value"
                 )+
            theme_bw()
        ggplotly(p)
    })
    
    
    output$barchart_ngroups <- renderPlotly({
        p <-globalValues$data %>%
            filter(rated==T)%>%
          
            pivot_longer(cols =  c("col",
                         "tile",
                         "area",
                         "point",
                         "spoke",
                         "line",
                         "polar"
                         ))%>%
            mutate(score=as.factor(score))%>%
            group_by(score,name)%>%
            summarise(value = sum(value,na.rm=T))%>%
            ggplot(aes(x=value,y=name,fill=score))+
            geom_bar(stat="identity", width=1) +
            scale_fill_discrete()+
            labs(
                title= "Geometrics",
                x = "Count",
                y = "Geometric"
            )+
            theme_bw()
        ggplotly(p)
    })
    output$barchart_colorscale <- renderPlotly({
        p <- globalValues$data %>%
            filter(rated==T)%>%
            mutate(score=as.factor(score),
                   colorscale=as.factor(colorscale)
                   )%>%
            ggplot(aes(x=colorscale,fill=score))+
            geom_bar() +
            scale_fill_discrete()+
            labs(
                title= "Colorscale",
                x = "Colorscale",
                y = "Count"
            )+
            theme_bw()
        ggplotly(p)
        
    })
    
    
    
    output$responses <- renderPlot({
        resp <- data.frame(n =seq(1,length(globalValues$responses)), resp = globalValues$responses)
        
        
        ggplot(aes(n,resp),data = resp)+
            geom_line()+
            theme_bw()
        
    })
    
    output$accs <- renderPlot({
        
        accs <- data.frame(n =c(rep(NA,5),seq(1,length(globalValues$accuracies))), acc = c(rep(NA,5),globalValues$accuracies))
        
        
        ggplot(aes(n,acc),data = accs)+
            geom_line()+
            theme_bw()
        
    })
    
    
    # Model Evalutation
    
    output$plot_vimp <- renderPlot({
        req(randomForestModel())
        model <- randomForestModel()
        
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
        req(randomForestModel())
        if (!is.na(randomForestModel())) {
            pred <-
                as.factor(modeldata()$pred_score)#predict(randomForestModel(),modeldata(),type="class")
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
    
    
})
