#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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
    
    
    rvalues <- reactiveValues(
        
        data = data.frame(        
            list(id = seq(1,NPOP),
                 geom = sample(1:8,NPOP,replace=T),
                 ngroups = sample(2:30,NPOP,replace=T),
                 N = sample(150:1500,NPOP,replace=T),
                 xmean = sample(1:5,NPOP,replace=T),
                 ymean = sample(1:5,NPOP,replace=T),
                 zmean = sample(1:5,NPOP,replace=T),
                 xvar = sample(1:50,NPOP,replace=T),
                 yvar = sample(1:50,NPOP,replace=T),
                 zvar = sample(1:50,NPOP,replace=T),
                 
                 alpha =runif(NPOP,0,1),
                 polar = sample(c(T,F),NPOP,replace=T),
                 size = sample(seq(1,100,1),NPOP,replace=T),
                 ncolor= sample(2:8,NPOP,replace=T),
                 colorscale = sample(1:14,NPOP,replace=T),
                 score = rep(NA,NPOP),
                 pred_score = rep(NA,NPOP),
                 rated = rep(F,NPOP)
            ))
    )
    
    observeEvent(input$keys,{
        idx <- which(rvalues$data$id==currentID())
        
        if( input$keys=="right"){
            rvalues$data[idx,"score"] <- 1
        }else{
            rvalues$data[idx,"score"] <- 0
        }
        
        rvalues$data[idx,"rated"] <- T
        if(!is.na(treeModel()[1])){
            pred <-predict(treeModel(),
                           rvalues$data,
                           type = "class"
            )
            rvalues$data$pred_score <- pred
            
        }
    })
    
    observeEvent(input$shuffle,{
        rvalues$data<-rvalues$data[sample(nrow(rvalues$data )),]
    })
    
    output$dataset <- DT::renderDataTable(rvalues$data)
    
    configure <- eventReactive(input$keys,{
        
        
        # preddata <- arrange(rvalues$data,-score)
        # 
        # # sample from top 10 % best scored data 80 % of the time
        # if(runif(1)>0.2){
        #     
        #     row <- sample_n(preddata[1:round(NPOP/10),],1,replace = T)
        # }else{
        #     row <- sample_n(preddata,1)
        # }
        like <- which(rvalues$data$pred_score==1)
        
        if(nrow(modeldata())>10 & length(like)>0){
            row <- sample_n(rvalues$data %>%filter(pred_score==1),1,replace=T)
        }else{
            row <- sample_n(rvalues$data ,1,replace=T)
        }
        row
    })
    
    
    currentID <- reactive({
        configure()$id
    })
    
    
    modeldata <- reactive({
        modeldata <- rvalues$data%>%
            filter(rated==T)
        
        modeldata$score <- as.factor(modeldata$score)
        modeldata
    })
    
    treeModel <- eventReactive(input$keys,{
        
        if(nrow(modeldata())>10){
            tree <- rpart(score~ngroups+N+alpha+polar+size+ncolor+colorscale,data=modeldata())
            
            tree
        }else{
            NA
        }
        
    })
    
    output$artID <- renderText(paste0("ID: ",currentID()))
    
    
    
    output$rfprint <- renderVisNetwork({
        if(!is.na(treeModel())){
            
            visTree(treeModel())
        }
    })
    
    output$confusionmatrix <- renderPlot({
        
        if(!is.na(treeModel())){
            pred <- as.factor(modeldata()$pred_score)#predict(treeModel(),modeldata(),type="class")
            ref <- modeldata()$score
            table <- data.frame(confusionMatrix(pred, ref)$table)
            
            plotTable <- table %>%
                mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
                group_by(Reference) %>%
                mutate(prop = Freq/sum(Freq))
            
            # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
            ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = prop)) +
                geom_tile() +
                geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
                scale_fill_viridis_c()+
                theme_bw() +
                xlim(rev(levels(table$Reference)))
        }
    })
    
    output$accuracy <- renderPrint({
        
        if(!is.na(treeModel())){
            pred <- predict(treeModel(),modeldata(),type="class")
            print(confusionMatrix(pred, modeldata()$score)$overall[1])
        }
    })
    
    output$artPlot <- renderPlot({
        plot_art(configure(),currentID())
        
    },height=900)
    
    
})


plot_art <- function(config,title){
    row <- config
    
    ngroup=row$ngroups
    N = row$N
    
    alpha=row$alpha
    polar = row$polar
    size = row$size
    ncolor=row$ncolor
    colorscale=row$colorscale
    print(size)
    # ps <- row %>%select(col,tile,area,line,point)
    # geom = which.max(ps)
    # 
    geom = row$geom
    names=paste("G_",seq(1,ngroup),sep="")
    DAT=data.frame()
    #set.seed(input$bins) #set the seed of R's random number generator
    library(ggplot2)
    library(RColorBrewer)
    # changes how many groups there are in the art graph
    names=paste("G_",seq(1,ngroup),sep="")
    DAT=data.frame()
    #creating dataframe 
    for(i in seq(1:ngroup)){ #must change if changed the ngroup from previous code block
        data=data.frame( matrix(0, ngroup , 3))
        
        data[,1]=sample(names, nrow(data))
        data[,2]=sample( c(rnorm(N,mean = row$xmean,sd = row$xvar),c(1:ngroup)) ,nrow(data))
        
        data[,3]=sample( c(rnorm(N,mean=row$ymean,sd = row$yvar),c(1:ngroup)) ,nrow(data))
        data[,4]=sample( c(rnorm(N,mean=row$zmean,sd = row$zvar),c(1:ngroup)) ,nrow(data))
        DAT=rbind(DAT,data)
    }
    colnames(DAT)=c("Group","X","Y","Z")
    
    scales = c("Set1","Set2","Set3","Pastel1","Pastel2","Paired","Dark2","Accent","Blues","Greys","BuGn","Reds","Oranges","Greens")
    
    coul = brewer.pal(ncolor,scales[colorscale])
    
    # coul = colorRampPalette(coul)(ngroup)
    # coul=sample(coul[sample(c(1:length(coul)) , size=length(coul) ) ],length(coul)) #deciding the color for the pallete
    print(geom)
    p<-ggplot() + 
        
        scale_color_brewer(palette = scales[colorscale])+
        scale_fill_brewer(palette = scales[colorscale])+
        #geom_col()+
        #theme_bw()# +
        theme(line = element_blank(),
              text = element_blank(),
              title = element_blank(),
              legend.position="none",
              panel.border = element_blank(),
              panel.background = element_blank())
    
    if(geom==1){
        p <- p+geom_col( aes(x=X,
                             y=Y,
                             
                             color=Group,
                             group=Group
        ), size=size,
        alpha=alpha,data = DAT)
    }  
    if(geom==2){
        p <- p+geom_tile( aes(x=X,
                              y=Y,
                              height = Z,
                              
                              color=Group,
                              group=Group,
                              width=size
        ), size=size,
        alpha=alpha,data = DAT)
    }  
    if(geom==3){
        p <- p+geom_boxplot( aes(x=X,
                                 y=Y,
                                 
                                 color=Group,
                                 group=Group
        ), size=size,
        alpha=alpha,data = DAT)
    }
    if(geom==4){
        p <- p+geom_violin( aes(x=Group,
                                y=Y,
                                
                                color=Group
        ),  size=size,
        alpha=alpha,data = DAT)
    }
    
    if(geom==5){
        p <- p+geom_area( aes(x=X,
                              y = Y,
                              
                              
                              color=Group,
                              group=Group
        ), size=size,alpha=alpha,data = DAT)
    }
    
    if(geom==6){
        p <- p+geom_point( aes(x=X,
                               y=Y,
                               
                               color=Group
        ),size=size,
        alpha=alpha,data = DAT)
    }
    
    if(geom==7){
        p <- p+geom_line( aes(x=X,
                              y=Y,
                              z=Z,
                              
                              color=Group,
                              group=Group
        ), size=size,
        alpha=alpha,data = DAT)
    }
    if(geom==8){
        p <- p+geom_bin2d( aes(x=X,
                               y=Y,
                               fill = Z
                               
        ),     
        alpha=alpha,size=size,data = DAT)
    }
    
    if(polar==T){
        p <- p+coord_polar()
    } 
    p
    
}