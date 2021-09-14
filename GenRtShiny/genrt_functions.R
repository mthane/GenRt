scales <-         c(
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

generate_data <- function(N,ngroup,xmean,xvar,ymean,yvar,zmean,zvar){
  DAT=data.frame()
  
  names=paste("G_",seq(1,ngroup),sep="")
  DAT=data.frame()
  for(i in seq(1:ngroup)){
    data=data.frame( matrix(0, ngroup , 3))
    data[,1]=sample(names, nrow(data))
    data[,2]=sample( c(rnorm(N,mean = xmean,sd = xvar),c(1:ngroup)) ,nrow(data))
    data[,3]=sample( c(rnorm(N,mean=ymean,sd = yvar),c(1:ngroup)) ,nrow(data))
    data[,4]=sample( c(rnorm(N,mean=zmean,sd = zvar),c(1:ngroup)) ,nrow(data))
    DAT=rbind(DAT,data)
  }
  colnames(DAT)=c("Group","X","Y","Z")
  DAT
}


generate_plot <- function(DAT,colorscale=1,size=5,alpha=0.5,geom=1,polar=F){
  colorscale = scales[colorscale]
  p <- ggplot() +
    
    scale_color_brewer(palette = colorscale) +
    scale_fill_brewer(palette = colorscale) +
    #geom_col()+
    #theme_bw()# +
    theme(
      line = element_blank(),
      text = element_blank(),
      title = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  
  if (geom == 1) {
    p <- p + geom_col(
      aes(
        x = X,
        y = Y,
        color = Group,
        group = Group
      ),
      size = size,
      alpha = alpha,
      data = DAT
    )
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

generate_config <- function(NPOP){
  data.frame(
    list(
      id = seq(1, NPOP),
      geom = sample(1:8, NPOP, replace = T),
      ngroups = sample(2:30, NPOP, replace = T),
      N = sample(150:1500, NPOP, replace = T),
      xmean = sample(1:5, NPOP, replace = T),
      ymean = sample(1:5, NPOP, replace = T),
      zmean = sample(1:5, NPOP, replace = T),
      xvar = sample(1:50, NPOP, replace = T),
      yvar = sample(1:50, NPOP, replace = T),
      zvar = sample(1:50, NPOP, replace = T),
      alpha = runif(NPOP, 0, 1),
      polar = sample(c(T, F), NPOP, replace = T),
      size = sample(seq(1, 100, 1), NPOP, replace = T),
      ncolor = sample(2:8, NPOP, replace = T),
      colorscale =sample(1:14,
        NPOP,
        replace = T
      ),
      score = rep(NA, NPOP),
      pred_score = rep(NA, NPOP),
      rated = rep(F, NPOP)
    )
  )
}