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

generate_data <-
  function(N,
           ngroup,
           xmean,
           xvar,
           ymean,
           yvar,
           zmean,
           zvar) {
    DAT = data.frame()
    
    names = paste("G_", seq(1, ngroup), sep = "")
    DAT = data.frame()
    for (i in seq(1:ngroup)) {
      data = data.frame(matrix(0, ngroup , 3))
      data[, 1] = sample(names, nrow(data))
      data[, 2] = sample(c(rnorm(N, mean = xmean, sd = xvar), c(1:ngroup)) , nrow(data))
      data[, 3] = sample(c(rnorm(N, mean = ymean, sd = yvar), c(1:ngroup)) , nrow(data))
      data[, 4] = sample(c(rnorm(N, mean = zmean, sd = zvar), c(1:ngroup)) , nrow(data))
      DAT = rbind(DAT, data)
    }
    colnames(DAT) = c("Group", "X", "Y", "Z")
    DAT
  }



generate_plot <-
  function(DAT,
           colorscale = 1,
           col = T,
           tile = T,
           area = T,
           point = T,
           line = T,
           spoke = T,
           size = 5,
           alpha = 0.5,
           polar = F) {
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
    
    if (col) {
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
    if (tile) {
      p <- p + geom_tile(
        aes(
          x = X,
          y = Y,
          height = Z,
          
          color = Group,
          group = Group,
          width = size
        ),
        size = size,
        alpha = alpha,
        data = DAT
      )
    }

    if (area) {
      p <- p + geom_area(
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
    
    if (point) {
      p <- p + geom_point(
        aes(x = X,
            y = Y,
            
            color = Group),
        size = size,
        alpha = alpha,
        data = DAT
      )
    }
    
    if (line) {
      p <- p + geom_line(
        aes(
          x = X,
          y = Y,
          z = Z,
          
          color = Group,
          group = Group
        ),
        size = size,
        alpha = alpha,
        data = DAT
      )
    }
    if (spoke) {
      p <- p + geom_spoke(
        aes(
          x = X,
          y = Y,
          angle = Z,
          color = Group
        ),
        alpha = alpha,
        radius = size,
        size = size,
        data = DAT
      )
    }
    
    
    
    if (polar == T) {
      p <- p + coord_polar()
    }
    p
    
  }



generate_config <- function(NPOP) {
  data.frame(
    list(
      id = seq(1, NPOP),
      ngroups = sample(2:20, NPOP, replace = T),
      N = sample(150:2500, NPOP, replace = T),
      xmean = sample(1:5, NPOP, replace = T),
      ymean = sample(1:5, NPOP, replace = T),
      zmean = sample(1:5, NPOP, replace = T),
      xvar = sample(1:50, NPOP, replace = T),
      yvar = sample(1:50, NPOP, replace = T),
      zvar = sample(1:50, NPOP, replace = T),
      col = sample(c(T, F), NPOP, replace = T),
      tile = sample(c(T, F), NPOP, replace = T),
      area = sample(c(T, F), NPOP, replace = T),
      point = sample(c(T, F), NPOP, replace = T),
      spoke = sample(c(T, F), NPOP, replace = T),
      line = sample(c(T, F), NPOP, replace = T),
      
      polar = sample(c(T, F), NPOP, replace = T),
      size = sample(seq(1, 30, 1), NPOP, replace = T),
      alpha = runif(NPOP, 0, 1),
      ncolor = sample(2:8, NPOP, replace = T),
      colorscale = sample(1:14,
        NPOP,
        replace = T
      ),
      score = rep(NA, NPOP),
      pred_score = rep(NA, NPOP),
      rated = rep(F, NPOP)
    )
  )
}



crossover <- function(config_data,id){
  cr <-config_data %>%
    summarise(
      id = 0,
      ngroups = round(mean(ngroups,na.rm=T)),
      N = round(mean(N,na.rm=T)),
      xmean = mean(xmean,na.rm=T),
      xvar = mean(xvar,na.rm=T),
      ymean = mean(ymean,na.rm=T),
      yvar = mean(yvar,na.rm=T),
      zmean = mean(zmean,na.rm=T),
      zvar = mean(zvar,na.rm=T),
      col = round(mean(col,na.rm=T)),
      tile = round(mean(tile,na.rm=T)),
      area = round(mean(area,na.rm=T)),
      point = round(mean(point,na.rm=T)),
      spoke = round(mean(spoke,na.rm=T)),
      line = mean(round(line),na.rm=T),
      polar = as.logical(round(mean(polar,na.rm=T))),
      size = mean(size,na.rm=T),
      alpha = mean(alpha,na.rm=T),
      ncolor = round(mean(ncolor,na.rm=T)),
      colorscale = round(mean(colorscale,na.rm=T)),
      score = NA,
      pred_score = NA,
      rated = F
    )
  cr$id <- id
  cr
}

