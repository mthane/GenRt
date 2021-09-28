---
title: "GenRt"
author: "Michael Thane"
date: "14 9 2021"
output: html_document
---



# Generative Art Using R
## Create the data set
There is a great potential for creating digital art by using R’s ggplot2 library and create visualizations that look like real art works. Mainly those approaches use data sets which are based on mathematical formulas and can be manipulated as desired. 
My approach of creating digital art using R is quite similar. First I created a data set that is based on three random variables “X”, “Y” and “Z” and one grouping variable “Group”. These distributions are based on a normal distribution with a given mean and standard deviation.



```r
generate_data <- function(N,ngroup,xmean,xvar,ymean,yvar,zmean,zvar){
   DAT=data.frame()

  names=paste("G_",seq(1,ngroup),sep="")
  DAT=data.frame()
  for(i in seq(1:ngroup)){
      data=data.frame( matrix(0, ngroup , 4))
      data[,1]=sample(names, nrow(data))
      data[,2]=sample( c(rnorm(N,mean = xmean,sd = xvar),c(1:ngroup)) ,nrow(data))
      data[,3]=sample( c(rnorm(N,mean=ymean,sd = yvar),c(1:ngroup)) ,nrow(data))
      data[,4]=sample( c(rnorm(N,mean=zmean,sd = zvar),c(1:ngroup)) ,nrow(data))
      DAT=rbind(DAT,data)
  }
  colnames(DAT)=c("Group","X","Y","Z")
  DAT
}
gendata <- generate_data(1050,5,1,2,3,4,5,6)
head(gendata)
```

```
##   Group          X        Y         Z
## 1   G_3 -1.1757651 4.771281  4.175128
## 2   G_4  1.6013904 4.375717  2.843582
## 3   G_5  1.4415897 7.427040 -4.754531
## 4   G_1  1.3460265 3.294832 10.273472
## 5   G_2 -0.5793881 6.667002 24.761452
## 6   G_3  0.2253360 5.672460  9.921660
```

## Create the plot
Using this data one can now plot an “art work”. I was trying to find attributes that would make the art works look quite different, but that can later also been used to find out which kind of art is preferred by the user. The attributes are the following:

1. N (Number of samples)
2. ngroups (Number of different groups)
3. xmean, ymean, zmean (means of the distributions)
4. xsd, ysd, zsd (standard deviations of the distributions)


5. geom (what kind of geometric is used in the art work)
6. alpha (opacity value)
7. polar (using polar coordinate system)
8. size (size of the geometric objects)
9. ncolor (number of different colors)
11. colorscale (which color scale is being used)

To find these attributes I was playing around with the outcome a bit, and it seemed that these attributes create various different visualizations. Some of these visualizations looked rather boring to me and some I enjoyed looking at. These visualizations will be used for the recommendation in the next step.




```r
generate_plot <- function(DAT,colorscale="Set1",col=T,tile=T,area=T,point=T,line=T,spoke=T,size=5,alpha=0.5,polar=F){

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
  if(tile){
      p <- p+geom_tile( aes(x=X,
                            y=Y,
                            height = Z,

                            color=Group,
                            group=Group,
                            width=size
      ), size=size,
      alpha=alpha,data = DAT)
  }
  # if(boxplot){
  #    p <- p+geom_boxplot( aes(x=X,
  #                             y=Y,
  # 
  #                             color=Group,
  #                             group=Group
  #    ), size=size,
  #    alpha=alpha,data = DAT)
  # }
  # if(violin){
  #   p <- p+geom_violin( aes(x=Group,
  #                           y=Y,
  # 
  #                           color=Group
  #   ),  size=size,
  #   alpha=alpha,data = DAT)
  # }

  if(area){
      p <- p+geom_area( aes(x=X,
                            y = Y,

                            color=Group,
                            group=Group
      ), size=size,alpha=alpha,data = DAT)
  }

  if(point){
      p <- p+geom_point( aes(x=X,
                             y=Y,

                             color=Group
      ),size=size,
      alpha=alpha,data = DAT)
  }

  if(line){
      p <- p+geom_line( aes(x=X,
                            y=Y,
                            z=Z,

                            color=Group,
                            group=Group
      ), size=size,
      alpha=alpha,data = DAT)
  }
  if(spoke){
      p <- p+geom_spoke( aes(x=X,y=Y,angle=Z,color=Group
      ),
      alpha=alpha,radius=size,size=size,data = DAT)
  }

  
  
  if(polar==T){
      p <- p+coord_polar()
  }
  p
    
}
generate_plot(gendata)
```

```
## Warning: Ignoring unknown aesthetics: z
```

![plot of chunk generate_plot](figure/generate_plot-1.png)

Below the same function is called, but this time with different parameters.


```r
gendata
```

```
##    Group           X          Y          Z
## 1    G_3 -1.17576511  4.7712813  4.1751279
## 2    G_4  1.60139041  4.3757166  2.8435815
## 3    G_5  1.44158968  7.4270398 -4.7545307
## 4    G_1  1.34602649  3.2948322 10.2734721
## 5    G_2 -0.57938808  6.6670018 24.7614522
## 6    G_3  0.22533596  5.6724605  9.9216597
## 7    G_5  0.47071515  3.1610686 10.1946922
## 8    G_4  3.43789114  0.7040976 -1.6979049
## 9    G_2  0.63907170  6.2918439  8.7306247
## 10   G_1 -0.77073500  5.6401788 -9.7717412
## 11   G_5  0.10680806  7.0035963  5.4933772
## 12   G_2  2.07447259 -4.4424499  2.0000000
## 13   G_4 -1.11764625  7.5293019 -1.8479477
## 14   G_1  0.61983232  2.0138600 -0.4353415
## 15   G_3  0.90470828  1.0656440 -2.1617010
## 16   G_2 -2.93572947  0.2634510  7.2304852
## 17   G_1  1.56819558  4.8625293 -1.1656530
## 18   G_5  2.72691831 -2.7303820 -2.1210609
## 19   G_4  2.87715586 -0.8287227 -0.3004653
## 20   G_3  2.69506130  2.4000935  4.8546466
## 21   G_3 -1.77848915 -7.6695210  1.1361314
## 22   G_1 -0.01221104 -2.3475002  8.0721751
## 23   G_2  6.27280125  3.8414479  6.6030204
## 24   G_5  0.47789619  3.8684395  9.2846308
## 25   G_4 -2.02813075  2.1485546  0.1803598
```

```r
generate_plot(gendata,colorscale = "Set2",size = 15,polar = T)
```

```
## Warning: Ignoring unknown aesthetics: z
```

![plot of chunk generate_plot2](figure/generate_plot2-1.png)


## Create various configurations
For creating various different visualizations I created a function that will create random configurations of the attributes.


```r
generate_config <- function(NPOP){
  data.frame(
    list(
      id = seq(1, NPOP),
      ngroups = sample(2:30, NPOP, replace = T),
      N = sample(150:1500, NPOP, replace = T),
      xmean = sample(1:5, NPOP, replace = T),
      ymean = sample(1:5, NPOP, replace = T),
      zmean = sample(1:5, NPOP, replace = T),
      xvar = sample(1:50, NPOP, replace = T),
      yvar = sample(1:50, NPOP, replace = T),
      zvar = sample(1:50, NPOP, replace = T),
      col = sample(c(T,F),NPOP,replace=T),
      tile = sample(c(T,F),NPOP,replace=T),
      boxplot= sample(c(T,F),NPOP,replace=T),
      violin= sample(c(T,F),NPOP,replace=T),
      area= sample(c(T,F),NPOP,replace=T),
      point= sample(c(T,F),NPOP,replace=T),
      spoke= sample(c(T,F),NPOP,replace=T),
      line = sample(c(T,F),NPOP,replace=T),
      
      polar = sample(c(T, F), NPOP, replace = T),
      size = sample(seq(1, 100, 1), NPOP, replace = T),
      alpha = runif(NPOP, 0, 1),
      ncolor = sample(2:8, NPOP, replace = T),
      colorscale = sample(
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
        ),
        NPOP,
        replace = T
      ),
      score = rep(NA, NPOP),
      pred_score = rep(NA, NPOP),
      rated = rep(F, NPOP)
    )
  )
}
generate_config(10)
```

```
##    id ngroups    N xmean ymean zmean xvar yvar zvar   col  tile boxplot violin  area point spoke  line polar size     alpha ncolor colorscale score
## 1   1       5  816     3     1     1    3   29   40 FALSE  TRUE    TRUE  FALSE  TRUE  TRUE  TRUE FALSE  TRUE    4 0.4100064      3       Set2    NA
## 2   2      22  267     4     5     3   39   17   18 FALSE FALSE   FALSE   TRUE FALSE FALSE  TRUE  TRUE  TRUE   18 0.2611337      7       BuGn    NA
## 3   3       8 1228     2     3     3   42    6   32  TRUE FALSE    TRUE   TRUE  TRUE FALSE  TRUE FALSE  TRUE   72 0.1009622      5    Pastel2    NA
## 4   4      20  941     5     5     2    4   29    5  TRUE  TRUE    TRUE  FALSE FALSE  TRUE FALSE  TRUE FALSE   28 0.2823639      7       Set1    NA
## 5   5      21  471     5     4     4   39   11    7 FALSE FALSE   FALSE   TRUE FALSE FALSE FALSE  TRUE FALSE   99 0.3076460      7    Pastel1    NA
## 6   6      11 1291     1     5     4   36   34    3  TRUE FALSE    TRUE  FALSE  TRUE  TRUE FALSE FALSE FALSE   42 0.7215566      3       Reds    NA
## 7   7      14  220     5     3     1   42   17   34 FALSE FALSE   FALSE   TRUE FALSE  TRUE  TRUE  TRUE FALSE   72 0.3998477      4       Set2    NA
## 8   8      21 1331     4     5     3   28   13   17  TRUE  TRUE   FALSE  FALSE FALSE FALSE FALSE  TRUE  TRUE   45 0.5525464      4     Paired    NA
## 9   9       4  551     1     2     5   27   36   33 FALSE FALSE    TRUE  FALSE  TRUE FALSE  TRUE  TRUE  TRUE   41 0.2252599      4      Dark2    NA
## 10 10       6  955     4     4     1   10   13   24  TRUE FALSE    TRUE   TRUE FALSE FALSE FALSE  TRUE FALSE   58 0.4980348      6    Pastel2    NA
##    pred_score rated
## 1          NA FALSE
## 2          NA FALSE
## 3          NA FALSE
## 4          NA FALSE
## 5          NA FALSE
## 6          NA FALSE
## 7          NA FALSE
## 8          NA FALSE
## 9          NA FALSE
## 10         NA FALSE
```
