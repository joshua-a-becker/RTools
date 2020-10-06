#source('~/R Code/joshfunctions.R')
#library(boot)
#setwd("C://Users//joshua//Dropbox")
library(ggplot2)
library(compiler)
oldjoshtheme =   theme(panel.background=element_rect(fill="white", color="grey"), 
                    axis.text=element_text(size=rel(1)), 
                    strip.text=element_text(size=rel(1.3)), 
                    legend.text=element_text(size=rel(1.3)), 
                    title=element_text(size=rel(1.3)),
                    panel.grid=element_blank())

beckertheme =   theme(panel.background=element_rect(fill="white", color="black", size=1.1), 
                    axis.text=element_text(size=rel(1), color="black"), 
                    strip.text=element_text(size=rel(1.1)), 
                    legend.text=element_text(size=rel(1.1)), strip.background=element_blank(),
                    title=element_text(size=rel(1.1)),
                    panel.grid=element_blank(),
                    plot.title=element_text(hjust=0.5))


rotateX = theme(axis.text.x=element_text(angle=90))

adjX = theme(axis.text.x=element_text(vjust=0.5))

startSQL = function(sqldb="mysql") {
  require(RODBC)
  mysql <<- odbcConnect(sqldb, case="nochange")
  cat("Available Sql Tables:\n")
  print(sqlTables(mysql))
}

mybootsd = function(data, boot.trials=1000, samplesize=-1) {
  if(samplesize<0) { samplesize = length(data) }
  resamples <- lapply(1:boot.trials, function(i) sample(data, size=samplesize, replace=T))
  sd(sapply(resamples, FUN=function(x){mean(x, na.rm=T)}))
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE, boot.trials=1) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm),
                     bootse = mybootsd(xx[[col]], boot.trials)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  datac$bootci = datac$bootse * ciMult
  
  return(datac)
}


samplemean <- function(x, d) {
  return(mean(x[d,]))
}

summaryBOOT <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                        conf.interval=.95, .drop=TRUE) {
  
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  
  ## generate bootstrapped CI
  for(i in 1:nrow(datac)) {
    
  }
  
  return(datac)
}

runQuery = function() {
  sqlQuery(mysql, myQuery)
}

sql = function(thisQuery) {
  sqlQuery(mysql, thisQuery)
}

VerticalHist <- function(x, xscale = NULL, xwidth, hist,
                         fillCol = "gray80", lineCol = "gray40") {
  ## x (required) is the x position to draw the histogram
  ## xscale (optional) is the "height" of the tallest bar (horizontally),
  ##   it has sensible default behavior
  ## xwidth (required) is the horizontal spacing between histograms
  ## hist (required) is an object of type "histogram"
  ##    (or a list / df with $breaks and $density)
  ## fillCol and lineCol... exactly what you think.
  binWidth <- hist$breaks[2] - hist$breaks[1]
  if (is.null(xscale)) xscale <- xwidth * 0.9 / max(hist$density)
  n <- length(hist$density)
  x.l <- rep(x, n)
  x.r <- x.l + hist$density * xscale
  y.b <- hist$breaks[1:n]
  y.t <- hist$breaks[2:(n + 1)]
  
  bordercol=rep(lineCol,n)
  bordercol[which(x.r==x.l)] = fillCol
  
  rect(xleft = x.l, ybottom = y.b, xright = x.r, ytop = y.t,
       col = fillCol, border = bordercol)
}


# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                    seq(0,1,length=256),  # Green
                    seq(1,0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  #ColorRamp = c("#FFFFFF",ColorRamp)
  #ColorLevels = c(-1, ColorLevels)
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}
myImagePlotSpecial <- function(x, ...){
  min <- 0
  max <- 1
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                    seq(0,1,length=256),  # Green
                    seq(1,0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  #ColorRamp = c("#FFFFFF",ColorRamp)
  #ColorLevels = c(-1, ColorLevels)
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}



myImagePlotExtraSpecial <- function(x, ...){
  min <- -1
  max <- 1
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp2 <- rgb( seq(0,1,length=256),  # Red
                     seq(0,1,length=256),  # Green
                     seq(1,0,length=256))  # Blue
  ColorRamp1 <- rgb( seq(1,0,length=256),  # Red
                     seq(0,0,length=256),  # Green
                     seq(0,1,length=256))  # Blue
  ColorRamp = c(ColorRamp1, ColorRamp2)
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  ColorRamp = c("#FFFFFF",ColorRamp)
  ColorLevels = c(-100, ColorLevels)
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}



plotContrastMatrix <- function(x, ...){
  min <- max(x)
  max <- min(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  
  
  ColorRamp <- c(brewer.pal(12,"Set3"),brewer.pal(8, "Set2"), brewer.pal(9, "Set1"))
  n = ceiling((abs(max-min))/length(ColorRamp))
  ColorRamp = rep(ColorRamp, n)
  ColorRamp = ColorRamp[1:abs(max-min)]
  #ColorRamp = 1:abs(max-min)
  print(ColorRamp)
  ColorLevels <- sort(unique(as.vector(x)))
  print(ColorLevels)
  ColorRamp = c("#FFFFFF",ColorRamp)
  #ColorLevels = c(-1, ColorLevels)
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(0,abs(max-min)))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}
# ----- END plot function ----- #




meanify = function(x) { 
  return(mean(x, na.rm=T))
}


nodiag = function(x) {
  diag(x) = NA
  x
}



fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("1e\\+", "10^ ", l)
  l <- gsub("1e\\-", "10^-", l)
  # return this as an expression
  parse(text=l)
}




structural.equivalence = function(adjmat) {
  
  ## calculates the euclidean distance between two nodes.
  ## this is not a directed function.
  eucdistance = function(mymat, i, j) {
    a=(mymat[i,j] - mymat[j,i])^2
    b=sum((mymat[i,]-mymat[j,])^2)
    c=sum((mymat[,i]-mymat[,j])^2)
    sqrt(a+b+c)
  }
  
  ## runs the eucdistance function on all the pairs
  ## generating a matrix of all the pairwise euclidean distances
  distmat = matrix(nrow=nrow(adjmat), ncol=ncol(adjmat))
  for(i in 1:ncol(adjmat) ) {
    for(j in which(adjmat[i,]!=0) ) {
      distmat[i,j] = eucdistance(adjmat, i,j)
    }
  }
  
  ## binarize the matrix of euclidean distances
  distmat = distmat*(adjmat/adjmat)
  distmat[which(is.na(distmat))]=0
  
  ## identify the max distance for a node's ego network
  dmax=apply(distmat, 2, FUN=max)
  
  ## generate a matrix of directed structural equivalence
  ## as per Burt, 1987
  ## with v = 1
  equivweight = matrix(nrow=nrow(adjmat), ncol=ncol(adjmat))
  for(j in 1:ncol(adjmat) ) {
    for(i in which(adjmat[j,]!=0) ) {
      equivweight[i,j] = (dmax[j] - distmat[j,i]) / sum(dmax[j]-distmat[which(adjmat[j,]!=0),i])
    }
  }
  
  ## capture 0-edges as -1 to preserve them in the matrix
  ## and differentiate them from non-edges
  equivweight[which(equivweight==0)]=-1
  equivweight[which(is.na(equivweight))] = 0
  
  equivweight
}


mybox <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.2, 0.5, 0.8, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}



gini = function(v) {
  top = sum(sapply(v, FUN=function(vi){sum(sapply(v, FUN=function(vj){abs(vj-vi)}))}))
  bot = 2*length(v)*sum(v)
  top/bot
}




innerTicks = function(plot_obj
                       , left_limit
                       , right_limit
                       , y_major_breaks
                       , y_minor_breaks
                       , y_major_length
                       , y_minor_length
                       , bottom_limit
                       , top_limit
                       , x_major_breaks
                       , x_minor_breaks
                       , x_major_length
                       , x_minor_length
                       , x_labels = x_major_breaks
                       , y_labels = y_major_breaks) {
  
  plot_obj + 
    theme(axis.ticks.length=unit(0, "cm")) +
    scale_x_continuous(  expand=expand_scale(mult = c(0, 0))
                         , lim=c(left_limit, right_limit)
                         , breaks=x_major_breaks
                         , labels=x_labels
    )+
    scale_y_continuous(  expand=expand_scale(mult = c(0, 0))
                         , lim=c(bottom_limit, top_limit)
                         , breaks=y_major_breaks
                         , labels=y_labels)+
    #y
    annotate(  geom="segment", y=y_major_breaks, yend=y_major_breaks
               , x=left_limit, xend= left_limit+y_major_length) +
    annotate(  geom="segment", y=y_minor_breaks, yend=y_minor_breaks
               ,  x=left_limit, xend= left_limit+y_minor_length) +
    annotate(  geom="segment", y=y_major_breaks, yend=y_major_breaks
               , x=right_limit, xend= right_limit-y_major_length) +
    annotate(  geom="segment", y=y_minor_breaks, yend=y_minor_breaks
               ,  x=right_limit, xend= right_limit-y_minor_length) + 
    
    #x
    annotate(  geom="segment", x=x_major_breaks, xend=x_major_breaks
               , y=bottom_limit, yend= bottom_limit+x_major_length) +
    annotate(  geom="segment", x=x_minor_breaks, xend=x_minor_breaks
               ,  y=bottom_limit, yend= bottom_limit+x_minor_length) +
    annotate(  geom="segment", x=x_major_breaks, xend=x_major_breaks
               , y=top_limit, yend= top_limit-x_major_length) +
    annotate(  geom="segment", x=x_minor_breaks, xend=x_minor_breaks
               ,  y=top_limit, yend= top_limit-x_minor_length)
  
}

### THIS FUNCTION COURTESY
### KEVIN USHEY
### https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}
