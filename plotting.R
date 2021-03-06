library(relimp)
library(ggplot2)
library(reshape)


overviewPlot <- function(frame) {
  filler <- c() # use uniform colors in plot
  for(i in 1:length(colnames(frame))) {
    filler <- append(filler, 1:11, length(filler))
  }
  
  melted <- melt(frame)
  melted$colors <- filler
  
  p <- ggplot(melted, aes(x=variable, y=value)) +
    geom_bar(aes(fill=as.factor(colors)), stat="Identity") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0), panel.background=element_blank()) +
    labs(title="Distribution", x="Country", y="Percentage")
  
  return(p)
}

boxPlot <- function(frame, x, y, plot_title) {
  frame$groups <- factor(frame[[x]])
  frame$foo <- frame[[y]]
    
  p <- ggplot(frame, aes(x=groups, y=foo)) +
    geom_boxplot(aes(fill=groups)) + 
    labs(title=plot_title, x=x, y=y) +
    scale_y_discrete(breaks=seq(1,10), labels=seq(1,10))
  
  return(p)
}

pointPlot <- function(frame, x, y, plot_title) {
  p <- ggplot(frame, aes_string(x=x, y=y)) +
    geom_point(alpha=0.3, color="firebrick", position=position_jitter(w=0.15, h=0.15)) +
    geom_smooth(method=lm) +
    labs(title=plot_title, x=x, y=y) +
    scale_x_discrete(breaks=seq(1,10), labels=seq(1,10)) +
    scale_y_discrete(breaks=seq(1,10), labels=seq(1,10))
    
  return(p)
}

fittedPointPlot <- function(frame, x, y, plot_title) {
  frame <- rbind(frame, c(0,0,0,0))
  frame <- rbind(frame, c(10,10,10,10))
  
  p <- ggplot(frame, aes_string(x=x, y=y)) +
    geom_point(alpha=0.5, color="firebrick", position=position_jitter(w=0.02, h=0.15)) +
    geom_smooth(method=lm) +
    labs(title=plot_title, x=x, y=y) +
    scale_x_discrete(breaks=seq(1,10), labels=seq(1,10)) +
    scale_y_discrete(breaks=seq(1,10), labels=seq(1,10)) +
    theme(panel.background=element_blank())
  
  return(p)
}