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
  p <- ggplot(frame, aes_string(x=factor(x), y=y)) +
    geom_boxplot(aes(fill=factor(v23))) +
    labs(title=plot_title, x=x, y=y)
  
  return(p)
}

pointPlot <- function(frame, x, y, plot_title) {
  p <- ggplot(frame, aes_string(x=x, y=y)) +
    geom_point(size=4, alpha=0.25, position=position_jitter(w=0.15, h=0.15)) +
    geom_smooth(method=lm) +
    labs(title=plot_title, x=x, y=y) +
    theme(panel.background=element_blank())
    
  return(p)
}