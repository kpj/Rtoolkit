library(foreign)
library(plyr)
library(ggplot2)


getWVSFrame <- function(path) {
  return(read.spss(path, to.data.frame=TRUE))
}

parseSAVFile <- function(wvs, col) {
  col_names <- c()
  data <- list()
  index = 1
  for(country in levels(wvs$V2)) {
    # only parse actual countries
    if(country %in% c("Missing; Unknown", "Not asked in survey", "Not applicable", "No answer", "Don't know")) {
      next
    }

    # filter important data
    s1 <- subset(wvs, V2 == country)
    cur <- s1[[col]]
    entry_num <- length(cur)

    # skip if no data is available
    if(entry_num == 0) {
      print(paste("No data set for", country, "available"))
      next
    }
    col_names <- append(col_names, country, after=length(col_names))

    # clean data set
    cur <- as.character(cur)
    
    # V23, V59
    cur[cur == "Completely satisfied"] <- "10"
    cur[cur == "Completely dissatisfied"] <- "1"
    
    # V55
    cur[cur == "A great deal of choice"] <- "10"
    cur[cur == "No choice at all"] <- "1"
    
    cur <- as.integer(cur)

    # aggregate frequencies
    agg <- count(cur)
    freqs <- agg$freq / entry_num

    # balance list to a length of 11 (1-10, NA)
    while(length(freqs) < 11) {
      freqs <- append(freqs, 0, length(freqs))
    }

    # store data
    data[[index]] <- freqs
    index = index + 1
  }

  # final assemblies
  frame = as.data.frame(data)
  colnames(frame) <- col_names

  return(frame)
}

plotOverview <- function(frame) {
  filler <- c() # use uniform colors in plot
  for(i in 1:length(colnames(frame))) {
    filler <- append(filler, 1:11, length(filler))
  }
    
  melted <- melt(frame)
  melted$colors <- filler
  
  ggplot(melted, aes(x=variable, y=value)) +
    geom_bar(aes(fill=as.factor(colors)), stat="Identity") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0), panel.background=element_blank()) +
    labs(title="Distribution", x="Country", y="Percentage")
}
