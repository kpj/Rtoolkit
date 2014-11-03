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

cleanDataFrame <- function(frame) {
  # cleans v23, v55, v59
  
  v23 <- sub$V23
  v23 <- as.character(v23)
  v23[v23 == "Completely satisfied"] <- "10"
  v23[v23 == "Completely dissatisfied"] <- "1"
  v23 <- as.integer(v23)
  v23[is.na(v23)] <- 0
  
  v55 <- sub$V55
  v55 <- as.character(v55)
  v55[v55 == "A great deal of choice"] <- "10"
  v55[v55 == "No choice at all"] <- "1"
  v55 <- as.integer(v55)
  v55[is.na(v55)] <- 0
  
  v59 <- sub$V59
  v59 <- as.character(v59)
  v59[v59 == "Completely satisfied"] <- "10"
  v59[v59 == "Completely dissatisfied"] <- "1"
  v59 <- as.integer(v59)
  v59[is.na(v59)] <- 0
  
  # assemble data
  frame <- as.data.frame(v23)
  frame$v55 <- v55
  frame$v59 <- v59
  
  return(frame)
}