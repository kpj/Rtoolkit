library(multilevel)

source("utils.R")


path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
wvs <- getWVSFrame(path)

#frame.v23 = parseSAVFile(wvs, "V23") # life satisfaction
#frame.v55 = parseSAVFile(wvs, "V55") # freedom
#frame.v59 = parseSAVFile(wvs, "V59") # finances

countries <- c("Mexico", "Sweden", "South Korea", "United States")
for(country in countries) {
  print(paste("Parsing", country))
  
  sub <- subset(wvs, V2 == country)
  frame <- cleanDataFrame(sub)
  
  # regression
  regr <- lm(v23 ~ v55, data = frame) # names(regr)
  
  # plot
  ggplot(frame, aes(x=v23, y=v55)) +
    geom_point() +
    geom_smooth(method=lm) +
    labs(title=country, x="v23", y="v55") +
    ggsave(file=paste("out_", country, ".png", sep=""))
}

# correlation
#cor.test(frame$v23, frame$v59)