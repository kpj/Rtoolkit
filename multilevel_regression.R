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
  sumy <- summary(regr)

  rs <- round(sumy$r.squared, digits=4)
  ars <- round(sumy$adj.r.squared, digits=4)

  print(paste("> r^2:", rs, ", adj. r^2:", ars))

  # plot
  ggplot(frame, aes(x=factor(v23), y=v55)) +
    geom_boxplot(aes(fill=factor(v23))) +
    labs(title=paste(country, ", r^2:", rs, ", adj. r^2:", ars), x="v23", y="v55") +
    ggsave(file=paste("out_", country, ".png", sep=""))
}

# correlation
#cor.test(frame$v23, frame$v59)
