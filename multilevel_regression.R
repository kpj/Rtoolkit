library(multilevel)

source("utils.R")
source("plotting.R")


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
  title <- paste(country, ", r^2:", rs, ", adj. r^2:", ars)
  var1 <- "v23"
  var2 <- "v55"
  
  plot.box <- boxPlot(frame, var1, var2, title)
  plot.point <- pointPlot(frame, var1, var2, title)
    
  ggsave(plot.box, file=paste("out_box_", country, ".png", sep=""))
  ggsave(plot.point, file=paste("out_point_", country, ".png", sep=""))
}

# correlation
#cor.test(frame$v23, frame$v59)
