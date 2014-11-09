library(multilevel)
library(car)
library(moments)
library(MVN)

source("utils.R")
source("plotting.R")


path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
wvs <- getWVSFrame(path)

#frame.v23 = parseSAVFile(wvs, "V23") # life satisfaction
#frame.v55 = parseSAVFile(wvs, "V55") # freedom
#frame.v59 = parseSAVFile(wvs, "V59") # finances

## variables to use in analysis
var1 <- "v23"
var2 <- "v55"

countries <- c("Mexico", "Sweden", "South Korea", "United States")
for(country in countries) {
  print(paste("Parsing", country))

  sub <- subset(wvs, V2 == country)
  frame <- cleanDataFrame(sub)
  
  ## helpful stuff
  formula <- paste(var1, "~", var2)

  ## regression
  regr <- lm(formula, data = frame) # names(regr)
  sumy <- summary(regr)

  rs <- round(sumy$r.squared, digits=4)
  ars <- round(sumy$adj.r.squared, digits=4)

  ## test assumptions
  #leveneTest(.., data=frame)
  # chi-squared test of independence
  chisq.test(frame[[var1]], frame[[var2]])
  # test for normality
  ks.test(frame[[var1]], frame[[var2]])
  
  shapiro.test(frame[[var1]])
  shapiro.test(frame[[var2]])
  
  kurtosis(frame[[var1]])
  kurtosis(frame[[var2]])

  skewness(frame[[var1]])
  skewness(frame[[var2]])
  
  mardiaTest(frame[[var1]], qqplot=TRUE)
  
  ## correlation
  cor.test(frame[[var1]], frame[[var2]])
  
  ## plot
  title <- paste(country, ", r^2:", rs, ", adj. r^2:", ars)
  
  plot.box <- boxPlot(frame, var1, var2, title)
  plot.point <- pointPlot(frame, var1, var2, title)
    
  ggsave(plot.box, file=paste("out_box_", country, ".png", sep=""))
  ggsave(plot.point, file=paste("out_point_", country, ".png", sep=""))
}
