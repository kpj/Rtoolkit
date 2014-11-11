library(multilevel)
library(QuantPsyc)

source("utils.R")
source("plotting.R")


# parse database
path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
wvs <- getWVSFrame(path)

# predictors are significant
all.frame <- data.frame()
count <- 0
countries <- c("Sweden", "South Korea")
for(country in countries) {
  sub <- subset(wvs, V2 == country)
  
  frame <- cleanDataFrame(sub)
  frame$id <- rep.int(count, nrow(frame))
  colnames(frame) <- c("life_sat", "freedom", "financial", "country")
  
  all.frame <- rbind(all.frame, frame)
  
  count <- count + 1
}

regr <- lm(life_sat ~ freedom + financial + country, data=all.frame)
lm.beta(regr)
avPlots(regr) # should look like random cloud, no structure

noco.regr <- lm(life_sat ~ freedom + financial, data=all.frame)
summary(noco.regr)
lm.beta(noco.regr)
avPlots(noco.regr)

# <0.8 multi-colinearity
all.frame.cor = all.frame
all.frame.cor$country = NULL
cor(all.frame.cor, use="pairwise.complete.obs")

# how's the model in each country
sweden.frame = subset(all.frame, country == 0)
sweden.regr <- lm(life_sat ~ freedom + financial, data=sweden.frame)
summary(sweden.regr)
lm.beta(sweden.regr)

south_korea.frame = subset(all.frame, country == 1)
south_korea.regr <- lm(life_sat ~ freedom + financial, data=south_korea.frame)
summary(south_korea.regr)
lm.beta(south_korea.regr)

# visual analysis
sweden.point.plot <- pointPlot(sweden.frame, "freedom", "life_sat", "Sweden")
sweden.point.plot <- pointPlot(sweden.frame, "financial", "life_sat", "Sweden")
south_korea.point.plot <- pointPlot(south_korea.frame, "freedom", "life_sat", "South Korea")

sweden.box.plot <- boxPlot(sweden.frame, "freedom", "life_sat", "Sweden")
print(sweden.box.plot)
