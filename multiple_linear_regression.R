library(multilevel)
library(QuantPsyc)
library(car)

source("utils.R")
source("plotting.R")


# parse database
path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
wvs <- getWVSFrame(path)

# predictors are significant
all.frame <- data.frame()
count <- 0
countries <- c("United States", "South Korea")
for(country in countries) {
  sub <- subset(wvs, V2 == country)
  
  frame <- cleanDataFrame(sub)
  frame$id <- rep.int(count, nrow(frame))
  colnames(frame) <- c("life_sat", "freedom", "financial", "country")
  
  all.frame <- rbind(all.frame, frame)
  
  count <- count + 1
}

regr <- lm(life_sat ~ freedom + financial, data=all.frame)
summary(regr)
lm.beta(regr)
avPlots(regr) # should look like random cloud, no structure

all.frame <- all.frame[complete.cases(all.frame),]
all.frame$fitted_values = regr$fitted.values
point.plot <- fittedPointPlot(all.frame, "fitted_values", "life_sat", "Combined Model")


# <0.8 multi-colinearity
all.frame.cor = all.frame
all.frame.cor$country = NULL
cor(all.frame.cor, use="pairwise.complete.obs")

cor.prob <- function(X, dfr=nrow(X)-2) {
  R <- cor(X)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

cor.prob(cor(all.frame.cor, use="pairwise.complete.obs"))

# how's the model in each country
united_states.frame = subset(all.frame, country == 0)
united_states.regr <- lm(life_sat ~ freedom + financial, data=united_states.frame)
summary(united_states.regr)
lm.beta(united_states.regr)

south_korea.frame = subset(all.frame, country == 1)
south_korea.regr <- lm(life_sat ~ freedom + financial, data=south_korea.frame)
summary(south_korea.regr)
lm.beta(south_korea.regr)

# visual analysis
for(country in countries) {
  for(y_var in c("freedom", "financial")) {
    plane_name <- gsub(" ", "_", tolower(country))
    fr <- get(paste0(plane_name, ".frame"))
    title <- country
    
    point.plot <- pointPlot(fr, y_var, "life_sat", title)
    box.plot <- boxPlot(fr, y_var, "life_sat", title)
    
    ggsave(point.plot, file=paste0("out_point_", country, "_", y_var, ".png"))
    ggsave(box.plot, file=paste0("out_box_", country, "_", y_var, ".png"))
  } 
}
