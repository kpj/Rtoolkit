library(multilevel)

source("utils.R")


# parse database
path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
wvs <- getWVSFrame(path)

# extract needed information
sweden.sub <- subset(wvs, V2 == "Sweden")
sweden.frame <- cleanDataFrame(sweden.sub)

south_korea.sub <- subset(wvs, V2 == "South Korea")
south_korea.frame <- cleanDataFrame(south_korea.sub)

# do regressions
sweden.regr <- lm(v23 ~ v55 + v59, data=sweden.frame)
south_korea.regr <- lm(v23 ~ v55 + v59, data=south_korea.frame)

summary(sweden.regr)$adj.r.squared
summary(south_korea.regr)$adj.r.squared

# check for non-multicolinearity
cor(sweden.frame)
cor(south_korea.frame)

# homogeneity of variance
with(sweden.frame, leveneTest(v23, interaction(v55, v59)))
with(south_korea.frame, leveneTest(v23, interaction(v55, v59)))

# independence of cases
chisq.test(sweden.frame$v23)
chisq.test(sweden.frame$v55)
chisq.test(sweden.frame$v59)
chisq.test(south_korea.frame$v23)
chisq.test(south_korea.frame$v55)
chisq.test(south_korea.frame$v59)

# normality
ks.test(sweden.frame$v23, "pnorm")
ks.test(sweden.frame$v55, "pnorm")
ks.test(sweden.frame$v59, "pnorm")
ks.test(south_korea.frame$v23, "pnorm")
ks.test(south_korea.frame$v55, "pnorm")
ks.test(south_korea.frame$v59, "pnorm")
