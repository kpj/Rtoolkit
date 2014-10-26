library(multilevel)

source("utils.R")


path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
wvs <- getWVSFrame(path)

#frame.v23 = parseSAVFile(wvs, "V23") # life satisfaction
#frame.v55 = parseSAVFile(wvs, "V55") # freedom
#frame.v59 = parseSAVFile(wvs, "V59") # finances

#cor(frame.v23$Sweden, frame.v55$Sweden)
#cor(frame.v23$Sweden, frame.v59$Sweden)

#cor(frame.v23$Mexico, frame.v55$Mexico)
#cor(frame.v23$Mexico, frame.v59$Mexico)

sub <- subset(wvs, V2 == "Sweden")

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

frame <- data.frame(v23, v55, v59)


ggplot(frame, aes(x=v23, y=v55)) +
  geom_point() +
  geom_smooth(method=lm)



cor.test(frame$v23, frame$v59)
