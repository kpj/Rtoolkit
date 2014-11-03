library(relimp)
library(ggplot2)
library(reshape)

source("utils.R")


path <- "WV6_Data_spss_v_2014_06_04.sav" #file.choose()
frame = parseSAVFile(getWVSFrame(path), "V23")

plotOverview(frame)
