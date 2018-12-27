library(tidyverse)
getwd()
setwd("/Users/thibursagbahoungbata/Downloads")
sante <- read.csv2("BDD_SANTE.csv")

sante2 <- sante %>% filter(COUNTRY %in% c("AT","CY","CZ","DE","FR"))
write.csv2(sante2,"sante2.csv")
