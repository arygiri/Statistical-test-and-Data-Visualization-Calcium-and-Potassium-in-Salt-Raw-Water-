setwd("D:/Documents/My Job/PENELITIAN/2022")
library(readxl)
analisa_clorofil_dan_peptin <- read_excel("analisa clorofil dan peptin.xlsx")
View(analisa_clorofil_dan_peptin)
data <- analisa_clorofil_dan_peptin[,-7]
data <- data[-225,]
data2 <- data[c(seq(4,224,4)),]
bAsam1<- data2[c(seq(1,10,2)),]
bAsam2 <- data2[c(seq(12,56,2)),]
bAsam3 <- data2[1,]
bAsam <- rbind(bAsam1,bAsam2)
aAsam1 <- data2[c(seq(2,10,2)),]
aAsam2 <- data2[c(seq(13,56,2)),]
aAsam3 <- data2[1,]
aAsam <- rbind(aAsam3,aAsam1,aAsam2)

fix <- cbind(aAsam, bAsam)
library(xlsx)
xlsx::write.xlsx(fix, "rata2clh.xlsx" )

library(readxl)
clh <- read_excel("dataCLHbuaya.xlsx")
View(clh)

#membuat summary data mean dan ad
library(tidyverse)
datasum <- clh %>%
  group_by(stasiun) %>%
  summarise(mean.clh = mean(clh),sd.clh = sd(clh))

xlsx::write.xlsx(datasum, "datasumclh_1.xlsx" )
