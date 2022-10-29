##########Olah data Ca dan K##################
setwd("D:/Documents/My Job/PENELITIAN/2022/data")
library(readxl)
CaK <- read_excel("DATA KALSIUM DAN POTASIUM.xlsx", 
                                        sheet = "olahR")
View(CaK)
fix(CaK)
CaK$Station <- as.factor(CaK$Station)
library(tidyverse)
sumCaK<- CaK %>%
  group_by(Station) %>%
  summarise(mean.Ca = mean(Ca),se.Ca = sd(Ca),
            mean.K = mean(K),se.K = sd(K),
            mean.DO = mean(DO),se.DO = sd(DO),            
            mean.Suhu = mean(Suhu),se.Suhu = sd(Suhu),
            mean.pH = mean(pHmeter),se.pH = sd(pHmeter),
            mean.Salinitas = mean(Salinitas),se.Salinitas = sd(Salinitas),
            mean.Turbidity = mean(Turbidity),se.Turbidity = sd(Turbidity))
write.csv(sumCaK, "mean dan se Ca, K, QW.csv")
str(CaK)

#UJI ANOVA CA
#Compute the analysis of variance data ca
res.aov <- aov(Ca ~ Station, data = CaK)

#summary anova
summary(res.aov)

#post hoc dengan TukeyHSD data Ca
tukey <- TukeyHSD(res.aov)
tukey

#menampilkan compact letter hasil tukey HSD
install.packages("multicompView")
library(multcompView)
cld <- multcompLetters4(res.aov, tukey)
cld
str(cld)

cld2 <- cld$Station["Letters"] 
cld2 <- as.data.frame(cld2)
print(cld2)

data <- add_column(sumCaK, cld2, .after = "se.Ca")
write.csv(data, "hasil tukey Ca.csv")
colnames(data)[4]  <- "TukeyCa" 

#UJI ANOVA K
#Compute the analysis of variance data ca
res.aov <- aov(K ~ Station, data = CaK)

#summary anova
summary(res.aov)

#post hoc dengan TukeyHSD data Ca
tukey <- TukeyHSD(res.aov)
tukey

#menampilkan compact letter hasil tukey HSD
install.packages("multicompView")
library(multcompView)
cld <- multcompLetters4(res.aov, tukey)
cld
str(cld)

cld2 <- cld$Station["Letters"] 
cld2 <- as.data.frame(cld2)
print(cld2)


#sisipkan kolumn baru setelah kolom se.K
data <- add_column(data, cld2, .after = "se.K")
#merubah nama kolum
colnames(data)[7]  <- "TukeyK" 
TukeyK <- c("A", "B", "C")

data <- data[,-7]
data <- add_column(data, TukeyK, .after="se.K")
write.csv(data, "hasil tukey Ca dan K.csv")
library(writexl)
writexl::write_xlsx(data, "data tukey ca dan K.xlsx")

##visualisasi data
grafikKCa <- read_excel("data tukey ca dan K.xlsx", 
                        +     sheet = "Sheet2")
str(grafikKCa)
grafikKCa$Station <- as.factor(grafikKCa$Station) 
grafikKCa <- mutate(grafikKCa, Minerals = case_when(
  Minerals == "Ca" ~ "Calcium",
  Minerals == "K" ~ "Potassium"))

grafik <- ggplot(data = grafikKCa,
       aes(x = Station, y = mean, fill = Station)) +
  facet_grid(cols = vars(Minerals)) +
  geom_col(width = .2, position = position_dodge(.8), colour="black")+
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_y_continuous(
    name = "Concentration (mg/L)",
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::number_format(accuracy = 1)) +
  labs(title = "", x="Station",
       caption = "Means followed by a common letter are not significantly different according to the Tukey-test") +
  geom_text(aes(label=Tukey, y= mean+se+20)) +
  scale_fill_manual(values = c("midnightblue", "steelblue3", 'lightskyblue')) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size=13, color = "black"),
    axis.text.y = element_text(size=13, color = "black"),
    axis.title.x = element_text(size=15, vjust = 2, color = "black"),
    axis.title.y = element_text(size=15, vjust=1.5, color = "black")
  ) +
  theme(legend.title = element_blank(),
        legend.position = "none")

grafik
ggsave("grafikCaK2.jpg", width=5.6, height =5, dpi=700)



#######REGRASI KORELASI#################
###########################################################################
#############################uji korelasi dgn p value################################
###########################################################################

library("Hmisc")
library("Hmisc")

datacor <- CaK[,4:10]
res2 <- rcorr(as.matrix(datacor))
koef <- res2$r
koef <- as.data.frame(koef)
koef <- res2$r
View(koef)
koef <- as.data.frame(koef)
pvalue <- res2$P
pvalue <- as.data.frame(pvalue)

writexl::write_xlsx(koef, "koefisien korelasi CaK.xlsx")
writexl::write_xlsx(pvalue, "pvalue korelasi CaK.xlsx")

###########################################################################
#############################regresi linier################################
###########################################################################
Kalium <-   lm(Nitrat ~ Klorofil, data=data)
summary(RClhNitrat) 
RClhNitrit <-   lm(Nitrit ~ Klorofil, data=data)
summary(RClhNitrit) 
RClhFosfat <-   lm(Fosfat ~ Klorofil, data=data)
summary(RClhFosfat)
RClhDIN <-   lm(DIN ~ Klorofil, data=data)
summary(RClhDIN)


RClhAmmonium <-   lm(Ammonium ~ Klorofil, data=data)
summary(RClhAmmonium)


RClhAmmonia<-   lm(Ammonia ~ Klorofil, data=data)
summary(RClhAmmonia)



