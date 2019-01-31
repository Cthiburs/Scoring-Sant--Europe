## Vérification du chemin du répertoire courant
getwd()

### CHARGEMENT LIBRARIES
library(tidyverse)
library(RCurl)
library(questionr)

### CHARGEMENT BASE, PONDERATION & CREATION BASE TEST
url_FR <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeFR_SCORE"
url_CY <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeCY_SCORE"
url_CZ <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeCZ_SCORE"
url_EL <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeEL_SCORE"
url_HU <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeHU_SCORE"
url_LT <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeLT_SCORE"
url_PT <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santePT_SCORE"
url_RO <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeRO_SCORE"
url_SE <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/santeSE_SCORE"
sante_FR <- read_delim(url_FR,delim = ";")
sante_FR2 <- sante_FR %>% select(c("PID","WGT","prob","class"))
sante_FR2$PAYS <- "FRANCE"
glimpse(sante_FR2)
sante_CY <- read_delim(url_CY,delim = ";")
sante_CY2 <- sante_CY %>% select(c("PID","WGT","prob","class"))
sante_CY2$PAYS <- "CHYPRE"
glimpse(sante_CY2)
sante_CZ <- read_delim(url_CZ,delim = ";")
sante_CZ2 <- sante_CZ %>% select(c("PID","WGT","prob","class"))
sante_CZ2$PAYS <- "TCHEQUE"
glimpse(sante_CZ2)
sante_EL <- read_delim(url_EL,delim = ";")
sante_EL2 <- sante_EL %>% select(c("PID","WGT","prob","class"))
sante_EL2$PAYS <- "GRECE"
glimpse(sante_EL2)
sante_HU <- read_delim(url_HU,delim = ";")
sante_HU2 <- sante_HU %>% select(c("PID","WGT","prob","class"))
sante_HU2$PAYS <- "HONGRIE"
glimpse(sante_EL2)
sante_LT <- read_delim(url_LT,delim = ";")
sante_LT2 <- sante_LT %>% select(c("PID","WGT","prob","class"))
sante_LT2$PAYS <- "LITUANIE"
glimpse(sante_LT2)
sante_PT <- read_delim(url_PT,delim = ";")
sante_PT2 <- sante_PT %>% select(c("PID","WGT","prob","class"))
sante_PT2$PAYS <- "PORTUGAL"
glimpse(sante_PT2)
sante_RO <- read_delim(url_RO,delim = ";")
sante_RO2 <- sante_RO %>% select(c("PID","WGT","prob","class"))
sante_RO2$PAYS <- "ROUMANIE"
glimpse(sante_RO2)
sante_SE <- read_delim(url_SE,delim = ";")
sante_SE2 <- sante_SE %>% select(c("PID","WGT","prob","class"))
sante_SE2$PAYS <- "SUEDE"
glimpse(sante_SE2)

# Construction base de score
sante_eu <- rbind(sante_FR2,sante_CY2, sante_CZ2, sante_EL2, sante_HU2, sante_LT2, sante_PT2, sante_RO2, sante_SE2)
freq(sante_eu$class)
freq(is.na(sante_eu$WGT))
sante_eu[is.na(sante_eu$WGT),]
sante_eu <- sante_eu %>% filter(!is.na(sante_eu$WGT))
sante_eu$PAYSCLASS <- paste(sante_eu$PAYS,sante_eu$class,sep="_")
sante_eu$class <- as.factor(sante_eu$class)

### SCORES
sante_eu %>% group_by(PAYSCLASS) %>% summarise(Nombre = n(), Moyenne=mean(prob), SD = sd(prob), Médianne = median(prob), Ecart_IQ = IQR(prob))
sante_eu %>% group_by(PAYS) %>% summarise(Nombre = n(), Moyenne=mean(prob), SD = sd(prob), Médianne = median(prob), Ecart_IQ = IQR(prob))
P <- sante_eu %>% ggplot(aes(x = PAYSCLASS, y = prob, color=class)) +
 geom_boxplot() + scale_color_manual(values = c("red4","orange2","green4")) + 
  stat_summary(fun.y=mean, geom="point")
P

## Test de Kruskal Wallis class 0
sante_eu$PAYSCLASS <- as.factor(sante_eu$PAYSCLASS)
sante_euclass0 <- sante_eu %>% filter(class=="0")
sante_euclass0$PAYSCLASS <- as.factor(sante_euclass0$PAYSCLASS)
kruskal.test(prob ~ PAYSCLASS, sante_euclass0)
# Comparaison multiple pour identifier les pays significativement différents
comp_class0 <- pairwise.wilcox.test(sante_euclass0$prob, sante_euclass0$PAYSCLASS, p.adjust.method = "BH")
pvalue_class0 <-as.data.frame(comp_class0$p.value)
pvalue_class0
pvalue_class0$LIGNE <- rownames(pvalue_class0)
write.csv2(pvalue_class0,"test_comp_score0.csv")

##### COMMENTAIRE  ####
# On 

## Test de Kruskal Wallis class 1
sante_eu$PAYSCLASS <- as.factor(sante_eu$PAYSCLASS)
sante_euclass1 <- sante_eu %>% filter(class=="1")
sante_euclass1$PAYSCLASS <- as.factor(sante_euclass1$PAYSCLASS)
kruskal.test(prob ~ PAYSCLASS, sante_euclass1)
# Comparaison multiple pour identifier les pays significativement différents
comp_class1 <- pairwise.wilcox.test(sante_euclass1$prob, sante_euclass1$PAYSCLASS, p.adjust.method = "BH")
pvalue_class1 <-as.data.frame(comp_class1$p.value)
pvalue_class1
pvalue_class1$LIGNE <- rownames(pvalue_class1)
write.csv2(pvalue_class1,"test_comp_score1.csv")

##### COMMENTAIRE  ####
# On 


## Test de Kruskal Wallis class 2
sante_eu$PAYSCLASS <- as.factor(sante_eu$PAYSCLASS)
sante_euclass2 <- sante_eu %>% filter(class=="2")
sante_euclass2$PAYSCLASS <- as.factor(sante_euclass2$PAYSCLASS)
kruskal.test(prob ~ PAYSCLASS, sante_euclass2)
# Comparaison multiple pour identifier les pays significativement différents
comp_class2 <- pairwise.wilcox.test(sante_euclass2$prob, sante_euclass2$PAYSCLASS, p.adjust.method = "BH")
pvalue_class2 <-as.data.frame(comp_class2$p.value)
pvalue_class2
pvalue_class2$LIGNE <- rownames(pvalue_class2)
write.csv2(pvalue_class2,"test_comp_score2.csv")

##### COMMENTAIRE  ####
# On 



## Pour la classe de ceux qui sont en MAUVAISE santé
SCOREmy0 <- sante_eu %>% filter(class=="0") %>% group_by(PAYS) %>% summarise(Score_Moyen0=wtd.mean(prob,weights = WGT))
SCOREmy0
SCOREmy0 <- SCOREmy0[order(SCOREmy0$Score_Moyen0,decreasing = T),]
SCOREmy0$Rang_Smy0 <- 1:9
SCOREmy0
SCOREmd0 <-  sante_eu %>% filter(class=="0") %>% group_by(PAYS) %>% summarise(Score_Médian0= median(prob))
SCOREmd0
SCOREmd0 <- SCOREmd0[order(SCOREmd0$Score_Médian0,decreasing = T),]
SCOREmd0$Rang_Smd0 <- 1:9
SCOREmd0
SCORE_0 <- merge(SCOREmy0,SCOREmd0,by="PAYS")
SCORE_0

## Pour la classe de ceux qui sont en santé modéré
SCOREmy1 <- sante_eu %>% filter(class=="1") %>% group_by(PAYS) %>% summarise(Score_Moyen1=wtd.mean(prob,weights = WGT))
SCOREmy1
SCOREmy1 <- SCOREmy1[order(SCOREmy1$Score_Moyen1,decreasing = T),]
SCOREmy1$Rang_Smy1 <- 1:9
SCOREmy1
SCOREmd1 <-  sante_eu %>% filter(class=="1") %>% group_by(PAYS) %>% summarise(Score_Médian1= median(prob))
SCOREmd1
SCOREmd1 <- SCOREmd1[order(SCOREmd1$Score_Médian1,decreasing = T),]
SCOREmd1$Rang_Smd1 <- 1:9
SCOREmd1
SCORE_1 <- merge(SCOREmy1,SCOREmd1,by="PAYS")
SCORE_1

## Pour la classe de ceux qui sont en bonne santé
SCOREmy2 <- sante_eu %>% filter(class=="2") %>% group_by(PAYS) %>% summarise(Score_Moyen2=wtd.mean(prob,weights = WGT))
SCOREmy2
SCOREmy2 <- SCOREmy2[order(SCOREmy2$Score_Moyen2,decreasing = T),]
SCOREmy2$Rang_Smy2 <- 1:9
SCOREmy2
SCOREmd2 <-  sante_eu %>% filter(class=="2") %>% group_by(PAYS) %>% summarise(Score_Médian2= median(prob))
SCOREmd2
SCOREmd2 <- SCOREmd2[order(SCOREmd2$Score_Médian2,decreasing = T),]
SCOREmd2$Rang_Smd2 <- 1:9
SCOREmd2
SCORE_2 <- merge(SCOREmy2,SCOREmd2,by="PAYS")
SCORE_2

SCORE <- inner_join(inner_join(SCORE_0,SCORE_1,by="PAYS"),SCORE_2,by="PAYS")
SCORE

SCORE_VF <- SCORE %>% select(c("PAYS","Rang_Smy0","Rang_Smy1","Rang_Smy2","Rang_Smd0", "Rang_Smd1","Rang_Smd2"))
SCORE_VF

write.csv2(SCORE,"Classement_PAYS.csv")
write.csv2(SCORE_VF,"Classement_PAYS2.csv")
