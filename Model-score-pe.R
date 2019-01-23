##*## Ce script permet de modéliser les scores de santé de chaque pays ##*##
## Vérification du chemin du répertoire courant
getwd()

### CHARGEMENT LIBRARIES
library(tidyverse)
library(RCurl)
library(questionr)
library(lattice)
library(caret)
library(VGAM)
#library(help="VGAM")  # Pour avoir la documentation sur la librairie

### CHARGEMENT BASE & CREATION BASE TEST
url <- "https://raw.githubusercontent.com/Cthiburs/Scoring-Sante-Europe/master/SANTE_FR"
sante_fr <- read_delim(url,delim = ";")
glimpse(sante_fr)

sante_fr$SEX <- as.factor(sante_fr$SEX)
sante_fr$hhtype <- as.factor(sante_fr$hhtype)
sante_fr$HH_ACT <- as.factor(sante_fr$HH_ACT)
sante_fr$HHINCOME <- as.factor(sante_fr$HHINCOME)
sante_fr$AM1 <- as.factor(sante_fr$AM1)
sante_fr$AM4 <- as.factor(sante_fr$AM4)
sante_fr$AM6A <- as.factor(sante_fr$AM6A)
sante_fr$AM6B <- as.factor(sante_fr$AM6B)
sante_fr$MD1 <- as.factor(sante_fr$MD1)
sante_fr$MD2 <- as.factor(sante_fr$MD2)
sante_fr$PA5 <- as.factor(sante_fr$PA5)
sante_fr$PA6 <- as.factor(sante_fr$PA6)
sante_fr$PE1 <- as.factor(sante_fr$PE1)
sante_fr$PE2 <- as.factor(sante_fr$PE2)
sante_fr$PE4 <- as.factor(sante_fr$PE4)
sante_fr$PE6 <- as.factor(sante_fr$PE6)
sante_fr$FV1 <- as.factor(sante_fr$FV1)
sante_fr$etat_sante <- as.character(sante_fr$etat_sante)
sante_fr$nb_maladiegrave <- as.factor(sante_fr$nb_maladiegrave)
sante_fr$DEG_URB_r <- as.factor(sante_fr$DEG_URB_r)
sante_fr$HATLEVEL_r <- as.factor(sante_fr$HATLEVEL_r)
sante_fr$MAINSTAT_r <- as.factor(sante_fr$MAINSTAT_r)
sante_fr$AC1A_r <- as.factor(sante_fr$AC1A_r)
sante_fr$AC1B_r <- as.factor(sante_fr$AC1B_r)
sante_fr$AC1C_r <- as.factor(sante_fr$AC1C_r)
sante_fr$HO3_r3 <- as.factor(sante_fr$HO3_r3)
sante_fr$AGE_PERS65 <- as.factor(sante_fr$AGE_PERS65)
sante_fr$AM7_HO12 <- as.factor(sante_fr$AM7_HO12)
sante_fr$PA2_r <- as.factor(sante_fr$PA2_r)
sante_fr$PA34 <- as.factor(sante_fr$PA34)
sante_fr$FUMACTPAS <- as.factor(sante_fr$FUMACTPAS)
glimpse(sante_fr)
sante_fr$etat_sante2 <- ifelse(sante_fr$etat_sante=="MAU",0,ifelse(sante_fr$etat_sante=="MOD",1,2))

sante_fr$etat_sante2 <- ordered(as.factor(sante_fr$etat_sante2))
class(sante_fr$etat_sante2)
freq(sante_fr$etat_sante2)
table(sante_fr$etat_sante2,sante_fr$etat_sante)
names(sante_fr)
sante_fr2 <- sante_fr[,-20]
dim(sante_fr2)

test_index <- createDataPartition(sante_fr2$etat_sante2, times=1, p=0.3, list=F)
train_set <- sante_fr2[-test_index,]
test_set <- sante_fr2[test_index,]
# Vérification de l'équi-distribution des classes entre l'échantillon test et train
freq(train_set$etat_sante2)
freq(test_set$etat_sante2)

### LOGIT MULTINOMIAL CUMULATIF
train(sante_fr2[,2:34],etat_sante2,method = 'vglmCumulative')
#Modèle avec la constante contrainte parallèle
lsfc_i <- vglm(etat_sante2 ~ 1, data = sante_fr2, family = cumulative(parallel=TRUE,reverse=TRUE)) 
summary(lsfc_i)
#modèle avec les variables + contrainte parallèle
lsfcp <- vglm(etat_sante2 ~ ., data = sante_fr2[,2:34], family = cumulative(parallel=TRUE,reverse=TRUE)) 
summary(lsfcp)

# # Modèle avec les variables sans contrainte parallèle
# lsfc <- vglm(etat_sante2 ~ ., data = sante_fr2, family = cumulative(parallel=F,reverse=TRUE)) 
# summary(lsfc)
# ?vglm
# # Sélection Variable
# 
# # Test de la significativité de la contrainte parallèle
# pchisq( deviance( lsfcp) - deviance( lsfc), df=df.residual( lsfcp) - df.residual( lsfc), lower.tail=FALSE)
# 
# # Test de la significativité de la contrainte parallèle
# pchisq( deviance( lsfcp) - deviance( lsfc_i), df=df.residual( lsfcp) - df.residual( lsfc_i), lower.tail=FALSE)
