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
library(MASS)
library(grid)
library(Matrix)
library(survey)
#library(help="VGAM")  # Pour avoir la documentation sur la librairie

### CHARGEMENT BASE, PONDERATION & CREATION BASE TEST
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
# sante_fr$etat_sante2 <- as.integer(sante_fr$etat_sante2)
class(sante_fr$etat_sante2)
freq(sante_fr$etat_sante2)
table(sante_fr$etat_sante2,sante_fr$etat_sante)
names(sante_fr)
sante_fr2 <- sante_fr[,-21]
dim(sante_fr2)

## dichotomisation des exogènes
#ref <- c("SEX2","hhtype1","HH_ACT0","HHINCOME1","AM14","AM43","AM6A2","AM6B2","MD12","MD22","PA55","PA65","PE11","PE20","PE40","PE60","FV15","nb_maladiegrave0","nb_maladiepasgrave0","DEG_URB_r3","HATLEVEL_rPrim","MAINSTAT_r20","AC1A_r2","AC1B_r2","AC1C_r","HO3_r30","AGE_PERS6515-39_Non","AM7_HO120_Non","PA2_rJamais","PA34Jamais","FUMACTPASNon Fumeur")
ref <- c("SEX2","hhtype1","HH_ACT0","HHINCOME1","AM13","AM41","AM6A2","AM6B2",
         "MD11","MD21","PA55","PA65","PE12","PE20","PE40","PE60","FV11",
         "nb_maladiegrave0","nb_maladiepasgrave0","DEG_URB_r3","HATLEVEL_rSecon",
         "MAINSTAT_r21","AC1A_r2","AC1B_r2","AC1C_r2","HO3_r30","AGE_PERS6515-39_Non",
         "AM7_HO120_Non","PA2_r<=1A","PA34<=1A","FUMACTPASNon Fumeur")

freq(sante_fr2$SEX)

sante_fr2$AM1 <-relevel(sante_fr2$AM1,ref="3")
levels(sante_fr2$AM1)
sante_fr2$AM6A <-relevel(sante_fr2$AM6A,ref="2")
levels(sante_fr2$AM6A)
sante_fr2$AM6B <-relevel(sante_fr2$AM6B,ref="2")
levels(sante_fr2$AM6B)
sante_fr2$PA5 <-relevel(sante_fr2$PA5,ref="5")
levels(sante_fr2$PA5)
sante_fr2$PA6 <-relevel(sante_fr2$PA6,ref="5")
levels(sante_fr2$PA6)
sante_fr2$PE1 <-relevel(sante_fr2$PE1,ref="2")
levels(sante_fr2$PE1)
sante_fr2$PE2 <-relevel(sante_fr2$PE2,ref="0")
levels(sante_fr2$PE2)
sante_fr2$FV1 <-relevel(sante_fr2$FV1,ref="1")
levels(sante_fr2$FV1)
sante_fr2$DEG_URB_r <-relevel(sante_fr2$DEG_URB_r,ref="3")
levels(sante_fr2$DEG_URB_r)
sante_fr2$HATLEVEL_r <-relevel(sante_fr2$HATLEVEL_r,ref="Secon")
levels(sante_fr2$HATLEVEL_r)
sante_fr2$MAINSTAT_r <-relevel(sante_fr2$MAINSTAT_r,ref="21")
levels(sante_fr2$MAINSTAT_r)
sante_fr2$AC1A_r <-relevel(sante_fr2$AC1A_r,ref="2")
levels(sante_fr2$AC1A_r)
sante_fr2$AC1B_r <-relevel(sante_fr2$AC1B_r,ref="2")
levels(sante_fr2$AC1B_r)
sante_fr2$AC1C_r <-relevel(sante_fr2$AC1C_r,ref="2")
levels(sante_fr2$AC1C_r)
sante_fr2$AM7_HO12 <-relevel(sante_fr2$AM7_HO12,ref="0_Non")
levels(sante_fr2$AM7_HO12)
sante_fr2$FUMACTPAS <-relevel(sante_fr2$FUMACTPAS,ref="Non Fumeur")
levels(sante_fr2$FUMACTPAS)

names(sante_fr2)
v <- sante_fr2[,-c(2,20,35)]
v2 <- sante_fr2[,c(1,2,20,35)]
exogene <- "~" 
for (l in 1:length(v)){
  exogene <- paste(exogene,names(v[l]),"+") 
}
exogene <- as.formula(paste(substr(exogene,1,nchar(exogene)-2),"-1"))
exogene
sante_fr3 <- data.frame(model.matrix(exogene,data=sante_fr2))

if(length(unique(sante_fr3$PID))==length(unique(v2$PID))){
  sante_fr4 <- merge(sante_fr3,v2, by="PID")
  names(sante_fr4)
}

## Création des bases TEST et TRAIN
test_index <- createDataPartition(sante_fr4$etat_sante2, times=1, p=0.3, list=F)
train_set <- sante_fr4[-test_index,]
test_set <- sante_fr4[test_index,]
# Vérification de l'équi-distribution des classes entre l'échantillon test et train
freq(train_set$etat_sante2)
freq(test_set$etat_sante2)


### LOGIT MULTINOMIAL CUMULATIF AVEC VGAM
# names(sante_fr2)
# sante_lo <- polr(etat_sante2 ~ ., data = sante_fr2[,2:34],method = "logistic")
# summary(sante_lo)
# stepAIC(sante_lo)
# confint(sante_lo,level = 0.95)

# train(sante_fr2[,2:34],etat_sante2,method = 'vglmCumulative')
#Modèle avec la constante contrainte parallèle
# lsfc_i <- vglm(etat_sante2 ~ 1, data = sante_fr4, family = cumulative(parallel=TRUE,reverse=TRUE)) 
# summary(lsfc_i)
#modèle avec les variables + contrainte parallèle
#lsfcp <- vglm(etat_sante2 ~ ., data = sante_fr4[,c(2,4:100,102,103)], family = cumulative(parallel=TRUE,reverse=TRUE)) 
#r_lsfcp <- summary(lsfcp)

#sélection automatique de variables - backward
select_bkwd <- function(x,seuil=0.01,...){
#nombre de modalités de y
K <- length(unique(x$etat_sante2))
#indicateur si recherche doit continuer
recherche <- TRUE
#boucle de recherche
while (recherche == TRUE && length(x) > 1){
  #former la formule de régression
  str_formule <- paste("etat_sante2","~") 
  for (j in 2:length(x)-1){
    str_formule <- paste(str_formule,names(x[j]),"+") 
    }
  str_formule <- substr(str_formule,1,nchar(str_formule)-2)
  print(str_formule)
  # transformer en formule
  formule <- as.formula(str_formule)
  # lancer la régression
  logor_sante <- vglm(formule, data = x, ...) 
  # logor_sante <- vglm(formule, data = x, family = cumulative(parallel=TRUE,reverse=TRUE))
  summary.logor_sante <- summary(logor_sante) 
  # récupérer coefs, écarts-type et t-value 
  mat.coef <- attr(summary.logor_sante,"coef3") 
  #print(mat.coef)
  p.value <- mat.coef[,4]
  #print(p.value)
  # récupérer le max. et son indice
  p.max <- max(p.value[K:length(p.value)]) 
  index.max <- which.max(p.value[K:length(p.value)])
  #print(index.max)
  
  if (p.max > seuil){
  # non-significatif, on supprime la variable
    x <- x[,-index.max]
  } else
  {
    recherche <- FALSE
  } 
  }
#récupérer la dernière formule
formule <- as.formula(str_formule)
return (vglm(formule, data = x, ...))
}
print(str_formule)
#modèle avec sélection de variables
losante <- select_bkwd(x=train_set[,c(2,4:100,102,103)],family = cumulative(parallel=TRUE,reverse=TRUE)) 
s.losante <- summary(losante)
# coef.losante <- attr(s.losante,"coef3")[,1]
# coef.losante <- coef.losante[-c(1,2)]
# names(coef.losante)
# test_set2 <- test_set[,names(coef.losante)]

new <- as.data.frame(predictvglm(losante, test_set2, type = "response"))

for (i in 1:nrow(new)) {
    new$prob[i] <- max(new[i,])
}

for (i in 1:nrow(new)) {
for (j in 1:3) {
  if(new$prob[i] == new[i,j]){
    new$class[i] <- j-1
  }
}
}

pred <- cbind(ID =rownames(new),new)
obs <- cbind(ID = rownames(test_set),test_set)
test <- merge(obs,pred,"ID")

# Matrice de confusion
table(test$class,test$etat_sante2)

# Table de confusion pour 1
test$class1 <- ifelse(test$class==1,"MAUV","MODBN")
test$etat_sante21 <- ifelse(test$etat_sante2==1,"MAUV","MODBN")
m0 <- as.matrix(table(test$class1,test$etat_sante21))
m0
Tx_erreur1 <- (m0[1,2] + m0[2,1])/sum(m0)
Tx_erreur1
Sensibilite1 <- (m0[1,1])/(m0[1,1] + m0[2,1])
Sensibilite1 
Specificite1 <- (m0[1,2])/(m0[1,2] + m0[2,2])
Specificite1

# Table de confusion pour 2
test$class2 <- ifelse(test$class==2,"MAUV","MODBN")
test$etat_sante22 <- ifelse(test$etat_sante2==2,"MAUV","MODBN")
m0 <- as.matrix(table(test$class2,test$etat_sante22))
m0
Tx_erreur2 <- (m0[1,2] + m0[2,1])/sum(m0)
Tx_erreur2
Sensibilite2 <- (m0[1,1])/(m0[1,1] + m0[2,1])
Sensibilite2 
Specificite2 <- (m0[1,2])/(m0[1,2] + m0[2,2])
Specificite2


############# PREDICTION GLOBALE ################
#modèle avec sélection de variables
losante_G <- select_bkwd(x=sante_fr4[,c(2,4:100,102,103)],family = cumulative(parallel=TRUE,reverse=TRUE)) 
s.losante_G <- summary(losante_G)
pred.losante_G <- as.data.frame(predictvglm(losante_G, sante_fr4, type = "response"))

# Affectation des probas
for (i in 1:nrow(pred.losante_G)) {
  pred.losante_G$prob[i] <- max(pred.losante_G[i,])
}

# Affectation des classes
for (i in 1:nrow(pred.losante_G)) {
  for (j in 1:3) {
    if(pred.losante_G$prob[i] == pred.losante_G[i,j]){
      pred.losante_G$class[i] <- j-1
    }
  }
}

pred2 <- cbind(ID =rownames(pred.losante_G),pred.losante_G)
obs2 <- cbind(ID = rownames(sante_fr4),sante_fr4)
pred.sante_FR <- merge(obs2,pred2,"ID")

# Dernière version avec scores
write_delim(pred.sante_FR,path="SANTE_FR_SCORE",delim=";")


