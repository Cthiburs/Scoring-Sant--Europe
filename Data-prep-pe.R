##*## Ce script permet de préparer les données en vue de la modélisation ##*##
## Vérification du chemin du répertoire courant
getwd()

### CHARGEMENT LIBRARIES
library(tidyverse)
library(googledrive) #-- chargement de l'API de google Drive
library(RCurl)
library(questionr)
library(labelled)
library(bnstruct)
library(VIM)


### DATA IMPORT VIA GOOGLE DRIVE
drive_find(n_max = 5,type="csv")
sante <- drive_download("BDD_SANTE.csv",overwrite = T)  # Pour télécharger la base
read_lines("BDD_SANTE.csv",n_max=2)
sante_eu <- read_delim("BDD_SANTE.csv",delim = ";")
glimpse(sante_eu)

# Vérifier la virgule de l'imc, du poids

### DATA WRANGLING (On regarde le tableau de fréquence des données)

sante_eu %>% group_by(COUNTRY) %>% count(REFYEAR) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="REFYEAR",value=prop)   # Supprimer les pays AT, HR, DE, cause ENQUÊTE EFFECTUÉE en 2013 ou 2015. Supprimer ou considérer pour la FR, CZ et SE les individus de 2015.

sante_eu %>% group_by(COUNTRY) %>% count(JOBSTAT) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="JOBSTAT",value=prop) # Fusionner MAINSTAT et JOBSTAT d'une part puis MAINSTAT et JOBSTAT d'autres pour tester leur corrélation avec etat de santé

sante_eu %>% group_by(COUNTRY) %>% count(FT_PT) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="FT_PT",value=prop) # Supprimé car dépendant intrinsèquement de MAINSTAT  

sante_eu %>% group_by(COUNTRY) %>% count(locnace) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="locnace",value=prop) # Supprimé car lié à JOBISCO

sante_eu %>% group_by(COUNTRY) %>% count(BIRTHPLACE) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="BIRTHPLACE",value=prop)    # Suppression de Birthplace à cause de la France (16% de DM)

sante_eu %>% group_by(COUNTRY) %>% count(HHNBPERS_65PLUS) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="HHNBPERS_65PLUS",value=prop) # Transformer cette variable en oui/Non

sante_eu %>% group_by(COUNTRY) %>% count(HH_ACT) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="HH_ACT",value=prop)  # Supprimé la HR et SI car 87% de DM et le reste pas d'acivité pour cette variable, alors que 36 % sont employés d'après . Même chose pour la SI avec 48 %

sante_eu %>% group_by(COUNTRY) %>% count(HH_INACT) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="HH_INACT",value=prop) # Supprimé car HH_ACT gardé

sante_eu %>% group_by(COUNTRY) %>% count(HHINCOME) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="HHINCOME",value=prop) # HR supprimé car 35% de DM, DK -19%, EE -26%, LU -27%, PL -19%, SI - 23%

sante_eu %>% group_by(COUNTRY) %>% count(AC1A) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="AC1A",value=prop) # Supprimé SE car 15 % DM

sante_eu %>% group_by(COUNTRY) %>% count(AC2) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="AC2",value=prop)  # Supprimé car très lié à AC1A et moins de 10% en général

sante_eu %>% group_by(COUNTRY) %>% count(HO2) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="HO2",value=prop)  # Fusionné H01 et H02 puis H03 et H04

sante_eu %>% group_by(COUNTRY) %>% count(PA7) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="PA7",value=prop) # PA7 et PA8 supprimés car ne concerne que les femmes, très lié donc au sexe.
# calculer IMC à base de BM1 et BM2

sante_eu %>% group_by(COUNTRY) %>% count(SK2) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="SK2",value=prop) # Supprimé SK2 et SK3 car lié à SK1 et ns informes que les gens consomment gprincipalement la cigarette

sante_eu %>% group_by(COUNTRY) %>% count(AL1) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="AL1",value=prop)  # supprimé car variable non dispo pour la FR et l'IT

sante_eu %>% group_by(COUNTRY) %>% count(etat_sante) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="etat_sante",value=prop) 

sante_eu_N <- sante_eu %>% select(-c(contains("CD1"),contains("MH1"),"HS1","HS3","PN1","score_etat_sante":"douleur_absente",
                                     "REFMONTH","SK2","AL1","AL3","AL5","AL6","SK3","BM1","BM2","PN2","AM3","AM5","AC2","PROXY","BIRTHPLACE","FT_PT","locnace","HH_INACT"))

# Extraction des données de chaque pays
pays <- as.matrix(sante_eu_N %>% group_by(COUNTRY) %>% summarise(n()) %>% select(COUNTRY))
for (i in pays) {
  assign(paste0("sante_",i), sante_eu_N %>% filter(COUNTRY == i))
}
head(sante_FR)

# DATA PREP SUR LA BASE SANTE_FR
glimpse(sante_FR)
freq(sante_FR$REFYEAR)
 sante_FR <- sante_FR %>% filter(REFYEAR==2014)

# Age
freq(sante_FR$age)   
class(sante_FR$age)
lprop(table(sante_FR$age,sante_FR$etat_sante))  # Regrouger l'âge en trois classes comme dans le rapport de l'irdes http://www.irdes.fr/recherche/rapports/566-enquete-sante-europeenne-ehis-enquete-sante-et-protection-sociale-esps-2014.pdf
chisq.test(table(sante_FR$age,sante_FR$etat_sante))
chisq.residuals(table(sante_FR$age,sante_FR$etat_sante))
mosaicplot(table(sante_FR$age,sante_FR$etat_sante),las=3,shade=TRUE) # confirme le choix de trois classes
sante_FR$age_r <- fct_collapse(sante_FR$age, "15-39"=c("15-17","18-19","20-24","25-29","30-34","35-39"),
                               "40-64"=c("40-44","45-49","50-54","55-59","60-64"),
                               "65+"=c("65-69","70-74","75-79","80-84","85+"))
chisq.test(table(sante_FR$age_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$age_r,sante_FR$etat_sante),las=3,shade=TRUE)                                  
freq(sante_FR$age_r)

# Degré d'urbanisation DEG_URB   Variable à supprimer car pas lié à état de santé
freq(sante_FR$DEG_URB)  
class(sante_FR$DEG_URB)
sante_FR$DEG_URB <- as.character(sante_FR$DEG_URB)
sante_FR$DEG_URB_r <- as.character(fct_recode(sante_FR$DEG_URB, "3"="-1"))  # Imputer par le mode
sante_FR$DEG_URB_r <- labelled(sante_FR$DEG_URB_r,c("Densely-populated area"="1","Intermediate-populated area"="2",
                                                    "Thinly-populated area"="3"))
freq(sante_FR$DEG_URB_r)
chisq.test(table(sante_FR$DEG_URB_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$DEG_URB_r,sante_FR$etat_sante),las=3,shade=TRUE)

# MARSTALEGAL : Etat matrimonial légal 
freq(sante_FR$MARSTALEGAL)  
class(sante_FR$MARSTALEGAL)
sante_FR$MARSTALEGAL <- as.character(sante_FR$MARSTALEGAL)
sante_FR$MARSTALEGAL_r <- as.character(fct_recode(sante_FR$MARSTALEGAL, "2"="-1"))  # Imputer par le mode
freq(sante_FR$MARSTALEGAL_r)
chisq.test(table(sante_FR$MARSTALEGAL_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$MARSTALEGAL_r,sante_FR$etat_sante),las=3,shade=TRUE)

# HATLEVEL : Niveau de diplôme le plus élevé achevé
freq(sante_FR$HATLEVEL)  
class(sante_FR$HATLEVEL)
sante_FR$HATLEVEL <- as.character(sante_FR$HATLEVEL)
sante_FR$HATLEVEL <- fct_collapse(sante_FR$HATLEVEL, "Prim"=c("0","1"), "Secon"=c("2","3","4"),
                                  "Univ"=c("5","6","7","8"))
sante_FR$HATLEVEL_r <- as.character(fct_recode(sante_FR$HATLEVEL, "Secon"="-1"))  # Imputer par le mode
freq(sante_FR$HATLEVEL_r)
chisq.test(table(sante_FR$HATLEVEL_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$HATLEVEL_r,sante_FR$etat_sante),las=3,shade=TRUE)

# Statut travailleur Auto déclaré (MAINSTAT) X Statut dans l'emploi (JOBSTAT) X Profession dans l'emploi (JOBISCO)
freq(sante_FR$MAINSTAT)
freq(sante_FR$JOBISCO)
freq(sante_FR$JOBSTAT)
class(sante_FR$MAINSTAT)
class(sante_FR$JOBISCO)
class(sante_FR$JOBSTAT)
sante_FR$MAINSTAT  <- as.character(sante_FR$MAINSTAT)
sante_FR$JOBSTAT <- as.character(sante_FR$JOBSTAT)
sante_FR$MAINSTAT2 <- sante_FR$MAINSTAT
table(sante_FR$MAINSTAT,sante_FR$JOBSTAT)
sante_FR$MAINSTAT2 <- ifelse(sante_FR$MAINSTAT2=="10",sante_FR$JOBSTAT[sante_FR$JOBSTAT!="-2"],sante_FR$MAINSTAT2[sante_FR$MAINSTAT2!="10"])
freq(sante_FR$MAINSTAT2)
sante_FR$MAINSTAT_r <- fct_recode(sante_FR$MAINSTAT2,"21"="-1")
freq(sante_FR$MAINSTAT_r)
chisq.test(table(sante_FR$MAINSTAT_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$MAINSTAT_r,sante_FR$etat_sante), las=3, shade = T)

# HHNBPERS : Nombre de personnes vivant dans le ménage, y compris le répondant
freq(sante_FR$HHNBPERS)
chisq.test(table(sante_FR$HHNBPERS,sante_FR$etat_sante))
mosaicplot(table(sante_FR$HHNBPERS,sante_FR$etat_sante), las=3, shade = T)

# HHNBPERS_65PLUS : Nombre de personnes âgées de 65 ans et plus
freq(sante_FR$HHNBPERS_65PLUS)
sante_FR$HHNBPERS_65PLUS <- as.character(sante_FR$HHNBPERS_65PLUS)
class(sante_FR$HHNBPERS_65PLUS)
table(sante_FR$HHNBPERS_65PLUS,sante_FR$age)
sante_FR$HHNBPERS_65PLUS_r <- ifelse(sante_FR$age %in% c("65-69","70-74","75-79","80-84","85+") & sante_FR$HHNBPERS_65PLUS=="-1","1",sante_FR$HHNBPERS_65PLUS)
sante_FR$HHNBPERS_65PLUS_r <- fct_collapse(sante_FR$HHNBPERS_65PLUS_r, "Non"=c("-1","0"), "Oui"=c("1","2","3"))
freq(sante_FR$HHNBPERS_65PLUS_r)
chisq.test(table(sante_FR$HHNBPERS_65PLUS_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$HHNBPERS_65PLUS_r,sante_FR$etat_sante), las=3, shade = T)
chisq.test(table(sante_FR$HHNBPERS_65PLUS_r,sante_FR$HHNBPERS))
mosaicplot(table(sante_FR$HHNBPERS_65PLUS_r,sante_FR$HHNBPERS), las=3, shade = T) # les deux variables sont liées

# HHTYPE : Type de ménage
freq(sante_FR$hhtype)
chisq.test(table(sante_FR$hhtype,sante_FR$etat_sante))
mosaicplot(table(sante_FR$hhtype,sante_FR$etat_sante), las=3, shade = T)

# HH_ACT : Nombre de personnes âgées de 16 à 64 dans le ménage qui sont employées
freq(sante_FR$HH_ACT)
class(sante_FR$HH_ACT)
sante_FR$HH_ACT <- as.character(sante_FR$HH_ACT)
sante_FR$HH_ACT <- fct_collapse(sante_FR$HH_ACT,"0"="-1","3+"=c("3","4","5"))
chisq.test(table(sante_FR$HH_ACT,sante_FR$etat_sante))
mosaicplot(table(sante_FR$HH_ACT,sante_FR$etat_sante), las=3, shade = T)

# HHINCOME : Revenu mensuel net du ménage
freq(sante_FR$HHINCOME)  # imputation plus proche voisin ou multiple



# AC1A : Accident de la route survenu au cours des 12 derniers mois
freq(sante_FR$AC1A)
sante_FR$AC1A <- as.character(sante_FR$AC1A)
sante_FR$AC1A_r <- fct_recode(sante_FR$AC1A,"2"="-1")
freq(sante_FR$AC1A_r)

# AC1B : Accident à domicile au cours des 12 derniers mois
freq(sante_FR$AC1B)
sante_FR$AC1B <- as.character(sante_FR$AC1B)
sante_FR$AC1B_r <- fct_recode(sante_FR$AC1B,"2"="-1")
freq(sante_FR$AC1B_r)

# AC1C : Accident de loisir au cours des 12 derniers mois
freq(sante_FR$AC1C)
sante_FR$AC1C <- as.character(sante_FR$AC1C)
sante_FR$AC1C_r <- fct_recode(sante_FR$AC1C,"2"="-1")
freq(sante_FR$AC1C_r)

# HO1 : Admission en hospitalisation à l'hôpital au cours des 12 derniers mois
freq(sante_FR$HO1)
sante_FR$HO1 <- as.character(sante_FR$HO1)
sante_FR$HO1_r <- fct_collapse(sante_FR$HO1,"N"=c("-1","2"),"O"="1")
freq(sante_FR$HO1_r)

# HO1 X HO2 : Nombre de nuits passées en tant que patient dans un hôpital au cours des 12 derniers mois
freq(sante_FR$HO2)
sante_FR$HO2  <- as.character(sante_FR$HO2)
sante_FR$HO1_r2 <- as.character(sante_FR$HO1_r)
freq(sante_FR$HO1_r2)
class(sante_FR$HO1_r2)
table(sante_FR$HO2,sante_FR$HO1_r2)
sante_FR$HO1_r2 <- ifelse(sante_FR$HO1_r2=="O",sante_FR$HO2[sante_FR$HO2!="-2"],sante_FR$HO1_r2[sante_FR$HO1_r2=="N"])
freq(sante_FR$HO1_r2)
sante_FR$HO1_r2 <- fct_collapse(sante_FR$HO1_r2,"0"=c("-1","N")) 
sante_FR$HO1_r2 <- as.integer(as.character(sante_FR$HO1_r2))
sante_FR$HO1_r3 <- cut(sante_FR$HO1_r2, breaks = c(-1,0,5,210))
freq(sante_FR$HO1_r3)
sante_FR$HO1_r3 <- fct_collapse(sante_FR$HO1_r3,"0"="(-1,0]","[1;5]"="(0,5]","[6;210]"="(5,210]")
chisq.test(table(sante_FR$HO1_r3 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$HO1_r3 ,sante_FR$etat_sante), las=3, shade = T)

# HO3 X HO4 : Nombre de fois admis en tant que patient de jour dans un hôpital au cours des 12 derniers mois
freq(sante_FR$HO3)
sante_FR$HO3 <- as.character(sante_FR$HO3)
sante_FR$HO3_r <- fct_collapse(sante_FR$HO3,"N"=c("-1","2"),"O"="1")
freq(sante_FR$HO3_r)
freq(sante_FR$HO4)
sante_FR$HO4  <- as.character(sante_FR$HO4)
sante_FR$HO3_r2 <- as.character(sante_FR$HO3_r)
freq(sante_FR$HO3_r2)
class(sante_FR$HO3_r2)
table(sante_FR$HO4,sante_FR$HO3_r2)
sante_FR$HO3_r2 <- ifelse(sante_FR$HO3_r2=="O",sante_FR$HO4[sante_FR$HO4!="-2"],sante_FR$HO3_r2[sante_FR$HO3_r2=="N"])
freq(sante_FR$HO3_r2)
sante_FR$HO3_r2 <- fct_collapse(sante_FR$HO3_r2,"0"=c("-1","N")) 
sante_FR$HO3_r3 <- fct_collapse(sante_FR$HO3_r2,"2-3"=c("2","3")) 
freq(sante_FR$HO3_r3)
chisq.test(table(sante_FR$HO3_r3 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$HO3_r3 ,sante_FR$etat_sante), las=3, shade = T)

# AM1 : Dernière visite chez le dentiste ou l'orthodentiste 
freq(sante_FR$AM1)
sante_FR$AM1 <- as.character(sante_FR$AM1)
sante_FR$AM1 <- fct_recode(sante_FR$AM1, "3"="-1")
chisq.test(table(sante_FR$AM1 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$AM1 ,sante_FR$etat_sante), las=3, shade = T)

# AM2 : Dernière consultation chez un médecin généraliste ou médecin de famille 
freq(sante_FR$AM2)
sante_FR$AM2 <- as.character(sante_FR$AM2)
sante_FR$AM2 <- fct_recode(sante_FR$AM2, "1"="-1")
chisq.test(table(sante_FR$AM2 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$AM2 ,sante_FR$etat_sante), las=3, shade = T)

# AM4 : Dernière consultation chez un médecin ou chirurgien spécialiste 
freq(sante_FR$AM4)
sante_FR$AM4 <- as.character(sante_FR$AM4)
sante_FR$AM4 <- fct_recode(sante_FR$AM4, "1"="-1")
chisq.test(table(sante_FR$AM4 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$AM4 ,sante_FR$etat_sante), las=3, shade = T)

# AM6a : Consultation d'un physiothérapeute ou d'un kinésithérapeute au cours des 12 derniers mois
freq(sante_FR$AM6A)
sante_FR$AM6A <- as.character(sante_FR$AM6A)
sante_FR$AM6A <- fct_recode(sante_FR$AM6A, "1"="-1")
chisq.test(table(sante_FR$AM6A ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$AM6A ,sante_FR$etat_sante), las=3, shade = T)

# AM6b : Consultation d'un psychologue ou d'un psychothérapeute au cours des 12 derniers mois
freq(sante_FR$AM6B)
sante_FR$AM6B <- as.character(sante_FR$AM6B)
sante_FR$AM6B <- fct_recode(sante_FR$AM6B, "2"="-1")
chisq.test(table(sante_FR$AM6B ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$AM6B ,sante_FR$etat_sante), las=3, shade = T)

# AM7 : Utilisation de services de soins à domicile pour des besoins personnels au cours des 12 derniers mois
freq(sante_FR$AM7)
sante_FR$AM7 <- as.character(sante_FR$AM7)
sante_FR$AM7 <- fct_recode(sante_FR$AM7, "2"="-1")
chisq.test(table(sante_FR$AM7 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$AM7 ,sante_FR$etat_sante), las=3, shade = T)

# MD1 : Utilisation de médicament prescrit par un médecin durant les deux dernieres semaines 
freq(sante_FR$MD1)
sante_FR$MD1 <- as.character(sante_FR$MD1)
sante_FR$MD1 <- fct_recode(sante_FR$MD1, "1"="-1")
chisq.test(table(sante_FR$MD1 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$MD1 ,sante_FR$etat_sante), las=3, shade = T)

# MD2 : Utilisation de médicaments, médicaments à base de plantes ou vitamines non prescrits par un médecin au cours des deux dernières semaines
freq(sante_FR$MD2)
sante_FR$MD2 <- as.character(sante_FR$MD2)
sante_FR$MD2 <- fct_recode(sante_FR$MD2, "2"="-1")
chisq.test(table(sante_FR$MD2 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$MD2 ,sante_FR$etat_sante), las=3, shade = T)

# PA1 : Dernière vaccination contre la grippe Variable à comprendre sinon supprimer
freq(sante_FR$pa1)  # Pas bien renseigner les dates


# PA2 : Dernière mesure de la tension artérielle par un professionnel de la santé
freq(sante_FR$PA2)
sante_FR$PA2 <- as.character(sante_FR$PA2)
sante_FR$PA2 <- fct_recode(sante_FR$PA2, "1"="-1")
chisq.test(table(sante_FR$PA2 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PA2 ,sante_FR$etat_sante), las=3, shade = T)


# PA3 : Dernière mesure du taux de cholestérol sanguin par un professionnel de la santé
freq(sante_FR$PA3)
sante_FR$PA3 <- as.character(sante_FR$PA3)
sante_FR$PA3 <- fct_recode(sante_FR$PA3, "1"="-1")
chisq.test(table(sante_FR$PA3 ,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PA3 ,sante_FR$etat_sante), las=3, shade = T)


# PA4 : Dernière mesure de la glycémie par un professionnel de la santé
freq(sante_FR$PA4)
sante_FR$PA4 <- as.character(sante_FR$PA4)
sante_FR$PA4 <- fct_recode(sante_FR$PA4, "1"="-1")
chisq.test(table(sante_FR$PA4,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PA4,sante_FR$etat_sante), las=3, shade = T)

# PA5 : La dernière fois d'une analyse de sang occulte dans les selles
freq(sante_FR$PA5)
sante_FR$PA5 <- as.character(sante_FR$PA5)
sante_FR$PA5 <- fct_recode(sante_FR$PA5, "5"="-1")
chisq.test(table(sante_FR$PA5,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PA5,sante_FR$etat_sante), las=3, shade = T)

# PA6 : Dernière coloscopie
freq(sante_FR$PA6)
sante_FR$PA6 <- as.character(sante_FR$PA6)
sante_FR$PA6 <- fct_recode(sante_FR$PA6, "5"="-1")
chisq.test(table(sante_FR$PA5,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PA5,sante_FR$etat_sante), las=3, shade = T)

# UN1A : Besoins non satisfaits en matière de soins de santé au cours des 12 derniers mois en raison des longues listes d'attente
freq(sante_FR$UN1A)  # Supprimer cette variable cause DM 49%
freq(sante_FR$UN1B)   # Supprimer cette variable cause DM 49%
freq(sante_FR$UN2A)   # Supprimer cette variable cause DM 49%
freq(sante_FR$UN2B)   # Supprimer cette variable cause DM 49%
freq(sante_FR$UN2C)   # Supprimer cette variable cause DM 49%


# PE1 : Effort physique au travail (rémunérées et non rémunérées activités de travail incluses)
freq(sante_FR$PE1)
sante_FR$PE1 <- as.character(sante_FR$PE1)
sante_FR$PE1 <- fct_recode(sante_FR$PE1, "2"="-1")
chisq.test(table(sante_FR$PE1,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PE1,sante_FR$etat_sante), las=3, shade = T)

# PE2 : Nombre de jours par semaine à réaliser des trajets à pied d'au moins 10 min sans interruption
freq(sante_FR$PE2)
sante_FR$PE2 <- as.character(sante_FR$PE2)
sante_FR$PE2 <- fct_recode(sante_FR$PE2, "0"="-1")
chisq.test(table(sante_FR$PE2,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PE2,sante_FR$etat_sante), las=3, shade = T)

# PE4 : Nombre de jours par semaine à réaliser des trajets en vélo d'au moins 10 min sans interruption
freq(sante_FR$PE4)
sante_FR$PE4 <- as.character(sante_FR$PE4)
sante_FR$PE4 <- fct_recode(sante_FR$PE4, "0"="-1")
chisq.test(table(sante_FR$PE4,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PE4,sante_FR$etat_sante), las=3, shade = T)

# PE6 : Nombre de jour par semaine à pratiquer une activité physique d'au moins 10min sans interruption
freq(sante_FR$PE6)
sante_FR$PE6 <- as.character(sante_FR$PE6)
sante_FR$PE6 <- fct_recode(sante_FR$PE6, "0"="-1")
chisq.test(table(sante_FR$PE6,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PE6,sante_FR$etat_sante), las=3, shade = T)

# PE 8 : Nombre de jour par semaine à faire du renforcement musculaire
freq(sante_FR$PE8)
sante_FR$PE8 <- as.character(sante_FR$PE8)
sante_FR$PE8 <- fct_recode(sante_FR$PE8, "0"="-1")
chisq.test(table(sante_FR$PE8,sante_FR$etat_sante))
mosaicplot(table(sante_FR$PE8,sante_FR$etat_sante), las=3, shade = T)

# FV1 : A quelle fréquence consommez- vous des fruits par semaine (jus de fruit inclus)
freq(sante_FR$FV1)
sante_FR$FV1 <- as.character(sante_FR$FV1)
sante_FR$FV1 <- fct_recode(sante_FR$FV1, "0"="-1")
chisq.test(table(sante_FR$FV1,sante_FR$etat_sante))
mosaicplot(table(sante_FR$FV1,sante_FR$etat_sante), las=3, shade = T)

# FV3 : A quelle fréquence consommez- vous des légumes ou de la salade par semaine (exclusion des jus et des pomme de terre)
freq(sante_FR$FV3)
sante_FR$FV3 <- as.character(sante_FR$FV3)
sante_FR$FV3 <- fct_recode(sante_FR$FV3, "0"="-1")
chisq.test(table(sante_FR$FV3,sante_FR$etat_sante))
mosaicplot(table(sante_FR$FV3,sante_FR$etat_sante), las=3, shade = T)

# SK1 : Type de consommateur de tabac
freq(sante_FR$SK1)
sante_FR$SK1 <- as.character(sante_FR$SK1)
sante_FR$SK1 <- fct_recode(sante_FR$SK1, "3"="-1")
chisq.test(table(sante_FR$SK1,sante_FR$etat_sante))
mosaicplot(table(sante_FR$SK1,sante_FR$etat_sante), las=3, shade = T)

# SK4 : Fréquence d'exposition à la fumé de tabac 
table(sante_FR$SK1,sante_FR$SK4)
freq(sante_FR$SK4)
sante_FR$SK4 <- as.character(sante_FR$SK4)
sante_FR$SK4_r <- if_else(sante_FR$SK4=="-1" & sante_FR$SK1=="1","3",
                          if_else(sante_FR$SK4=="-1" & sante_FR$SK1=="2","2",
                                 if_else(sante_FR$SK4=="-1" & sante_FR$SK1=="3","1",sante_FR$SK4)))
table(sante_FR$SK1,sante_FR$SK4_r)

# IC1 : Fournissez-vous de l'aide à des personnes agées, malades,… au moins une fois par semaine ?
freq(sante_FR$IC1) # Variable à supprimer

# nb_maladie
freq(sante_FR$nb_maladiegrave)
sante_FR$nb_maladiegrave <- as.character(sante_FR$nb_maladiegrave)
sante_FR$nb_maladiegrave <- fct_collapse(sante_FR$nb_maladiegrave, "3+"=c("3","4","5","6","7","8"))
freq(sante_FR$nb_maladiegrave)

freq(sante_FR$nb_maladiepasgrave)
sante_FR$nb_maladiepasgrave <- as.character(sante_FR$nb_maladiepasgrave)
sante_FR$nb_maladiepasgrave <- fct_collapse(sante_FR$nb_maladiepasgrave, "4+"=c("4","5","6","7"))
freq(sante_FR$nb_maladiepasgrave)

###### Base Finale V1
sante_FR2 <- sante_FR %>% select(-c("REFYEAR","HO2","IC1","UN1B","UN2A","UN2B","UN2C","UN2D","HO4","pa1","PA7","PA8","UN1A","JOBISCO","JOBSTAT","COUNTRY","age","DEG_URB","MARSTALEGAL","HATLEVEL","MAINSTAT","MAINSTAT2","HHNBPERS_65PLUS","AC1A","AC1B","AC1C","HO1","HO1_r","HO1_r2","HO3","HO3_r","HO3_r2"))

dim(sante_FR2)
glimpse(sante_FR2)

### CORRELATION QUALI : V DE CRAMER
var <- as.matrix(colnames(sante_FR2))
var <- var[-match(c("PID","WGT","nb_maladiepasgrave","nb_maladiegrave"),table=var[,1]),]
var
class(var)
cram_V <- matrix(0,length(var),length(var),dimnames = list(var,var))
cram_V
#cram_V[-match("SEX",table=var),]
cram_V2 <- cram_V

for (i in var) {
  for (j in var) {
    y <- sante_FR2 %>% select(c(i,j))
    cram_V[i,j] <- ifelse(i==j,assign(paste0("cram_",i,j), 1), assign(paste0("cram_",i,j), cramer.v(table(y[[1]],y[[2]]))))
  cram_V2[i,j] <- ifelse(cram_V[i,j]>=0.3,cram_V[i,j],0)
    }
}

cram_V3 <- cram_V2
for (i in var) {
  if(sum(cram_V2[,i])==1) {
  cram_V3 <- cram_V3[-match(i,table=var),]
  cram_V3 <- cram_V3[,-match(i,table=var)]
  }
}


# Créer une interaction entre age_r et HHNBPERS_65_plus
table(sante_FR2$age_r,sante_FR2$HHNBPERS_65PLUS_r)
sante_FR2$AGE_PERS65 <- if_else(sante_FR2$age_r=="15-39" & sante_FR2$HHNBPERS_65PLUS_r=="Non","15-39_Non",
                                if_else(sante_FR2$age_r=="15-39" & sante_FR2$HHNBPERS_65PLUS_r=="Oui","15-39_Oui",
                                        if_else(sante_FR2$age_r=="40-64" & sante_FR2$HHNBPERS_65PLUS_r=="Non","40-64_Non",
                                                if_else(sante_FR2$age_r=="40-64" & sante_FR2$HHNBPERS_65PLUS_r=="Oui","40-64_Oui", "65+_Oui"))))
freq(sante_FR2$AGE_PERS65)

# Supprimer AM7 et HO1_r3 et créer une intercation (demander explication lydie,depret,diana)
table(sante_FR2$AM7,sante_FR2$HO1_r3)
sante_FR2$AM7_HO12 <- if_else(sante_FR2$AM7=="2" & sante_FR2$HO1_r3=="0","0_Non",
                              if_else(sante_FR2$AM7=="2" & sante_FR2$HO1_r3=="[1;5]","[1;5]_Non",
                                      if_else(sante_FR2$AM7=="2" & sante_FR2$HO1_r3=="[6;210]","[6;210]_Non",
                                              if_else(sante_FR2$AM7=="1" & sante_FR2$HO1_r3=="0","0_Oui",
                                                      if_else(sante_FR2$AM7=="1" & sante_FR2$HO1_r3=="[1;5]","[1;5]_Oui","[6;210]_Oui")))))
freq(sante_FR2$AM7_HO12)
chisq.test(table(sante_FR2$AM7_HO12,sante_FR2$etat_sante))
mosaicplot(table(sante_FR2$AM7_HO12,sante_FR2$etat_sante),las = 3, shade = T)

# Créer interaction PA3 X PA4 et recoder PA2
table(sante_FR2$PA3,sante_FR2$PA4)
sante_FR2$PA3_r <- fct_collapse(sante_FR2$PA3, "<=1A"="1",">1A"=c("2","3","4"),"Jamais"="5")
freq(sante_FR2$PA3_r)
sante_FR2$PA4_r <- fct_collapse(sante_FR2$PA4, "<=1A"="1",">1A"=c("2","3","4"),"Jamais"="5")
freq(sante_FR2$PA4_r)
sante_FR2$PA2_r <- fct_collapse(sante_FR2$PA2, "<=1A"="1",">1A"=c("2","3","4"),"Jamais"="5")
freq(sante_FR2$PA2_r)
table(sante_FR2$PA3_r,sante_FR2$PA4_r)
cramer.v(table(sante_FR2$PA3_r,sante_FR2$PA4_r))

sante_FR2$PA34 <- if_else(sante_FR2$PA3_r=="<=1A" & sante_FR2$PA4_r=="<=1A","<=1A",
                          if_else(sante_FR2$PA3_r==">1A" & sante_FR2$PA4_r==">1A",">1A",
                                  if_else(sante_FR2$PA3_r=="Jamais" & sante_FR2$PA4_r=="Jamais","Jamais","Autres")))
freq(sante_FR2$PA34)
cramer.v(table(sante_FR2$PA34,sante_FR2$etat_sante))                              
                              
# créer une interaction SK1 X SK4, regrouper SK1 en fumeur actif Oui/Non et SK4 en fumeur passif Oui/Non
prop(table(sante_FR2$SK1,sante_FR2$SK4_r))
freq(sante_FR2$SK1)
freq(sante_FR2$SK4_r)
sante_FR2$FUMACT <- fct_collapse(sante_FR2$SK1, "Oui"=c("1","2"),"Non"="3")
sante_FR2$FUMPAS <- fct_collapse(sante_FR2$SK4_r, "Oui"=c("3","2"),"Non"="1")
freq(sante_FR2$FUMACT)
freq(sante_FR2$FUMPAS)
prop(table(sante_FR2$FUMACT,sante_FR2$FUMPAS))

sante_FR2$FUMACTPAS <- if_else(sante_FR2$FUMACT=="Oui" & sante_FR2$FUMPAS=="Oui","Fumeur Actif et Passif",
                               if_else(sante_FR2$FUMACT=="Non" & sante_FR2$FUMPAS=="Non","Non Fumeur",
                                       if_else(sante_FR2$FUMACT=="Oui" & sante_FR2$FUMPAS=="Non","Fumeur Actif","Fumeur Passif")))
freq(sante_FR2$FUMACTPAS)
cramer.v(table(sante_FR2$FUMACTPAS,sante_FR2$etat_sante)) 



# Supprimer HHNBPERS, hhtype et garder HHNBPERS_65_plus car les trois sont liés (0.46) et hhnbpers65 est plus lié à l'état santé que les autres (0.3 VS 0.17 VS 0.18)
# Supprimer HH_ACT car lié à l'âge et à l'IMC(0.4), Supprimer AM2 car lié à MD1 et PA2. Supprimer PA2, PA3 et PA4 car lié à MD1-age_r et très lié entre elles. Supprimer PE8 et garder PE6 car elle est plus liée à etat santé (0.16 VS 0.10). Supprimer FV3 et garder FV1 car plus liée à etat de santé (0.06 VS 0.04)
##### BASE FINALE V03
glimpse(sante_FR2)
sante_FR3 <- sante_FR2 %>% select(-c("PID","HHNBPERS","AM2","AM7","PA4","PA3","PA2","PE8","FV3","SK1","SK4","age_r","MARSTALEGAL_r","HHNBPERS_65PLUS_r","HO1_r3","SK4_r","PA4_r","PA3_r","FUMPAS","FUMACT"))
glimpse(sante_FR3)
dim(sante_FR3)
freq(sante_FR3$HHINCOME)
class(sante_FR3$HHINCOME)
sante_FR3$HHINCOME <- as.factor(sante_FR3$HHINCOME)
sante_FR3$SEX <- as.factor(sante_FR3$SEX)
sante_FR3$hhtype <- as.factor(sante_FR3$hhtype)
sante_FR3$HHINCOME<- fct_recode(sante_FR3$HHINCOME,NULL="-1")
freq(sante_FR3$HHINCOME)
sante_FR33 <- sante_FR3[,-c(1,19)]
nam <- names(sante_FR3)
nam <- nam[-c(1,5,19)]
nam
#sante_FR4 <-  knn.impute(sante_FR3,k=10, cat.var = nam,to.impute = "HHINCOME",using = nam )
#freq(sante_FR3$HHINCOME)
sante_FR4 <- kNN(sante_FR3, variable="HHINCOME", k=10, dist_var = nam,catFun=maxCat)
freq(sante_FR4$HHINCOME)
freq(sante_FR4$HHINCOME_imp)
table(sante_FR4$HHINCOME,sante_FR4$HHINCOME_imp)

### DERNIERE VERSION ISSUE DE LA DATA PREP
sante_FR5 <- sante_FR4[,-35]
dim(sante_FR5)
glimpse(sante_FR5)

write_delim(sante_FR5,path="SANTE_FR",delim=";")
