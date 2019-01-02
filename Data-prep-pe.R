##*## Ce script permet de préparer les données en vue de la modélisation ##*##
## Vérification du chemin du répertoire courant
getwd()

### CHARGEMENT LIBRARIES
library(tidyverse)
library(googledrive) #-- chargement de l'API de google Drive
library(RCurl)
library(questionr)
library(labelled)


### DATA IMPORT VIA GOOGLE DRIVE
drive_find(n_max = 5,type="csv")
sante <- drive_download("BDD_SANTE.csv",overwrite = T)  # Pour télécharger la base
read_lines("BDD_SANTE.csv",n_max=2)
sante_eu <- read_csv2("BDD_SANTE.csv")


### DATA WRANGLING

sante_eu %>% group_by(COUNTRY) %>% count(REFYEAR) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="REFYEAR",value=prop)   # Supprimer les pays AT, HR, DE, cause ENQUÊTE EFFECTUÉE en 2013 ou 2015. Supprimer ou considérer pour la FR, CZ et SE les individus de 2015.

sante_eu %>% group_by(COUNTRY) %>% count(JOBISCO) %>% mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>% spread(key="JOBISCO",value=prop) # Fusionner MAINSTAT et JOBISCO d'une part puis MAINSTAT et JOBSTAT d'autres pour tester leur corrélation avec etat de santé

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
  select(-n) %>% spread(key="AC2",value=prop)  # Supprimé car très lié à AC1 et moins de 10% en général

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

# Etat matrimonial légal : MARSTALEGAL
freq(sante_FR$MARSTALEGAL)  
class(sante_FR$MARSTALEGAL)
sante_FR$MARSTALEGAL <- as.character(sante_FR$MARSTALEGAL)
sante_FR$MARSTALEGAL_r <- as.character(fct_recode(sante_FR$MARSTALEGAL, "2"="-1"))  # Imputer par le mode
freq(sante_FR$MARSTALEGAL_r)
chisq.test(table(sante_FR$MARSTALEGAL_r,sante_FR$etat_sante))
mosaicplot(table(sante_FR$MARSTALEGAL_r,sante_FR$etat_sante),las=3,shade=TRUE)

# Niveau de diplôme le plus élevé achevé : HATLEVEL
freq(sante_FR$HATLEVEL)  
class(sante_FR$HATLEVEL)
sante_FR$HATLEVEL <- as.character(sante_FR$HATLEVEL)
sante_FR$HATLEVEL <- fct_collapse(sante_FR$HATLEVEL, "Prim"=c("0","1"), "Secon"=c("2","3","4"),
                               "Univ"=c("5","6","7","8"))
sante_FR$HATLEVEL_r <- as.character(fct_recode(sante_FR$HATLEVEL, "2"="-1"))  # Imputer par le mode
freq(sante_FR$HATLEVEL_r)
