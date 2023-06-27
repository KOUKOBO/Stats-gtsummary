# Installation des packages
library(labelled)
library(readxl)

#Importation de jeux de données
wage2 <- read_excel("C:/Users/krist/Desktop/wage2.xlsx") # Importation base
data<- wage2


#Traitement des données-----

data %>% # Visualisation des données
  look_for()


##Etiquette de variable

data<-data %>% 
  set_variable_labels(
    wage = "Salaire",
    hours= "Volume d'heure",
    IQ =" Quotient Intellectuel",
    KWW = "Score de connaissance du monde de travail",
    educ= " Nombre d'année d'étude",
    exper= "Nombre d'année d'expérience",
    tenure ="Nombre_d_année_avec_employeur",
    age= "Âge en année",
    married= "Situation matrimoniale",
    black="Origine",
    south="Situation_géo",
    urban= "Milieu de résidence",
    sibs="nombre_frère",
    brthord="Ordre_naissance",
    meduc="Niveau_education_mère",
    feduc="Niveau_education_père",
    lwage="log_salaire"
  )

data %>%
  look_for()

## Recoding data$black into data$black_rec-----

data$Black_rec <- data$black %>%
  as.character() %>%
  fct_recode(
    "Autres" = "0",
    "Noir" = "1"
  )

## Recoding data$married into data$married_rec----
data$Married_rec <- data$married %>%
  as.character() %>%
  fct_recode(
    "Autres" = "0",
    "Marié" = "1"
  )
## Recoding data$south into data$south_rec----
data$South_rec <- data$south %>%
  as.character() %>%
  fct_recode(
    "Autres" = "0",
    "Sud" = "1"
  )
## Recoding data$urban into data$urban_rec----
data$Urban_rec <- data$urban %>%
  as.character() %>%
  fct_recode(
    "Rural" = "0",
    "Urbain" = "1"
  )

## Réordonnancement de data$black_rec----
data$Black_rec <- data$Black_rec %>%
  fct_relevel(
    "Noir", "Autres"
  )
## Réordonnancement de data$married_rec----
data$Married_rec <- data$Married_rec %>%
  fct_relevel(
    "Marié", "Autres"
  )
## Réordonnancement de data$south_rec-----
data$South_rec <- data$South_rec %>%
  fct_relevel(
    "Sud", "Autres"
  )

#Statistique univarié-----

library(gtsummary)
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

data %>% 
  tbl_summary(
    include = c(-lwage, -feduc,-meduc,-brthord,-black,-south,-urban) # Par défaut les statistiques calculés sont la médiane et l'intervalle inter quartile pour les variables quantitatifs. Pour les variables qualitatifs, nous avons l'effectifs
  )


data %>% 
  tbl_summary(
    include = c(-lwage, -feduc,-meduc,-brthord,-black,-south,-urban), #Include toutes les variables sauf lwage, feduc, etc.
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", #L'option statistic permet de spécificié les statistiques qui seront calculés( moyenne et écart_type).
      all_categorical() ~ "{p}%"
    ),
    digits= everything()~ 0 # Le nombre de décimal après la virgule
  )

#Statistique Bivariée----

## Variable quantitatif----

data %>% 
  tbl_summary(
    include = c("wage","hours","IQ", "educ","exper"),
    by="Married_rec", # Croissement en fonction du statut matrionomial
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits= everything()~ 0
  ) %>% 
  add_p() # Ajout de la p_value 

## Variable qualitatif----

data %>% 
  tbl_summary(
    include = c("Urban_rec","South_rec"),
    by="Black_rec",# Croissement en fonction de l'origine
    statistic = list(
    all_categorical() ~ " {n} ({p}%)" 
    ),
    digits= everything()~ 0
  ) %>% 
  add_p() #Effectue le test d'indépendeance et affiche les résultats

#Partie 2 : Regréssion économétrique----

mod<-lm(wage ~ hours + IQ +educ + exper + age + Married_rec + Black_rec + Urban_rec, data= data) # Modèle de régression linéaire
mod

summary(mod)

mod %>% 
  tbl_regression(
    intercept=FALSE # Permet de ne pas inclure la constante dans le tableau
  )
