#Exercice 1

#Importer la base "céréales"
library(haven)
base_cereale=read_dta("C:\\Users\\user\\Desktop\\mtn\\ENSAE\\ISE\\ISEP2\\SEMESTRE 2\\Informatique\\R_2023\\Bases\\cereales.dta")
View(base_cereale)
colnames(base_cereale)
#Renommer
colnames(base_cereale)[4:14]= c("autre_céréa", "quanite_cons","unites_cons","taille_cons","proven_auto",
                                "proven_other","freq_achat","quatite_acha", 
                                "unite_acha","taille_acha","vl_lastacha")

#les variables d'intérêt

#valeurs manquantes et imputation;
 #Identification des valeurs manquantes:
sapply(base_cereale, function(x) sum(is.na(x))) #Pour chaque variabe on a le nombre de NA 
View(is.na(base_cereale)) #ça permet de sortitr un view où pour s'il y'a NA il met True, au cas contraire il met False
 
#Imputation:
View(table(base_cereale$proven_auto))
#le View de la variable proven_auto montre que la quasi-totalité des observations prennent la valeur 0, on va donc utiliser la methode d'imputation par le mode
base_cereale$proven_auto[is.na(base_cereale$proven_auto)] = "0"
#De même pour proven_other 
View(table(base_cereale$proven_other))
base_cereale$proven_other[is.na(base_cereale$proven_other)] = "0"
#Pour les autres on va utiliser la methode des moyennes
mean_quanite_cons <- mean(base_cereale$quanite_cons , na.rm = TRUE)
base_cereale$quanite_cons[is.na(base_cereale$quanite_cons)] <- mean_quanite_cons
mean_unites_cons <- mean(base_cereale$unites_cons  , na.rm = TRUE)
base_cereale$unites_cons [is.na(base_cereale$unites_cons )] <- mean_unites_cons
mean_taille_cons  <- mean(base_cereale$taille_cons  , na.rm = TRUE)
base_cereale$taille_cons [is.na(base_cereale$taille_cons )] <- mean_taille_cons 
mean_freq_achat   <- mean(base_cereale$freq_achat   , na.rm = TRUE)
base_cereale$freq_achat  [is.na(base_cereale$freq_achat  )] <- mean_freq_achat
mean_quatite_acha    <- mean(base_cereale$quatite_acha    , na.rm = TRUE)
base_cereale$quatite_acha   [is.na(base_cereale$quatite_acha   )] <- mean_quatite_acha
mean_unite_acha      <- mean(base_cereale$unite_acha      , na.rm = TRUE)
base_cereale$unite_acha [is.na(base_cereale$unite_acha     )] <- mean_unite_acha  
mean_taille_acha      <- mean(base_cereale$taille_acha      , na.rm = TRUE)
base_cereale$taille_acha     [is.na(base_cereale$taille_acha     )] <- mean_taille_acha 
mean_vl_lastacha      <- mean(base_cereale$vl_lastacha       , na.rm = TRUE)
base_cereale$vl_lastacha      [is.na(base_cereale$vl_lastacha      )] <- mean_vl_lastacha 
#Verifier que les valeurs NA ont été bien remplacées
sapply(base_cereale, function(x) sum(is.na(x)))

#Importer la base table de conversion
  library(readxl)
  table_conversion=read_excel("C:\\Users\\user\\Desktop\\mtn\\ENSAE\\ISE\\ISEP2\\SEMESTRE 2\\Informatique\\R_2023\\Bases\\Table de conversion phase 2.xlsx")
  View(table_conversion)
#Décrire la variable poids
    class(table_conversion$poids)
      #Ici la variable poids donne pour chaque type de produit selon sa taille son poids.
   View(table(table_conversion$poids)) # Afficher le tableau de fréquence de la variable poids
     # Avec ce tableau on voit que la variable compte 601 modalités
   #Verifier s'il n'y a pas de valeur manquante
     sum(is.na(table_conversion$poids))
     # La varible n'a pas de valeur manquante
   #descriptives
     poids <- as.numeric(table_conversion$poids)
     summary(table_conversion$poids)
#clé d'identification dans les deux bases
     table_conversion$key_ID1= paste0(table_conversion$produitID,table_conversion$uniteID,table_conversion$tailleID)
     base_cereale$key_ID = paste0(base_cereale$cereales__id,base_cereale$unites_cons,base_cereale$taille_cons )
#Fusionner les deux bases
     Base_complete=merge(base_cereale, table_conversion, by.x="key_ID", by.y="key_ID1", all.x=T)
     View(bd)
#Convertir les quantités consommées en unités standards
     Base_complete$poids=as.double(Base_complete$poids)
     Base_complete$Cons_year <- ((Base_complete$quanite_cons*Base_complete$poids/1000)/7)*365 # quantité en KG
     library(lessR)
     label(data=Base_complete ,Cons_year, "Consommation anuelle des ménages en Kg")  
#Variable taille_men
     men=data.frame(table(Base_complete$interview__key))# Creer une base ménage où il y a les ménages et leurs frequences
     View(men)
     taille_men=floor(rnorm(n=3479 ,mean= 9)) #Creer la variable taiile_men
     men=data.frame(men$Var1, men$Freq, taille_men) # mettre la variable taille_men dans la base menage
     men$taille_men=as.double(taille_men)
     max(men$taille_men)
     Base_complete <- merge(Base_complete, men, by.x ="interview__key", by.y="men.Var1", all.x = T) # fusionner la base men et la base complète 
     View(Base_complete)

#Exercice 2
     
# Focntion d'importation de base
   import<- function(chemin, nom_fichier, extention="csv", sep=";", quote="\\"){
     base=read.csv2(paste0(chemin, "\\", nom_fichier, ".csv"))
     View(base) 
     return(base)
   }
#Fonction qui renome les variables
   renome = function(data_name,y=vector_name){
     colnames(data_name)[1:length(data_name)]=y
     View(data_name)
   }
bd=import("C:\\Users\\user\\Desktop\\mtn\\ENSAE\\ISE\\ISEP2\\SEMESTRE 2\\Informatique\\R_2023\\R_2023", "df.MameThiernoNdaiye")

#Fonction qui fusionne deux bases
fusionne= function(B1=base1, B2=base2, B1_key=key_ID, B2_key=key_ID, all.x="T", all.y="F"){
       Base_fusion=merge(B1,B2, by.x=B1_key, by.y=B2_key, all.x=T) 
       return(Base_fusion)
}

# Fonction qui détecte les valeurs manquates
value_Na= function(dt=data){
 sapply(dt, function(x) sum(is.na(x)))
}

#Fonction qui impute les valeurs manquantes
impute <- function(dt=data, var=variable) {
  if (is.numeric(dt[[var]])) {
    met_var <- mean(dt[[var]], na.rm = TRUE)
    dt[[var]][is.na(dt[[var]])] <- met_var 
  } else {
    counts <- table(dt[[var]])
    mode_index <- which.max(counts)
    mode <- names(counts)[mode_index]
    dt[[var]][is.na(dt[[var]])] <- mode
  }
  return(dt)
}

# Fonction qui détecte les valeurs aberrantes 
 Value_aber= function(dt=data, var=variable, coef=1.5){
   boxplot(dt[[var]])
   q1 = quantile(dt[[var]], 0.25, na.rm = TRUE)
   q3 =quantile(dt[[var]], 0.75, na.rm = TRUE)
   iqr = q3 - q1
   lower <- q1 - coef *iqr
   upper <- q3 + coef **iqr
   outliers <- dt[[var]][dt[[var]] < lower | dt[[var]] > upper]
   return(outliers)
 }
 
#Fonction qui les corrige.
 
 correct_aber= function(dt=data, var=variable, coef=1.5){
 q1 <- quantile(dt[[var]], 0.25, na.rm = TRUE)
 q3 <- quantile(dt[[var]], 0.75, na.rm = TRUE)
 iqr <- q3 - q1
 lower <- q1 - coef * iqr
 upper <- q3 + coef * iqr
 dt[[var]][dt[[var]] < lower | dt[[var]] > upper] <- median(dt[[var]], na.rm = TRUE)
 return(dt)
}
