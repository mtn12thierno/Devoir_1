#Exercice1
#Création aléatoire des variables quantitatives
ID=seq(from=1, to=200)
age=sample(18:25, 200, T)
Taille=as.double(rnorm(200, 1.75, 0.02))
Moy_seme1=as.double(rnorm(200,15.5, 0.80))
Moy_final=as.double(rnorm(200,14, 0.85))
#Création des variables qualitatives
nationalité=factor(sample(c("Sénégal", "Togo"," Burkina Faso", "Niger"), 200, T))
Région_Examen=factor(sample(c("Dakar", "Thiès","Saint Louis", "Kaolack"), 200, T))
centre_examen=factor(sample(c("ENSAE", "Lycée S.Malick Sy","Prytanée militaire", "LYCEE VALDIODIO NDIAYE "), 200, T))
#Création du data frame
dt=data.frame(ID,nationalité, Région_Examen,centre_examen, Taille,age, Moy_seme1, Moy_final )
#Recodage et création de certaines variables qualitative de type factor
dt$mention_final=factor(ifelse(data$Moy_final<=18 & data$Moy_final>=16,"Très bien",ifelse(data$Moy_final<16 & data$Moy_final>=14,"bien", ifelse(data$Moy_final<14 & data$Moy_final>=12,"assez bien","Insuffisant"))))
dt$mention_Seme1=factor(ifelse(data$Moy_seme1<=18 & data$Moy_seme1>=16,"Très bien",ifelse(data$Moy_seme1<16 & data$Moy_seme1>=14,"bien", ifelse(data$Moy_seme1<14 & data$Moy_seme1>=12,"assez bien","Insuffisant"))))
dt$Decision_final=factor(ifelse(data$Moy_final<13,"Rattrapage", "Passe en classe superieure"), levels =c("Rattrapage", "Passe en classe superieure"), labels=c(0,1))
df.MameThiernoNdaiye=dt
class(df.MameThiernoNdaiye$Taille)
View(df.MameThiernoNdaiye)
#Exportation de la base de données sous format csv
write.table(df.MameThiernoNdaiye, file="C:\\Users\\user\\Desktop\\mtn\\ENSAE\\ISE\\ISEP2\\SEMESTRE 2\\Informatique\\R_2023\\R_2023\\Devoir 1\\df.MameThiernoNdaiye.csv", sep=";")
