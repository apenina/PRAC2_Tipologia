#TIPOLOGIA I CICLE DE VIDA DE LES DADES -- PRAC2

#Lectura de les dades

#Indiquem on es troba el fitxer que volem carregar:
setwd("~/Desktop/Màster UOC Data Science/3r semestre/Tipologia i cicle de vida de les dades/PRAC2")
#Carreguem l'arxiu csv:
data<-read.csv2('Habitatges_Lloguer_Barcelona_20201104.csv', sep=",", fileEncoding = "utf-16")

#Observem de quin tipus és cada variable:
str(data)

#La variable 'Preu' figura com a caràcter. La transformem a numèrica, eliminant amb la funció gsub() els caràcters no numèrics:
data$Preu <- gsub(" €","", data$Preu)
data$Preu <- gsub(".","", data$Preu, fixed = T)
data$Preu <- as.integer(as.character(data$Preu))
str(data)




#Neteja de les dades:

#Missings:
#Nombre de missings per variable:
sapply(data, function(x) sum(is.na(x)))
#Eliminem els habitatges que continguin algun missing:
data <- na.omit(data)

#Outliers:
#Mostrem els quartils i la mitjana de les variables numèriques:
summary(data)

#Gràfics
#Histogrames
hist(data$Metres_Quadrats)
hist(data$Num_Habitacions)
hist(data$Num_Lavabos)
hist(data$Preu)

#Boxplots
boxplot(data$Metres_Quadrats, main='Metres Quadrats')
boxplot(data$Num_Habitacions, main="Nombre d'habitacions")
boxplot(data$Num_Lavabos, main='Nombre de lavabos')
boxplot(data$Preu, main='Preu')

#Detecció de valors anòmals:
sum(data$Metres_Quadrats<10)
sum(data$Num_Habitacions==0)

#Eliminem els valors anòmals:
data <- data[!(data$Metres_Quadrats<10),]
data <- data[!(data$Num_Habitacions==0),]




#Anàlisi de les dades:

#Observem el nombre de categories de la variable Barri:
levels(factor(data$Barri))

#Reagrupem els barris en districtes per reduir el nombre de categories:
install.packages("readxl")
library(readxl)
#Llegim l'Excel que conté les relacions entre els barris i els districtes:
barris <- as.data.frame(read_excel("Barri_Districte.xlsx", sheet = 1))
districtes <- as.data.frame(read_excel("Barri_Districte.xlsx", sheet = 2))

#Creem la nova variable Districte amb la nova agrupació dels barris:
for (i in 1:nrow(barris)) data$Districte[as.character(data$Barri) == barris[i, 1]] <- barris[i, 2]
data$Districte <- factor(data$Districte, levels = 1:10, districtes$Districtes)

for (i in 1:nrow(barris)) dat$Barri2[as.character(dat$Barri) == barris[i, 1]] <- barris[i, 2]
dat$Barri2 <- factor(dat$Barri2, levels = 1:10, barris_codi$Barri )

levels(factor(data$Districte))

sapply(data, function(x) sum(is.na(x)))
#Tots els barris han estat assignats al seu corresponent districte.




#Comprovació de la normalitat i homogeneïtat de la variància

#Normalitat
shapiro.test(data$Metres_Quadrats)
#No es pot realitzar el test de Shapiro, ja que la bdd ha de tenir entre 3 i 5000 registres.

#Utilitzarem altres tests de normalitat, inclosos en el paquet "nortest":
install.packages("nortest")
library(nortest)

#Prova d'Anderson-Darling:
ad.test(data$Metres_Quadrats)
ad.test(data$Num_Habitacions)
ad.test(data$Num_Lavabos)
ad.test(data$Preu)

#Prova de Lilliefors (Kolmogorov-Smirnov):
lillie.test(data$Metres_Quadrats)
lillie.test(data$Num_Habitacions)
lillie.test(data$Num_Lavabos)
lillie.test(data$Preu)
#Els 2 tests indiquen una NO normalitat de les variables numèriques.

#Gràfics de normalitat:
quanti <- c("Num_Habitacions", "Num_Lavabos", "Metres_Quadrats", "Preu")
par(mfrow = c(2,2))
for (i in seq_along(quanti)) {
  qqnorm(data[,quanti[i]],main = quanti[i])
  qqline(data[,quanti[i]])
}


#Homogeneïtat de les variàncies:
install.packages("car")
library(car)
for (i in seq_along(quanti)) print(leveneTest(data[,quanti[i]], data$Districte))
#Es rebutja homogeneïtat de les variàncies per a totes les variables quantitatives, tenint 
#en compte com a grups els districtes.




#Mètodes d'anàlisi per a donar resposta a les preguntes

#Per quin tipus d’immoble hi ha més anuncis?
#Fem un CrossTable per a donar resposta a aquesta pregunta:
install.packages("gmodels")
library(gmodels)
CrossTable(data$Tipus_Immoble)


#Quin és el preu de lloguer mitjà dels habitatges a Barcelona?
mean(data$Preu)


#El districte on es troba l’immoble influeix en el preu del lloguer?
#Al tenir més de 2 grups, realizem el test de kruskall-Wallis:
kruskal.test(data$Districte ~ data$Preu)
#Es rebutja la hipòtesi nul·la i, per tant, el districte influeix en el preu del lloguer.


#A quin districte són més cars els lloguers dels immobles? I més barats?
#Observem gràficament aquestes diferències mitjançant boxplots dels preus segons el districte:
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x=Districte, y=Preu, fill=Districte)) + geom_boxplot() + geom_point()

#Fem un model de regressió lineal per a determinar en quina magnitud influeix cada districte 
#en el preu del lloguer:
#Ajustem un model de regressió lineal per veure l'efecte de cada barri:
m1 <- lm(Preu ~ Districte, data = data)
summary(m1)


#Quin és l’augment de preu per metre quadrat?
#Realitzem un altre model de regressió lineal:
m2 <- lm(Preu ~ Metres_Quadrats, data = data)
summary(m2)


#Quin seria el preu de lloguer estimat d’un pis de 90m2, 3 habitacions i 2 lavabos 
#en el barri de Sant Gervasi?
#Creem un model de regressió lineal multivariant per a poder explicar amb la màxima precisió
#possible els preus de lloguer:
m3 <- lm(Preu ~ Metres_Quadrats+Num_Habitacions+Num_Lavabos+Tipus_Immoble+Districte, data =data)
summary(m3)
#Totes les variables resulten significatives per al model.

#Amb aquest últim model ja podem fer la predicció:
predict(m3, newdata = data.frame(Num_Habitacions = 3,
                                 Num_Lavabos = 2,
                                 Tipus_Immoble = "Piso",
                                 Metres_Quadrats = 90,
                                 Districte = "Sarrià-Sant Gervasi"))
#La predicció del preu del pis és de 1.527,81€.

