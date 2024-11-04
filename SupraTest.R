setwd("/net/netud/m/ilerousic/AnalyseDeDonnees/Projet")
library("FactoMineR")
library(PCAmixdata)
library(GGally)
require(pls)
require(splines)
rm(list=ls(all=TRUE))
load("/net/netud/m/ilerousic/AnalyseDeDonnees/Projet/superconductivity_data_train.rda")
print(data_train)

# Calcul de la moyenne et de l’écart type des variables
mean <- apply(data_train,2,mean)
std <- apply(data_train,2,sd) #standard deviation
stat <- rbind(mean,std)


# Création des données centrées ...
datanorm <- sweep(data_train,2,mean,"-")
# ... et réduites
datanorm <- sweep(datanorm,2,std,"/")
# Affichage des données centrées - réduites
print(datanorm,digits=4)

# Visualisation des données en description bivariée
pairs(data_train[,1:5])
# Afficher la matrice de corrélation
ggcorr(data_train[,1:5])
# Aller encore plus loin avec ggpairs
ggpairs(data_train[,1:5])

# Matrice des distances entre les individus
dist(data_train)

# Corrélation entre les variables
cor(data_train[,1:5])

# Analyse en composantes principales sur les données d’origine
# (scale.unit=FALSE)
res <- PCA(data_train,graph=FALSE,scale.unit=FALSE)




# Analyse en composantes principales sur les données centrées-réduites
# (par défaut: scale.unit=TRUE)
resnorm <- PCA(data_train,graph=FALSE)
# Figure individus
plot(resnorm,choix="ind",cex=1.5,title="")
# Figure variables
plot(resnorm,choix="var",cex=1.5,title="")


# Inertie (variance) des composantes principales
resnorm$eig
barplot(resnorm$eig[,1])

# Contribution des individus
resnorm$ind$contrib
