setwd("/home/lucas/TP/code")
library(PCAmixdata)
library("FactoMineR")
library("factoextra")
rm(list=ls(all=TRUE))
load("../data/Data/Data/Projets/superconductivity_data_train.rda")

mean <- apply(data_train, 2, mean)
std <- apply(data_train, 2, sd)
stat <- rbind(mean, std)

datanorm <- sweep(data_train, 2, mean, "-")
datanorm <- sweep(datanorm, 2, std, "/")

res <- PCA(datanorm, ncp=17, graph=FALSE)
ind <- get_pca_ind(res, 15)
ind$coord
