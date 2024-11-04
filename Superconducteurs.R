setwd("/net/netud/m/lgadbin/S7/AnalyseDeDonnees/Projet")

library(MASS)
require(pls)
require(splines)

rm(list=ls(all = TRUE))

?Boston

print(Boston)