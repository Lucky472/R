setwd("/home/lucas/TP/code")
library(PCAmixdata)
library("FactoMineR")
library("factoextra")
library(mgcv)
require(splines)
rm(list=ls(all=TRUE))
load("../data/Data/Data/Projets/superconductivity_data_train.rda")
load("../data/Data/Data/Projets/superconductivity_data_test.rda")

mean <- apply(data_train, 2, mean)
std <- apply(data_train, 2, sd)
stat <- rbind(mean, std)

datanorm <- sweep(data_train, 2, mean, "-")
datanorm <- sweep(datanorm, 2, std, "/")

imax=17
res <- PCA(datanorm, ncp=imax, graph=FALSE)
ind <- get_pca_ind(res)
ind$coord

data <- data.frame(y = data_train$critical_temp, ind$coord)

n <-length(data_train$critical_temp)
alpha <- 0.05

#Régresion linéaire multiple
## Cas polynomial
degpoly <- 5
formulaPoly <- as.formula(paste("data$y ~", paste("poly(data$Dim.", 1:imax, sep="", ", degree =", degpoly, ")", collapse = " + ")))
multiplePolyReg <- lm(formulaPoly, data = data)
summary(multiplePolyReg)

## Cas smoothing splines
degspline <- 8
formulaSpline <- as.formula(paste("data$y ~", paste("s(data$Dim.", 1:imax, sep="", ", k =", degspline, ")", collapse = " + ")))
multipleSplineReg <- gam(formulaSpline, data = data)
summary(multipleSplineReg)

#Castest
mean_test <- apply(data_test, 2, mean)
std_test <- apply(data_test, 2, sd)
datanorm_test <- sweep(data_test, 2, mean_test, "-")
datanorm_test <- sweep(datanorm_test, 2, std_test, "/")


data_test_PCA <- predict(res, newdata = data_train)
data_test_polyreg <- predict(multiplePolyReg, newdata = data_test_PCA)
print(data_test_polyreg)
