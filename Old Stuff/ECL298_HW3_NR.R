#ECL298 HW#3 - Elasticities for Teasel Data
#Noam Ross, 1/19/2011

#See Les_analysis.R for function definitions

rm(list=ls()) #clear workspace
source("Les_analysis.R")
L = as.matrix(read.table("teasel_stage.txt")) #load data
E = Les_elas(L)

barplot(E,beside=T,axes=T, main="Elasticities for Teasel Leslie Matrix", ylab="Elasticity",xlab="Matrix element (Clustered by columns)")