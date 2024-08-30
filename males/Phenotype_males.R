### A script to extract the Phenotypic data from the UKB data set ####
# load libraries 

remotes::install_github('MRCIEU/TwoSampleMR', force = TRUE)

library(dplyr)
library(data.table)
library(TwoSampleMR)
library(MRInstruments)
library(ieugwasr)
library(readr)
library(remotes)
library(MVMR)

################################################
# Read in PRS and BMI pheno and covariates #
##############################################


setwd("/user/work/cr23646/PRS/males/phenotype_m")

Linker<-read.csv("/user/work/cr23646/PRS/males/phenotype_m/linker.81499.csv")
# or 
Linker<-read.csv("linker.81499.csv") # if the first command does not work

Pheno<-fread("/user/work/cr23646/PRS/males/phenotype_m/data.51913.csv",check.names=TRUE)
#or
Pheno<-fread("data.51913.csv",check.names=TRUE) # the data is not readable using the first command 

grs_males<-read.table("/user/work/cr23646/PRS/males/phenotype_m/grs_data_males.txt")

# rename column names 
colnames(Linker)[1] <- "ID"
colnames(Linker)[2] <- "eid"
PCS<-data.frame(fread("/user/work/cr23646/PRS/phenotype/data.pca1-10.plink.txt"))

#PCs
colnames(PCS)[1] <- "ID"
colnames(PCS)[2] <- "FID"
colnames(PCS)[3] <- "PC1"
colnames(PCS)[4] <- "PC2"
colnames(PCS)[5] <- "PC3"
colnames(PCS)[6] <- "PC4"
colnames(PCS)[7] <- "PC5"
colnames(PCS)[8] <- "PC6"
colnames(PCS)[9] <- "PC7"
colnames(PCS)[10] <- "PC8"
colnames(PCS)[11] <- "PC9"
colnames(PCS)[12] <- "PC10"

#Extract only the ones i want!!
#ID
which(colnames(Pheno) == "eid")
# 1
#SEX
which(colnames(Pheno) == "X31.0.0")
#23
#AGE
which(colnames(Pheno2) == "X4080.0.0")
# 1560
# Blood pressure(SBP)
which(colnames(Pheno) == "X21001.0.0")
# 9790
# BMI
which(colnames(Pheno2) == "X21003.0.0")
# 9798
# age

# Extract the ones we want
Pheno<-Pheno[,c(1,23,1560,9790,9798)]

## Merge Files  in the terminal (R)
# merge the PRS and Linker
Scores = merge(Linker, grs_males, by='ID', all.x=TRUE, all.y=FALSE, sort=FALSE)

# merge the score with phenotype data 
Final_males= merge(Scores, Pheno, by='eid', all.x=TRUE, all.y=FALSE, sort=FALSE)

# merge with PCs
Final_data_m<-merge(Final_males,PCS , by='ID', all.x=TRUE, all.y=FALSE, sort=FALSE)

### write files in the terminal (R)
write.csv(Pheno, '/user/work/cr23646/PRS/males/phenotype_m/Pheno_m.csv', row.names = FALSE)
write.csv(Final_data_m, '/user/work/cr23646/PRS/males/phenotype_m/Final_data_m.csv', row.names = FALSE)
write.csv(Scores, '/user/work/cr23646/PRS/phenotype/Scores.csv', row.names = FALSE)   





