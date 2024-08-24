### A script to extract the Phenotypic data from the UKB data set ####

remotes::install_github('MRCIEU/TwoSampleMR', force = TRUE)
# First, install the remotes package if you don't have it
install.packages("remotes")

# Then, install MVMR from GitHub
remotes::install_github("repository_owner/MVMR")

# load libraries 
library(dplyr)
library(data.table)
library(TwoSampleMR)
library(MRInstruments)
library(ieugwasr)
library(readr)
library(remotes)
library(MVMR)

################################################
# Read in PRS and cancer pheno and covariates #
##############################################

setwd ("/user/work/cr23646/PRS/phenotype/")

Linker<-read.csv("/user/work/bu19525/BristolPhD/MVMR_follow_up/Data/linker.81499.csv")
Pheno<-fread("/user/work/bu19525/SRA_files/Cancer_epi_project/Data/UKB/data.51913.csv",check.names=TRUE)
PCS<-data.frame(fread("/user/work/bu19525/SRA_files/Cancer_epi_project/Data/data.pca1-10.plink.txt"))
colnames(Linker)[1] <- "ID"
colnames(Linker)[2] <- "eid"
#PCs
colnames(PCS)[1] <- "ID"
colnames(PCS)[2] <- "FID"
colnames(PCS)[3] <- "PCA_1"
colnames(PCS)[4] <- "PCA_2"
colnames(PCS)[5] <- "PCA_3"
colnames(PCS)[6] <- "PCA_4"
colnames(PCS)[7] <- "PCA_5"
colnames(PCS)[8] <- "PCA_6"
colnames(PCS)[9] <- "PCA_7"
colnames(PCS)[10] <- "PCA_8"
colnames(PCS)[11] <- "PCA_9"
colnames(PCS)[12] <- "PCA_10"


#Extract only the ones i want!!
#ID
which(colnames(Pheno) == "eid")
# 1
#SEX
which(colnames(Pheno) == "X31.0.0")
#23
#AGE
which(colnames(Pheno) == "X21022.0.0")
# 9838
#Cancer ICD
which(colnames(Pheno) == "X40006.0.0")
# 14776
#Cancer ICD
which(colnames(Pheno) == "X40006.21.0")
#14797
# Extract the ones we want

Pheno<-Pheno[,c(1,23,9838,14776,14777,14778,14779,14780,14781,14782,14783,14784,14785,14786,14787,14788,14789,14790,14791,14792,14793,14794,14795,14796,14797)]