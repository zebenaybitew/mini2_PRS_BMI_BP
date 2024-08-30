################## BMI_PRS using UKB data for females  #####################################

############## Set Working directory ###########################################

## removes anything already in memory
rm(list=ls())

setwd("/Users/cr23646/Desktop/Mini2/PRS/females")

## load libraries 
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library("AER")

## Read the GRS Data which contains the individual PRS for females from the UKB data 
grs_females <- read.table("/Users/cr23646/Desktop/Mini2/PRS/females/GRSscore.sscore", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

## View the data
head(grs_females)
dim(grs_females)
# change column names 
colnames(grs_females) <- c("IID", "CNT", "CNT2", "PRS")

## standardized the PRS 
grs_females$SCOREstd <- scale(grs_females$PRS, center=TRUE, scale=TRUE) 

# Converting the SCOREstd column to numeric
grs_females$SCOREstd <- as.numeric(grs_females$SCOREstd)

## Write the data
write.table(grs_females, 'grs_data_females.txt', sep = "\t", row.names = TRUE, col.names = TRUE)

## read data 
grs_females<-read.table("/Users/cr23646/Desktop/Mini2/PRS/females/grs_data_females.txt")

############# plot a histogram of the standardized score 
# Calculate the mean and standard deviation
mean_score <- mean(grs_females$SCOREstd, na.rm = TRUE)
sd_score <- sd(grs_females$SCOREstd, na.rm = TRUE)

# Create the histogram with ggplot2
plot<-ggplot(grs_females, aes(x = SCOREstd)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_score, color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_score + sd_score, color = "Mean + SD"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_score - sd_score, color = "Mean - SD"), linetype = "dashed", size = 1) +
  labs(title = "Histogram of Standardized PRS Scores among females",
       x = "Standardized PRS Score",
       y = "Frequency") +
  scale_color_manual(values = c("Mean" = "blue", "Mean + SD" = "green", "Mean - SD" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "right") +
  guides(colour = guide_legend("Legend"))

## Save the plot to a file
ggsave(filename = "histogram_prs_scores.pdf", plot = plot, width = 8, height = 6, dpi = 300)

# Create a violin plot
ggplot(grs_females, aes(y = SCOREstd)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal(base_size = 8) + # Smaller base font size
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + # Smaller margins
  labs(title = "Boxplot of SD_score", y = "SD_score")


###############################################################################
###################### Read the data to perform one sample MR #################
Final_data_f<- read.csv("/Users/cr23646/Desktop/Mini2/PRS/females/Final_data_f.csv")


## exclude na values 
Final_data_f <- Final_data_f[!is.na(Final_data_f$SCOREstd), ]
Final_data_f <- Final_data_f[!is.na(Final_data_f$sbp), ]
Final_data_f <- Final_data_f[!is.na(Final_data_f$BMI), ]

## Move FID to the second column 
Final_data_f <- Final_data_f[, c(1, which(names(Final_data_f) == "FID"), 
                             setdiff(2:ncol(Final_data_f), which(names(Final_data_f) == "FID")))]

## Write the data which is clean to be used for the final analyses 
write.csv(Final_data_f, 'Final_data_f_clean.csv', row.names = TRUE)


###############################################################################
######################### Perform one sample MR ###############################

# PART 1 - read the combined data set
########################################################################## 
## read the clean data for females
Final_data_f<-read.csv("/Users/cr23646/Desktop/Mini2/PRS/females/Final_data_f_clean.csv")

## select females 
final_females <- Final_data_f[Final_data_f$sex == 0, ]

## Write female data containing genotype and phenotype information 
write.csv(final_females, 'final_females.csv', row.names = TRUE)

## read final data for females 
final_females<-read.csv("/Users/cr23646/Desktop/Mini2/PRS/females/final_females.csv")

##Exclude the first column 
final_females<-final_females[, -c(1)]

## change characters 
final_females$sbp <- as.numeric(final_females$sbp)
final_females$BMI <- as.numeric(final_females$BMI)
final_females$age <- as.numeric(final_females$age)


##########################################################################
# PART 2 - Is PRS associated with BMI using conventional OLS regression among females? #Ordinary Least Squares regression (OLS)
##########################################################################
## 1. Are sbp and BMI associated?
summary(lm(sbp ~ BMI, final_females)) # yes (β=0.77)
plot(sbp ~ BMI, final_females)
abline(lm(sbp ~ BMI, data = final_females), col="red")

## 2. Check whether BMI and sbp are associated with the covariables age and sex
summary(lm(BMI ~ age, final_females)) # yes(β=0.04)
summary(lm(sbp ~ age, final_females)) # yes (β=0.97)

## 3. Are sbp and BMI associated after adjusting for age and sex?
summary(lm(sbp~BMI + age , data=final_females)) # (β_bmi=0.67, β_age=0.94)

##################################################
# PART 3 - Does PRS have a causal effect on sbp? among females #
##################################################

# 1. Is bias due to weak instruments likely in the one-sample MR analyses?(check the strength of instrument association with exposure)
summary(lm(BMI ~ SCOREstd, final_females)) # no coz F_stat=2963, and β=0.57

# check if PRS is associated with covariates 
summary(lm(SCOREstd~age, final_females)) # no , β=0.00004, p=0.882

##  Is the PRS genetic variant associated with potential confounders (age, sex, PCA'S) or principal components of ancestry? 
# Why is this important? is used to check IV2 (independent assumption)
# Correctly generate the formula for PC1 to PC10
summary(lm(as.formula(paste("SCOREstd ~ age +", paste0("PC", 1:10 ,collapse="+"))), final_females))

# check if sbp is associated with the PCs and other covariates 
summary(lm(as.formula(paste("sbp ~ age +", paste0("PC", 1:10 ,collapse="+"))), final_females))

# 3. Perform two-stage least squares (2SLS) regression 
#  Is there any evidence that circulating BMI affects sbp?
tsls1 <- ivreg(as.formula(paste("sbp ~ BMI | SCOREstd")), data=final_females)
summary(tsls1, diagnostics = T)  # B=0.34

# Is there any evidence that circulating BMI affects sbp after adjusting age and sex?
tsls2 <- ivreg(sbp ~ BMI + age | SCOREstd +  age, data=final_females)
summary(tsls2, diagnostics = T) # B=0.34

# # Is there any evidence that circulating BMI affects sbp after adjusting age and pcs 
# Construct the formula for the 2SLS model
# Correctly generate the formula for PCA_1 to PCA_10
pca_vars <- paste0("PC", 1:10, collapse = "+")
formula <- as.formula(paste("sbp ~ BMI + age +", pca_vars, "| SCOREstd + age +", pca_vars))

## Fit the 2SLS model
tsls3 <- ivreg(formula, data = final_females)

## Summarize the model with diagnostics
summary(tsls3, diagnostics = TRUE) # B=0.30

## 4. Do the MR results corroborate with the results from the OLS regression?

## 5. Plot the results using png
png("./sbp_bmi.png")
plot(sbp ~ BMI, final_females)
abline(lm(sbp ~ BMI, final_females), col="red")
abline(tsls1, col="blue")
dev.off()

## 5. Plot the results using pdf
pdf("./sbp_bmi.pdf")  # Open a PDF device
plot(final_females$BMI, final_females$sbp, 
     xlab = "Body Mass Index", 
     ylab = "Systolic Blood Pressure", 
     main = "")
abline(lm(sbp ~ BMI, final_females), col="red")  # Add the red regression line
abline(tsls1, col="blue")  # Add the blue regression line
dev.off()  # Close the PDF device


## plot the results for sex_age adjusted result
pdf("./four_models.pdf")
plot(final_females$BMI, final_females$sbp, 
     xlab = "Body Mass Index", 
     ylab = "Systolic Blood Pressure", 
     main = "")
abline(lm(sbp ~ BMI, final_females), col="red")  # Add the red regression line
abline(tsls1, col="blue4")
abline(tsls2, col="green4")  # Add the blue regression line
abline(tsls3, col="red4")
legend("topright", 
       legend = c("Linear Regression", "2SLS Model 1", "2SLS Model 2", "2SLS Model 3"), 
       col = c("red", "blue4", "green4", "red4"), 
       pch = 16,  # Legend symbols as dots
       cex = 0.6)
dev.off()  # Close the PDF device

# save in png form
png("./four_models.png")
plot(final_females$BMI, final_females$sbp, 
     xlab = "Body Mass Index", 
     ylab = "Systolic Blood Pressure", 
     main = "")
abline(lm(sbp ~ BMI, final_females), col="red")  # Add the red regression line
abline(tsls1, col="blue4")
abline(tsls2, col="green4")  # Add the blue regression line
abline(tsls3, col="red4")
legend("topright", 
       legend = c("Linear Regression", "2SLS Model 1", "2SLS Model 2", "2SLS Model 3"), 
       col = c("red", "blue4", "green4", "red4"), 
       pch = 16,  # Legend symbols as dots
       cex = 0.6)
dev.off()  # Close the PDF device

