################## BMI_PRS using UKB data for males  #####################################

############## Set Working directory ###########################################

## removes anything already in memory
rm(list=ls())

setwd("/Users/cr23646/Desktop/Mini2/PRS/males")

## load libraries 
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library("AER")

## Read the GRS Data
grs_males <- read.table("/Users/cr23646/Desktop/Mini2/PRS/males/GRSscore.sscore", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

## View the data
head(grs_males)
dim(grs_males)
# change column names 
colnames(grs_males) <- c("IID", "CNT", "CNT2", "PRS")

## standardized the PRS 
grs_males$SCOREstd <- scale(grs_males$PRS, center=TRUE, scale=TRUE) 

# Converting the SCOREstd column to numeric
grs_males$SCOREstd <- as.numeric(grs_males$SCOREstd)

## Write the data
write.table(grs_males, 'grs_data_males.txt', sep = "\t", row.names = TRUE, col.names = TRUE)
write.csv(grs_males, 'grs_males.csv', row.names = TRUE)

## read data 
grs_males<-read.table("/Users/cr23646/Desktop/Mini2/PRS/males/grs_data_males.txt")
grs_males<- read.csv("/Users/cr23646/Desktop/Mini2/PRS/males/grs_males.csv")
## rename ID
names(grs_males)[names(grs_males) == "IID"] <- "ID"

##plot a histogram of the standardized score 
# Calculate the mean and standard deviation
mean_score <- mean(grs_males$SCOREstd, na.rm = TRUE)
sd_score <- sd(grs_males$SCOREstd, na.rm = TRUE)

# Create the histogram with ggplot2
plot<-ggplot(grs_males, aes(x = SCOREstd)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_score, color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_score + sd_score, color = "Mean + SD"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_score - sd_score, color = "Mean - SD"), linetype = "dashed", size = 1) +
  labs(title = "Histogram of Standardized PRS Scores among Males",
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
ggplot(grs_males, aes(y = SCOREstd)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal(base_size = 8) + # Smaller base font size
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + # Smaller margins
  labs(title = "Boxplot of SD_score", y = "SD_score")

###############################################################################
###################### Read the data to perform one sample MR #################
Final_data_m<- read.csv("/Users/cr23646/Desktop/Mini2/PRS/males/Final_data_m.csv")


## exclude na values 
Final_data_m <- Final_data_m[!is.na(Final_data_m$SCOREstd), ]
Final_data_m <- Final_data_m[!is.na(Final_data_m$sbp), ]
Final_data_m <- Final_data_m[!is.na(Final_data_m$BMI), ]

## Move FID to the second column 
Final_data_m <- Final_data_m[, c(1, which(names(Final_data_m) == "FID"), 
                                 setdiff(2:ncol(Final_data_m), which(names(Final_data_m) == "FID")))]

## Write the data which is clean to be used for the final analyses 
write.csv(Final_data_m, 'Final_data_m_clean.csv', row.names = TRUE)



###############################################################################
######################### Perform one sample MR ###############################

# PART 1 - read the combined data set
########################################################################## 
## read the clean data for females
Final_data_m<-read.csv("/Users/cr23646/Desktop/Mini2/PRS/males/Final_data_m_clean.csv")

## select males 
final_males<-Final_data_m[Final_data_m$sex==1, ]

## write the data
write.csv(final_males, 'final_males.csv', row.names = TRUE)

## read the data
final_males<-read.csv("/Users/cr23646/Desktop/Mini2/PRS/males/final_males.csv")

## change characters 
final_males$sbp <- as.numeric(final_males$sbp)
final_males$BMI <- as.numeric(final_males$BMI)
final_males$age <- as.numeric(final_males$age)

##Exclude the first column 
final_males<-final_males[, -c(1)]

##########################################################################
# PART 2 - Is PRS associated with BMI using conventional OLS regression among males? #Ordinary Least Squares regression (OLS)
##########################################################################
## 1. Are sbp and BMI associated?
summary(lm(sbp ~ BMI, final_males)) # yes (β=0.0.66)
plot(sbp ~ BMI, final_males)
abline(lm(sbp ~ BMI, data = final_males), col="red")

## 2. Check whether BMI and sbp are associated with the covariables age and sex
summary(lm(BMI ~ age, final_males)) # yes(β=0.01)
summary(lm(sbp ~ age, final_males)) # yes (β=0.58)


## 3. Are sbp and BMI associated after adjusting for age and sex?
summary(lm(sbp~BMI + age , data=final_males)) # (β_bmi=0.63, β_age=0.57)

##################################################
# PART 3 - Does PRS have a causal effect on sbp? among femals #
##################################################

# 1. Is bias due to weak instruments likely in the one-sample MR analyses?(check the strength of instrument association with exposure)
summary(lm(BMI ~ SCOREstd, final_males)) # no coz F_stat=2560, and β=0.48

# check if PRS is associated with covariates 
summary(lm(SCOREstd~age, final_males)) # no , β=0.0002

##  Is the PRS genetic variant associated with potential confounders (age, sex, PCA'S) or principal components of ancestry? 
# Why is this important? is used to check IV2 (independent assumption)
summary(lm(as.formula(paste("SCOREstd ~ age +", paste0("PC", 1:10 ,collapse="+"))), final_males))

# check if sbp is associated with the PCs and other covariates 
summary(lm(as.formula(paste("sbp ~ age +", paste0("PC", 1:10 ,collapse="+"))), final_males))

# 3. Perform two-stage least squares (2SLS) regression 
#  Is there any evidence that circulating BMI affects sbp?
tsls1 <- ivreg(as.formula(paste("sbp ~ BMI | SCOREstd")), data=final_males)
summary(tsls1, diagnostics = T)  # B=0.19

# Is there any evidence that circulating BMI affects sbp after adjusting age and sex?
tsls2 <- ivreg(sbp ~ BMI + age | SCOREstd + age, data=final_males)
summary(tsls2, diagnostics = T) # B=0.19

# # Is there any evidence that circulating BMI affects sbp after adjusting age and sex?
# Construct the formula for the 2SLS model
# Correctly generate the formula for PCS1 to PCS10
pca_vars <- paste0("PC", 1:10, collapse = "+")
formula <- as.formula(paste("sbp ~ BMI + age +", pca_vars, "| SCOREstd + age +", pca_vars))
#Fit the 2SLS model
tsls3 <- ivreg(formula, data = final_males)
# Summarize the model with diagnostics
summary(tsls3, diagnostics = TRUE) # B=0.14

## 4. Do the MR results corroborate with the results from the OLS regression?

## 5. Plot the results using png
png("./sbp_bmi.png")
plot(sbp ~ BMI, final_males)
abline(lm(sbp ~ BMI, final_males), col="red")
abline(tsls1, col="blue")
dev.off()

## 5. Plot the results using pdf
pdf("./sbp_bmi.pdf")  # Open a PDF device
plot(final_males$BMI, final_males$sbp, 
     xlab = "Body Mass Index", 
     ylab = "Systolic Blood Pressure", 
     main = "")
abline(lm(sbp ~ BMI, final_males), col="red")  # Add the red regression line
abline(tsls1, col="blue")  # Add the blue regression line
dev.off()  # Close the PDF device



## plot the results for sex_age adjusted result
pdf("./four_models.pdf")
plot(final_males$BMI, final_males$sbp, 
     xlab = "Body Mass Index", 
     ylab = "Systolic Blood Pressure", 
     main = "")
abline(lm(sbp ~ BMI, final_males), col="red")  # Add the red regression line
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
plot(final_males$BMI, final_males$sbp, 
     xlab = "Body Mass Index", 
     ylab = "Systolic Blood Pressure", 
     main = "")
abline(lm(sbp ~ BMI, final_males), col="red")  # Add the red regression line
abline(tsls1, col="blue4")
abline(tsls2, col="green4")  # Add the blue regression line
abline(tsls3, col="red4")
legend("topright", 
       legend = c("Linear Regression", "2SLS Model 1", "2SLS Model 2", "2SLS Model 3"), 
       col = c("red", "blue4", "green4", "red4"), 
       pch = 16,  # Legend symbols as dots
       cex = 0.6)
dev.off()  # Close the PDF device
