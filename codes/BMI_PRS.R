################## BMI_PRS using UKB data #####################################

############## Set Working directory ###########################################

setwd("/Users/cr23646/Desktop/Mini2/PRS")

## load libraries 
library(data.table)
library(tidyr)
library(dplyr)
# Load the ggplot2 package
library(ggplot2)

## Read the GRS Data
grs_data <- read.table("/Users/cr23646/Desktop/Mini2/PRS/GRSscore.sscore", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

## View the data
head(grs_data)
dim(grs_data)
# change column names 
colnames(grs_data) <- c("IID", "CNT", "CNT2", "PRS")

## standardized the PRS 
grs_data$SCOREstd <- scale(grs_data$PRS, center=TRUE, scale=TRUE) 

# Converting the SCOREstd column to numeric
grs_data$SD_score <- as.numeric(grs_data$SD_score)

## Write the data
write.table(grs_data, 'grs_data.txt', sep = "\t", row.names = TRUE, col.names = TRUE)

## read data 
grs_data<-read.table("/Users/cr23646/Desktop/Mini2/PRS/grs_data.txt")
  
## rename the colname
names(grs_data)[names(grs_data) == "SD_score"] <- "SCOREstd"

############# plot a histogram of the standardized score 
# Calculate the mean and standard deviation
mean_score <- mean(grs_data$SD_score, na.rm = TRUE)
sd_score <- sd(grs_data$SD_score, na.rm = TRUE)

# Create the histogram with ggplot2
plot<-ggplot(grs_data, aes(x = SD_score)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_score, color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_score + sd_score, color = "Mean + SD"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_score - sd_score, color = "Mean - SD"), linetype = "dashed", size = 1) +
  labs(title = "Histogram of Standardized PRS Scores",
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
ggplot(grs_data, aes(y = SD_score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal(base_size = 8) + # Smaller base font size
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + # Smaller margins
  labs(title = "Boxplot of SD_score", y = "SD_score")

## Read the phenotypic data 
library(data.table)
Pheno <- fread("/Users/cr23646/Desktop/Mini2/PRS/pheno.txt", sep=",", header=TRUE, fill=TRUE, quote=FALSE, strip.white=TRUE)
library(data.table)



pheno_bmi <- fread("/Users/cr23646/Desktop/Mini2/PRS/pheno_bmi_filtered.txt", sep=",", header=TRUE, fill=TRUE, quote=FALSE, strip.white=TRUE)

pheno_bmi1 <- fread("/Users/cr23646/Desktop/Mini2/PRS/pheno_bmi_clean.txt", sep=",", header=TRUE, fill=TRUE, quote=FALSE, strip.white=TRUE) 

pheno_bmi1$BMI <- as.numeric(pheno_bmi1$BMI)
pheno_bmi1$BMI[pheno_bmi1$BMI>100 | pheno_bmi1$BMI<10] <- NA

BMI_filtered2 <- pheno_bmi1[!is.na(BMI)]

## Write the data
write.table(pheno_bmi, 'BMI_UKB_filtered.txt', sep = "\t", row.names = TRUE, col.names = TRUE)

############### combine GRS with phenotype and check if GRS is associated with BMI #########################
GRS<-grs_data
# Remove "IEU" prefix from the ID column
GRS$IID <- gsub("^IEU", "", grs_data$IID)
names(GRS)[names(GRS) == "IID"] <- "ID"

GRS_BMI<-merge(GRS, BMI_filtered2, by.x = "ID", all = FALSE)
# Perform linear regression
model <- lm(BMI ~ SCOREstd, data = GRS_BMI)
summary(model)
hist(BMI_filtered2$BMI, breaks = 40, col = "lightblue")



 