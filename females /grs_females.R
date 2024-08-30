################## BMI_PRS using UKB data for females  #####################################

############## Set Working directory ###########################################

## removes anything already in memory
rm(list=ls())

setwd("/Users/cr23646/Desktop/Mini2/PRS/females")

## load libraries 
library(data.table)
library(tidyr)
library(dplyr)
# Load the ggplot2 package
library(ggplot2)

## Read the GRS Data
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



