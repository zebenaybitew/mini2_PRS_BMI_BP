################## BMI_GRS using UKB data sex combined #####################################

############## Set Working directory ###########################################

setwd("/Users/cr23646/Desktop/Mini2/PRS")

## load libraries 
library(data.table)
library(tidyr)
library(dplyr)
# Load the ggplot2 package
library(ggplot2)

## Read the GRS Data
#grs_data <- read.table("/Users/cr23646/Desktop/Mini2/PRS/GRSscore.sscore", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

## View the data
#head(grs_data)
#dim(grs_data)
# change column names 
#colnames(grs_data) <- c("IID", "CNT", "CNT2", "PRS")

## standardized the PRS 
#grs_data$SCOREstd <- scale(grs_data$PRS, center=TRUE, scale=TRUE) 

# Converting the SCOREstd column to numeric
#grs_data$SCOREstd <- as.numeric(grs_data$SCOREstd)



## Write the data
write.table(grs_data, 'grs_data_sex_combined.txt', sep = "\t", row.names = TRUE, col.names = TRUE)

## read data 
grs_data<-read.table("/Users/cr23646/Desktop/Mini2/PRS/grs_data_sex_combined.txt")

write.csv(grs_data, 'grs_data_sex_combined.csv', row.names = TRUE)

############# plot a histogram of the standardized score 
# Calculate the mean and standard deviation
mean_score <- mean(grs_data$SCOREstd, na.rm = TRUE)
sd_score <- sd(grs_data$SCOREstd, na.rm = TRUE)

# Create the histogram with ggplot2
plot<-ggplot(grs_data, aes(x = SCOREstd)) +
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
ggplot(grs_data, aes(y = SCOREstd)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal(base_size = 8) + # Smaller base font size
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + # Smaller margins
  labs(title = "Boxplot of SD_score", y = "SD_score")








 