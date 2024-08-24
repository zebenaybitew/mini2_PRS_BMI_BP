################################################################################
#################### Generating Association with BMI ##########################

rm(list = ls())

############## Set Working directory #########################################

setwd("/Users/cr23646/Desktop/Mini2/PRS")

remotes::install_github('MRCIEU/TwoSampleMR', force = TRUE)
# load libraries 
library(dplyr)
library(data.table)
library(TwoSampleMR)
library(MRInstruments)
library(ieugwasr)
library(readr)
library(remotes)
library(MVMR)
library(ggplot2)

## Read the combined data set
Final <- read.csv("/Users/cr23646/Desktop/Mini2/PRS/Final.csv")
Final<-Final[, -3]

## Rename column values 
names(Final)[names(Final) == "X31.0.0"] <- "sex"
names(Final)[names(Final) == "X21001.0.0"] <- "BMI"
names(Final)[names(Final) == "X23104.0.0"] <- "BMI1"

## change BMI to numeric values 
Final$BMI <- as.numeric(Final$BMI)
Final$BMI1 <- as.numeric(Final$BMI1)

## exclude na values 
Final <- Final[!is.na(BMI)]
Final <- Final[!is.na(Final$BMI), ]
Final <- Final [, -9]
Final <- Final[!is.na(Final$PRS), ]

## Check the distribution of BMI 
hist (Final$BMI, breaks = 50)

## Run the GLM analyses (BMI vs SCOREstd)
model<- glm(BMI~SCOREstd, data = Final)
summary(model)

##Run the GLM analyses (BMI vs SCOREstd and sex (0=female and 1=male ))
Final$sex <- as.character(Final$sex)
model2<- glm(BMI~SCOREstd+sex, data = Final)
summary(model2)
# Fit the model
model2 <- glm(BMI ~ SCOREstd + sex, data = Final)

# Print the summary of the model
summary(model2)

# Compute 95% confidence intervals for the model coefficients
confint(model2)






## add scatter plot between BMI and Standardized PRS 
# Fit a linear model first 
model <- lm(BMI ~ SCOREstd, data = Final)

# Extract the regression coefficient (slope) and R-squared
slope <- coef(model)["SCOREstd"]
r_squared <- summary(model)$r.squared

# Create the plot with a regression line, slope, and R-squared
Plot2<-ggplot(Final, aes(x = SCOREstd, y = BMI)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line with confidence interval
  labs(title = "Scatter Plot of Standardized PRS and BMI with Regression Line",
       x = "Standardized PRS",
       y = "BMI") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Slope =", round(slope, 2)), 
           hjust = 2.0, vjust = 2, size = 5, color = "red") +  # Slope annotation
  annotate("text", x = Inf, y = Inf, 
           label = paste("R^2 =", round(r_squared, 2)), 
           hjust = 2.0, vjust = 3.5, size = 5, color = "blue") +  # R-squared annotation
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

## save the figure
ggsave(filename = "scatter_plot_PRS_BMI.pdf", plot = Plot2, width = 6, height = 6, dpi = 300)

## Sactter plot based on sex
# Fit a linear model
library(ggplot2)
library(dplyr)

# Calculate slope and R-squared for each sex group
stats <- Final %>%
  group_by(sex) %>%
  do({
    model <- lm(BMI ~ SCOREstd, data = .)
    data.frame(
      slope = coef(model)["SCOREstd"],
      r_squared = summary(model)$r.squared,
      x_max = max(.$SCOREstd, na.rm = TRUE),
      y_max = max(.$BMI, na.rm = TRUE)
    )
  })

# Create the plot with separate scatter plots based on sex and annotations
Plot3<-ggplot(Final, aes(x = SCOREstd, y = BMI)) +
  geom_point(aes(color = factor(sex))) +  # Scatter plot with color by sex
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line
  labs(title = "Scatter Plot of Standardized PRS and BMI by Sex",
       x = "Standardized PRS",
       y = "BMI") +
  facet_wrap(~ sex, labeller = labeller(sex = c("0" = "Female", "1" = "Male"))) +  # Separate plots for sex
  geom_text(data = stats, aes(x = x_max, y = y_max, 
                              label = paste("Slope =", round(slope, 2))),
            color = "red", size = 5, hjust = 1.1, vjust = 1.5) +  # Slope annotation
  geom_text(data = stats, aes(x = x_max, y = y_max - 1, 
                              label = paste("R^2 =", round(r_squared, 2))),
            color = "blue", size = 5, hjust = 1.1, vjust = 2) +  # R-squared annotation
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

## save the figure
ggsave(filename = "scatter_plot_PRS_BMI_sex_separated.pdf", plot = Plot3, width = 6, height = 6, dpi = 300)

## Convert SCOREstd to deciles
# Create deciles for SCOREstd
Final$SCOREstd_decile <- cut(Final$SCOREstd, 
                             breaks = quantile(Final$SCOREstd, probs = 0:10/10, na.rm = TRUE), 
                             labels = FALSE, 
                             include.lowest = TRUE)

# Check the first few rows to verify
head(Final)

## Bar plot of the decile
# Calculate frequency of each decile
decile_counts <- table(Final$SCOREstd_decile)

# Create a bar plot of decile frequencies
# Convert deciles to a factor for better plotting
#Final$SCOREstd_decile <- factor(Final$SCOREstd_decile, 
                                #levels = 1:10, 
                                #labels = paste("Decile", 1:10))

# Create a bar plot of decile frequencies using ggplot2
ggplot(Final, aes(x = SCOREstd_decile)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequency of Each SCOREstd Decile",
       x = "SCOREstd Decile",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
        plot.title = element_text(hjust = 0.5))  # Center the title

## plot decile PRS VS mean BMI
# Calculate mean BMI for each decile
mean_bmi_per_decile <- Final %>%
  group_by(SCOREstd_decile) %>%
  summarize(mean_BMI = mean(BMI, na.rm = TRUE), .groups = 'drop')

# Convert deciles to a factor for better plotting
mean_bmi_per_decile$SCOREstd_decile <- factor(mean_bmi_per_decile$SCOREstd_decile, 
                                              levels = 1:10, 
                                              labels = paste("Decile", 1:10))

# Verify data
print(mean_bmi_per_decile)

# Create a bar plot of mean BMI for each decile
plot4<-ggplot(mean_bmi_per_decile, aes(x = SCOREstd_decile, y = mean_BMI)) +
  geom_point(size = 4, color = "black") +  # Customize the size and color of the dots
  labs(x = "Polygenic Risk Score Decile",
       y = "Mean BMI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
        panel.background = element_blank(),  # Remove background color from the plotting area
        plot.background = element_rect(fill = "white"),  # Set the plot background to white color
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))  # Add axis lines if desired

## save the figure
ggsave(filename = "decile_prs_bmi.pdf", plot = plot4, width = 6, height = 6, dpi = 300)

## Write the data
write.table(Final, 'Final_data_4_lm.txt', sep = "\t", row.names = TRUE, col.names = TRUE)

## add a new column called BMI category 
# Add a new column 'BMI_cat' to the Final data frame based on BMI classification
Final$BMI_cat <- with(Final, 
                      ifelse(BMI < 18.5, 1,  # Underweight
                             ifelse(BMI >= 18.5 & BMI < 25, 2,  # Normal weight
                                    ifelse(BMI >= 25 & BMI < 30, 3,  # Overweight
                                           ifelse(BMI >= 30, 4, NA)))))  # Obesity
##bar plot
# Calculate the counts of each BMI category
bmi_counts <- table(Final$BMI_cat)

# Calculate the frequencies (proportions)
bmi_frequencies <- bmi_counts / sum(bmi_counts)

# Create the bar plot
bp <- barplot(bmi_frequencies,
              main = "Distribution of BMI Categories",
              xlab = "BMI Category",
              ylab = "Frequency",
              names.arg = c("Underweight", "Normal Weight", "Overweight", "Obesity"),  # Add labels for categories
              col = c("lightblue", "lightgreen", "orange", "red"),  # Customize the colors
              width = 0.05,  # Adjust the width of the bars
              ylim = c(0, max(bmi_frequencies) + 0.05))  # Add space above bars for labels

# Add frequency labels on top of the bars
text(x = bp, 
     y = bmi_frequencies, 
     label = round(bmi_frequencies, 3),  # Round to 3 decimal places for clarity
     pos = 3,  # Position above the bars
     cex = 0.8,  # Adjust the size of the text
     col = "black")  # Text color




## Reclassify deciles 
# Reclassify the SCOREstd_decile into three categories and create a new column 'decile_cat'
Final$decile_cat <- with(Final,
                         ifelse(SCOREstd_decile == 1, "Bottom Decile",
                                ifelse(SCOREstd_decile == 10, "Top Decile",
                                       "Middle Deciles (2-9)")))


## Bar plot of BMI cat and Decile_cat 
# Calculate the counts and frequencies of BMI categories within each decile category
bmi_freq <- Final %>%
  group_by(decile_cat, BMI_cat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(decile_cat) %>%
  mutate(frequency = count / sum(count))  # Compute frequency relative to the decile category

# Create the bar plot with frequencies and labels
plot5<-ggplot(bmi_freq, aes(x = factor(BMI_cat), y = frequency, fill = decile_cat)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +  # Adjust width of the bars
  geom_text(aes(label = round(frequency, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +  # Add labels on top of each bar
  labs(title = " ", 
       x = "Weight",
       y = "Frequency",
       fill = "Polygenic risk score") +  # Add a legend
  scale_x_discrete(labels = c("Underweight", "Normal Weight", "Overweight", "Obesity")) +  # Customize x-axis labels
  theme_minimal() +
  theme(legend.position = "right", 
  plot.title = element_text(hjust = 0.5)
  )

## save the figure
ggsave(filename = "decile_prs_bmi2.pdf", plot = plot5, width = 10, height = 6, dpi = 300)

# Save the plot as a PNG file with a transparent background
ggsave(filename = "bmi_categories_by_decile.png", 
       plot = plot5, 
       width = 10, 
       height = 6, 
       dpi = 300, 
       bg = "transparent")  # Set background color to transparent

## Plot Histogram of BMI based on  deciles 
library(ggplot2)
library(dplyr)

# Calculate the mean BMI for each decile category
mean_bmi <- Final %>%
  group_by(decile_cat) %>%
  summarize(mean_BMI = mean(BMI, na.rm = TRUE), .groups = 'drop')

# Convert decile_cat to a factor for better legend control
Final$decile_cat <- factor(Final$decile_cat, levels = c(1, 2, 3), labels = c("Bottom Decile", "Middle Deciles", "Top Decile"))

# Create a named vector for mean values to use in the legend
mean_bmi_legend <- setNames(mean_bmi$mean_BMI, levels(Final$decile_cat))

# Create the histogram plot
ggplot(Final, aes(x = BMI, fill = factor(decile_cat))) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +  # Adjust binwidth as needed
  labs(title = "Histogram of BMI by Decile Category",
       x = "BMI",
       y = "Count",
       fill = "Polygenic risk score") +  # Add legend title
  scale_fill_manual(values = c("red", "blue", "green"),  # Customize colors
                    labels = paste(names(mean_bmi_legend), "Mean BMI =", round(mean_bmi_legend, 2))) +  # Add mean values to legend
  geom_vline(data = mean_bmi, aes(xintercept = mean_BMI, color = factor(decile_cat)),
             linetype = "dashed", size = 1) +  # Add vertical lines for mean BMI
  geom_text(data = mean_bmi, aes(x = mean_BMI, y = Inf, label = round(mean_BMI, 2), color = factor(decile_cat)),
            vjust = -0.5, size = 3.5, hjust = -0.1) +  # Add labels for mean BMI
  theme_minimal() +
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(hjust = 0.5)  # Center the main title
  )


model3<-glm(BMI~decile_cat, data = Final)
summary(model3)

################
library(ggplot2)
library(dplyr)

# Fit the GLM model
model3 <- glm(BMI ~ decile_cat, data = Final)

# Summary of the model
summary(model3)

# Compute the 95% confidence intervals for the estimates
conf_intervals <- confint(model3)

# Extract the coefficients (betas)
betas <- coef(model3)

# Combine the betas and confidence intervals into a data frame
beta_data <- data.frame(
  term = names(betas),
  estimate = betas,
  conf.low = conf_intervals[, 1],
  conf.high = conf_intervals[, 2]
)

# Plot the betas with 95% CI

# Filter the data to exclude the intercept and bottom decile (reference)
  filtered_beta_data <- beta_data %>% 
  filter(term %in% c("decile_cat2", "decile_cat3"))

# Create the forest plot
  # Remove scale_y_discrete to check if the plot works without it
  PP2 <- ggplot(filtered_beta_data, aes(x = estimate, y = term)) +
    geom_point(size = 3) +  # Plot the beta estimates as points
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +  # Horizontal error bars for 95% CI
    geom_text(aes(label = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)),
              hjust = -0.1, vjust = 0.5, size = 3.5) +  # Add labels for estimates and CIs
    labs(title = "Forest Plot of Beta Coefficients with 95% Confidence Intervals",
         x = "Estimate",
         y = "Decile Category") +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Ensure y-axis labels are aligned
  
  # Try saving the plot

  


  
