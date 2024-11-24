# Load external scripts for data extraction, auxiliary functions, and custom color schemes
source("./code/02_extract.R")  
source("./code/auxFunctions.R")
source("./code/predictor_colors.R")

# Load necessary libraries
library(MASS)      # For stepwise regression
library(reshape2)  # For reshaping data
library(ggplot2)   # For plotting

# Suppress warnings for cleaner output
options(warn = -1)

# Set seed for reproducibility
set.seed(1)

## Study 1 ####
# Subset the data for Study 1 and aggregate to get only the first row per participant
df <- subset(dfs, Study == 1)
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)

# Select the relevant metrics for regression analysis
metrics <- df[c("mPA", "sdPA", "mNA", "sdNA", "P2N_ASR")]

# Initialize an empty dataframe to store the stepwise regression results for Study 1
cols <- colnames(metrics)
rows <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS")  # Outcomes of interest
lres <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))  # Create empty dataframe
colnames(lres) <- cols
rownames(lres) <- rows

# Perform stepwise regression for each outcome in Study 1
for (i in 1:length(rows)) {
  outcome <- rows[i]  # Define the outcome variable (e.g., GAD, PHQ)
  res <- stepwiseRegression(df[[outcome]], metrics)  # Perform stepwise regression
  lres[i, names(res$coefficients[-1])] <- res$coefficients[-1]  # Store coefficients (exclude intercept)
}

## Study 2 ####
# Subset the data for Study 2 and aggregate to get only the first row per participant
df <- subset(dfs, Study == 2)
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)

# Select the relevant metrics for regression analysis
metrics <- df[c("mPA", "sdPA", "mNA", "sdNA", "P2N_ASR")]

# Initialize an empty dataframe to store the stepwise regression results for Study 2
cols <- colnames(metrics)
rows <- c("DASSa", "DASSd", "AAQ", "BRS", "SWLS")  # Outcomes of interest for Study 2
lres2 <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))  # Create empty dataframe
colnames(lres2) <- cols
rownames(lres2) <- rows

# Perform stepwise regression for each outcome in Study 2
for (i in 1:length(rows)) {
  outcome <- rows[i]  # Define the outcome variable (e.g., DASSa, DASSd)
  res <- stepwiseRegression(df[[outcome]], metrics)  # Perform stepwise regression
  lres2[i, names(res$coefficients[-1])] <- res$coefficients[-1]  # Store coefficients (exclude intercept)
}

# Transpose and round results for both Study 1 and Study 2
lres1 <- t(round(lres, 2))  # Transpose and round results for Study 1
lres2 <- t(round(lres2, 2))  # Transpose and round results for Study 2

# Combine results from both studies
lres <- cbind(lres1, lres2)  # Combine Study 1 and Study 2 results
rownames(lres)[5] <- "P2N-ASR"  # Rename row for clarity

# Optional: Save the results to a CSV file
# write.csv(lres, file = "./tables/stepwiseData.csv")

# Create and save Stepwise regression table (optional)
source("./code/table_stepwise.R")
# Uncomment the lines below to save the table in different formats:
# gtsave(tbl, "./tables/table_stepwise.html")
# gtsave(tbl, file = "./tables/table_stepwise.pdf")
# gtsave(tbl, file = "./tables/table_stepwise.png")
# gtsave(tbl, file = "./tables/table_stepwise.docx")

# Visualization: Create a data frame for plotting
data <- data.frame(t(lres))  # Convert results to a data frame
colnames(data)[5] <- "P2N-ASR"  # Rename column for clarity
data$outcome <- rownames(data)  # Add 'outcome' as a separate column

# Reshape data for plotting
data_long <- melt(data, id.vars = "outcome", variable.name = "predictor", value.name = "coefficient")

# Define x-axis labels for the plot
xlabels <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS", "DASSa", "DASSd", "AAQ", "BRS", "SWLS")

# Create Stepwise regression plot
p <- ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Plot coefficients as bars
  scale_fill_manual(values = predictor_colors, name = "Predictor") +  # Use custom color palette for predictors
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels) +  # Customize x-axis labels
  theme_minimal() +  # Use minimal theme for a clean plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  labs(title = "Stepwise regression",
       x = "Well-being Indicator",
       y = "Coefficient") +  # Add titles and labels to the plot
  theme(axis.title = element_text(size = 24),  # Customize axis title size
        axis.text = element_text(size = 20, colour = "black"),  # Customize axis text size and color
        plot.title = element_text(size = 24),  # Customize plot title size
        legend.text = element_text(size = 16),  # Customize legend text size
        legend.title = element_text(size = 16))  # Customize legend title size

# Add vertical dashed line to separate studies
p <- p + geom_vline(xintercept = 6.5, linetype = "dashed", color = "black", size = 1)

# Annotate the plot for Study 1 (Spanish data)
p <- p + annotate("text", x = 3.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "Spanish", size = 8)

# Annotate the plot for Study 2 (German data)
p <- p + annotate("text", x = 8.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "German", size = 8)

# Optional: Save the plot as a PDF
# ggsave("./figures/Stepwise.pdf", p, width = 10, height = 6, units = "in")