# Load external scripts and required packages
source("./code/02_extract.R")  # Data extraction script
source("./code/auxFunctions.R")  # Auxiliary functions
source("./code/predictor_colors.R")  # Custom colors for predictors

# Suppress warnings for cleaner output
options(warn = -1)

# Load necessary libraries
library(reshape2)  # For reshaping data
library(glmnet)    # For LASSO regression
library(caret)     # For training models

# Set seed for reproducibility
set.seed(1)

## Study 1 ####
# Subset the data for Study 1 and keep the first row per participant
df <- subset(dfs, Study == 1)
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)
df$Type <- as.factor(df$Type)  # Convert 'Type' to a factor

# Select the relevant metrics for LASSO regression
metrics <- df[c("Type",
                "P2N_ASR", "N2P_ASR", 
                "mPRT", "sdPRT", "mNRT", "sdNRT", 
                "mP2N", "sdP2N", "mN2P", "sdN2P", 
                "mPA", "sdPA", "mNA", "sdNA")]

# Initialize an empty dataframe for storing LASSO results for Study 1
cols <- colnames(metrics)
rows <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS")  # Outcomes of interest
lres <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))  # Create empty dataframe
colnames(lres) <- cols
rownames(lres) <- rows

# Perform LASSO regression for each outcome
for (i in 1:length(rows)) {
  outcome <- rows[i]  # Define the outcome variable
  res <- lassomodel(df[[outcome]], metrics)  # Perform LASSO regression using custom function
  lres[i, res$DV_lasso$predictor] <- res$DV_lasso$coefficient  # Store coefficients in result table
}

# Handle 'Type' column separately
lres$Type <- lres$Type1
lres <- lres[, -ncol(lres)]  # Remove the last column (Type1 placeholder)

## Study 2 ####
# Subset the data for Study 2 and keep the first row per participant
df <- subset(dfs, Study == 2)
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)
df$Type <- as.factor(df$Type)  # Convert 'Type' to a factor

# Select the relevant metrics for Study 2
metrics <- df[c("Type",
                "P2N_ASR", "N2P_ASR", 
                "mPRT", "sdPRT", "mNRT", "sdNRT", 
                "mP2N", "sdP2N", "mN2P", "sdN2P", 
                "mPA", "sdPA", "mNA", "sdNA")]

# Initialize an empty dataframe for storing LASSO results for Study 2
cols <- colnames(metrics)
rows <- c("DASSa", "DASSd", "AAQ", "BRS", "SWLS")  # Outcomes for Study 2
lres2 <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))  # Create empty dataframe
colnames(lres2) <- cols
rownames(lres2) <- rows

# Perform LASSO regression for each outcome in Study 2
for (i in 1:length(rows)) {
  outcome <- rows[i]  # Define the outcome variable
  res <- lassomodel(df[[outcome]], metrics)  # Perform LASSO regression
  lres2[i, res$DV_lasso$predictor] <- res$DV_lasso$coefficient  # Store coefficients
}

# Handle 'Type' column separately for Study 2
lres2$Type <- lres2$Type1
lres2 <- lres2[, -ncol(lres2)]  # Remove the last column (Type1 placeholder)

# Transpose and round results for both studies
lres1 <- t(round(lres, 2))  # Transpose and round results for Study 1
lres2 <- t(round(lres2, 2))  # Transpose and round results for Study 2

# Combine results from both studies
lres <- cbind(lres1, lres2)
rownames(lres)[2:3] <- c("P2N-ASR", "N2P-ASR")  # Rename rows for clarity
rownames(lres)[8:11] <- c("mP2N-ASM", "sdP2N-ASM", "mN2P-ASM", "sdN2P-ASM")

# Create and save LASSO table (if needed)
source("./code/table_lasso.R")
# Uncomment below to save results as a table:
# gtsave(tbl, "./tables/table_lasso.html")
# gtsave(tbl, file = "./tables/table_lasso.pdf")
# gtsave(tbl, file = "./tables/table_lasso.png")
# gtsave(tbl, file = "./tables/table_lasso.docx")

# Visualization: Create a data frame for plotting
data <- data.frame(t(lres))  # Convert results to a data frame
colnames(data)[2:3] <- c("P2N-ASR", "N2P-ASR")  # Rename columns for clarity
colnames(data)[8:11] <- c("mP2N-ASM", "sdP2N-ASM", "mN2P-ASM", "sdN2P-ASM")
data$outcome <- rownames(data)  # Add 'outcome' as a separate column

# Reshape data for plotting
data_long <- melt(data, id.vars = "outcome", variable.name = "predictor", value.name = "coefficient")

# Define x-axis labels
xlabels <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS", "DASSa", "DASSd", "AAQ", 
             "BRS", "SWLS")

# Create LASSO regression plot
p <- ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 1.4), width = 1.2) +  # Plot coefficients as bars
  scale_fill_manual(values = predictor_colors, name = "Predictor") +  # Use custom color palette
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels) +  # Customize x-axis labels
  theme_minimal() +  # Use minimal theme for plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "LASSO regression",
       x = "Well-being Indicator",
       y = "Coefficient") +  # Add labels
  theme(axis.title = element_text(size = 24),  # Customize axis title size
        axis.text = element_text(size = 20, colour = "black"),  # Customize axis text size
        plot.title = element_text(size = 24),  # Customize plot title size
        legend.text = element_text(size = 16),  # Customize legend text size
        legend.title = element_text(size = 16))  # Customize legend title size

# Add vertical line and annotations
p <- p + geom_vline(xintercept = 6.5, linetype = "dashed", color = "black", size = 1)  # Add vertical dashed line
p <- p + annotate("text", x = 3.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "Spanish", size = 8)  # Annotate Spanish data
p <- p + annotate("text", x = 8.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "German", size = 8)  # Annotate German data

# Optional: Save the plot as PDF
# ggsave("./figures/LASSO.pdf", p, width = 10, height = 6, units = "in")