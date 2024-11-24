# Load external extraction script
source("./code/02_extract.R")

# Load the 'corrplot' package for correlation matrix visualization
library(corrplot)

# Study 1: Extract relevant metrics
df <- subset(dfs, Study == 1)  # Subset the data for Study 1
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)  # Collapse the data to one row per participant
metricsS1 <- df[c("P2N_ASR", "N2P_ASR", 
                  "mPRT", "sdPRT", "mNRT", "sdNRT", 
                  "mP2N", "sdP2N", "mN2P", "sdN2P", 
                  "mPA", "sdPA", "mNA", "sdNA")]  # Select the relevant metrics

# Study 2: Extract relevant metrics
df <- subset(dfs, Study == 2)  # Subset the data for Study 2
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)  # Collapse the data to one row per participant
metricsS2 <- df[c("P2N_ASR", "N2P_ASR", 
                  "mPRT", "sdPRT", "mNRT", "sdNRT", 
                  "mP2N", "sdP2N", "mN2P", "sdN2P", 
                  "mPA", "sdPA", "mNA", "sdNA")]  # Select the relevant metrics

# Combine data from both studies
data <- bind_rows(metricsS1, metricsS2)

# Remove rows with missing data (listwise deletion)
data <- na.omit(data)

# Rename some columns for clarity
colnames(data)[1:2] <- c("P2N-ASR", "N2P-ASR")
colnames(data)[7:10] <- c("mP2N-ASM", "sdP2N-ASM", "mN2P-ASM", "sdN2P-ASM")

# Calculate the correlation matrix for the dataset
data_matrix <- cor(data)

# Compute p-values for the correlations using Pearson's method
p_values <- cor.mtest(data_matrix, method = "pearson")$p

# Save the plot as a PDF or PNG (optional)
# pdf(file = "./figures/Corrplot.pdf", width = 12, height = 10)
# png(file = "./figures/Corrplot.png")

# Create and customize a correlation plot
corrplot(data_matrix,
         method = "number",        # Display correlation coefficients
         type = "lower",           # Show only the lower triangle of the matrix
         p.mat = p_values,         # Include p-values for significance testing
         sig.level = 0.001,        # Set significance level for correlations
         insig = "blank",          # Hide insignificant correlations
         tl.cex = 1.5,             # Increase size of text labels
         cl.cex = 1.5,             # Increase size of color labels
         tl.font = "bold",         # Set text labels to bold font
         number.cex = 1.2)         # Increase size of the correlation coefficients
 
# Close the device for saving plots
# dev.off()
 