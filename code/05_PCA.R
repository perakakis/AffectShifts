# Load external script
source("./code/02_extract.R")

# Load necessary libraries
library(psych)    # For psychological analysis and PCA
library(ggsci)    # For custom color palettes in plots
library(factoextra)  # For visualizing PCA results

# Study 1: Extracting relevant metrics
df <- subset(dfs, Study == 1)  # Subset data for Study 1
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)  # Collapse the data to one row per participant
metricsS1 <- df[c("P2N_ASR", "N2P_ASR", "mPRT", "sdPRT", "mNRT", "sdNRT", "mP2N", "sdP2N", "mN2P", "sdN2P")]  # Select relevant metrics

# Study 2: Extracting relevant metrics
df <- subset(dfs, Study == 2)  # Subset data for Study 2
df <- aggregate(df, list(df$PID), data = df, FUN = head, 1)  # Collapse the data to one row per participant
metricsS2 <- df[c("P2N_ASR", "N2P_ASR", "mPRT", "sdPRT", "mNRT", "sdNRT", "mP2N", "sdP2N", "mN2P", "sdN2P")]  # Select relevant metrics

# Combine data from both studies
data <- bind_rows(metricsS1, metricsS2)

# Remove rows with missing data (listwise deletion)
data <- na.omit(data)

# Rename some columns for clarity
colnames(data)[1:2] <- c("P2N-ASR", "N2P-ASR")
colnames(data)[7:10] <- c("mP2N-ASM", "sdP2N-ASM", "mN2P-ASM", "sdN2P-ASM")

# Calculate the correlation matrix for the dataset
data_matrix <- cor(data)

# Perform diagnostic checks for PCA:
# KMO (Kaiser-Meyer-Olkin) measure of sampling adequacy (values > 0.7 are good)
KMO(data)

# Bartlett's test of sphericity (p-value should be < .05 to reject null hypothesis of identity matrix)
cortest.bartlett(data)

# Check determinant of the correlation matrix (value > 0.0001 suggests no multicollinearity)
det(data_matrix)

# Perform PCA using the 'psych' package with oblique rotation (oblimin)
pca <- principal(data, nfactors = 2, rotate = "oblimin")  # Extract 2 factors with oblique rotation

# Perform PCA using the 'prcomp' function with scaling and centering
res.pca <- prcomp(data, scale. = TRUE, center = TRUE)

# Optional: Scree plot to visualize explained variance by each principal component
# fviz_screeplot(res.pca, ncp = 10)

# Optional: Get eigenvalues for each component
# get_eigenvalue(res.pca)

# Extract variable loadings and contributions to each principal component
var <- get_pca_var(res.pca)

# Create a plot for the PCA results, coloring variables by their contribution to the principal components
p <- fviz_pca_var(res.pca,
                  col.var = "contrib",  # Color variables by their contribution
                  gradient.cols = c(pal_jco("default")(1),  # Set color gradient using the 'ggsci' palette
                                    pal_jco("default")(2)[2], "#FC4E07"),
                  repel = TRUE,  # Avoid overlapping labels
                  title = "",  # Remove title for clean presentation
                  xlab = "Component 1 (40.90%)",  # Label x-axis with explained variance
                  ylab = "Component 2 (22.58%)"  # Label y-axis with explained variance
) +
  # Customize plot appearance
  theme(axis.title = element_text(size = 14),  
        axis.text = element_text(size = 14),  
        text = element_text(size = 14),  
        plot.title = element_text(size = 0))  # Remove plot title

# Optional: Save the PCA plot as a PDF
# ggsave(filename = "./figures/PCA.pdf", plot = p, width = 5, height = 5, units = "in")