source("./code/02_extract.R")
library(corrplot)

df <- subset(dfs,Study==1)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)
metricsS1 <- df[c("P2N_ASR","N2P_ASR",
                  "mPRT","sdPRT","mNRT","sdNRT",
                  "mP2N","sdP2N","mN2P","sdN2P",
                  "mPA","sdPA","mNA","sdNA")]

df <- subset(dfs,Study==2)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)
metricsS2 <- df[c("P2N_ASR","N2P_ASR",
                  "mPRT","sdPRT","mNRT","sdNRT",
                  "mP2N","sdP2N","mN2P","sdN2P",
                  "mPA","sdPA","mNA","sdNA")]

data <- bind_rows(metricsS1,metricsS2)
data <- na.omit(data)
colnames(data)[1:2] <- c("P2N-ASR","N2P-ASR")
colnames(data)[7:10] <- c("mP2N-ASM","sdP2N-ASM","mN2P-ASM","sdN2P-ASM")
data_matrix <- cor(data)

# Compute the p-values
p_values <- cor.mtest(data_matrix, method = "pearson")$p

pdf(file = "./figures/Corrplot.pdf", width = 12, height = 10)
# png(file = "./figures/Corrplot.png")
corrplot(data_matrix,
         method = "number",
         type = "lower",
         p.mat = p_values,
         sig.level = 0.001,
         insig = "blank",
         tl.cex = 1.5,       # Increase text label size
         cl.cex = 1.5,       # Increase color label size
         tl.font = "bold",
         number.cex = 1.2)   # Set text label font weight to bold)
dev.off()