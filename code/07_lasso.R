source("./code/02_extract.R")
source("./code/auxFunctions.R")
source("./code/predictor_colors.R")

options(warn = -1)

library(reshape2)
library(glmnet)
library(caret)

set.seed(1)

## Study 1 ####
df <- subset(dfs,Study==1)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)

metrics <- df[c("Type",
                "P2N_ASR","N2P_ASR",
                "mPRT","sdPRT","mNRT","sdNRT",
                "mP2N","sdP2N","mN2P","sdN2P",
                "mPA","sdPA","mNA","sdNA")]

# empty data frame of lasso results
cols <- colnames(metrics)
rows <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS")
lres <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))
colnames(lres) <- cols
rownames(lres) <- rows

for (i in 1:length(rows)){
  outcome <- rows[i]
  res <- lassomodel(df[[outcome]],metrics,train=F)
  lres[i,res$DV_lasso$predictor] <- res$DV_lasso$coefficient
}

## Study 2 ####
df <- subset(dfs,Study==2)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)

metrics <- df[c("Type",
                "P2N_ASR","N2P_ASR",
                "mPRT","sdPRT","mNRT","sdNRT",
                "mP2N","sdP2N","mN2P","sdN2P",
                "mPA","sdPA","mNA","sdNA")]

cols <- colnames(metrics)
rows <- c("DASSa","DASSd","AAQ","BRS","SWLS")

lres2 <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))
colnames(lres2) <- cols
rownames(lres2) <- rows

for (i in 1:length(rows)){
  outcome <- rows[i]
  res <- lassomodel(df[[outcome]],metrics,train=F)
  lres2[i,res$DV_lasso$predictor] <- res$DV_lasso$coefficient
}

lres1 <- t(round(lres,2)) # transpose and round
lres2 <- t(round(lres2,2)) # transpose and round

lres <- cbind(lres1,lres2)
rownames(lres)[2:3] <- c("P2N-ASR","N2P-ASR")
rownames(lres)[8:11] <- c("mP2N-ASM","sdP2N-ASM","mN2P-ASM","sdN2P-ASM")

# # Create and save LASSO table
# source("./code/table_lasso.R")
# gtsave(tbl, "./tables/table_lasso.html")
# gtsave(tbl, file = "./tables/table_lasso.pdf")
# gtsave(tbl, file = "./tables/table_lasso.png")
# gtsave(tbl, file = "./tables/table_lasso.docx")

# Create and save LASSO figure
data <- data.frame(t(lres))
colnames(data)[2:3] <- c("P2N-ASR","N2P-ASR")
colnames(data)[8:11] <- c("mP2N-ASM","sdP2N-ASM","mN2P-ASM","sdN2P-ASM")
data$outcome <- rownames(data)

data_long <- melt(data, id.vars = "outcome", variable.name = "predictor",
                  value.name = "coefficient")

xlabels <- c("GAD", "PHQ" ,"AAQ","FS","BRS","SWLS","DASSa","DASSd", "AAQ", 
             "BRS",  "SWLS")

p <- ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 1.4), width = 1.2) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "LASSO regression",
       x = "Well-being Indicator",
       y = "Coefficient")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"), 
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))

p <- p + geom_vline(xintercept = 6.5, linetype = "dashed", color = "black", size = 1)
p <- p + annotate("text", x = 3.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "Spanish", size = 8)
p <- p + annotate("text", x = 8.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "German", size = 8)


# ggsave("./figures/LASSO.pdf", p, width = 10,
#        height = 6, units = "in")