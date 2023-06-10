source("./code/02_extract.R")
source("./code/auxFunctions.R")
source("./code/predictor_colors.R")

library(MASS)
library(reshape2)
library(ggplot2)

options(warn = -1)
set.seed(1)

## Study 1 ####
df <- subset(dfs,Study==1)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)

metrics <- df[c("mPA","sdPA","mNA","sdNA",
                "P2N_ASR")]

# empty data frame of lasso results
cols <- colnames(metrics)
rows <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS")
lres <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))
colnames(lres) <- cols
rownames(lres) <- rows

## Models
for (i in 1:length(rows)){
  outcome <- rows[i]
  res <- stepwiseRegression(df[[outcome]],metrics)
  lres[i,names(res$coefficients[-1])] <- res$coefficients[-1]
}

## Study 2 ####
df <- subset(dfs,Study==2)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)

metrics <- df[c("mPA","sdPA","mNA","sdNA",
                "P2N_ASR")]

cols <- colnames(metrics)
rows <- c("DASSa","DASSd","AAQ","BRS","SWLS")
lres2 <- data.frame(matrix(ncol = length(cols), nrow = length(rows)))
colnames(lres2) <- cols
rownames(lres2) <- rows

for (i in 1:length(rows)){
  outcome <- rows[i]
  res <- stepwiseRegression(df[[outcome]],metrics)
  lres2[i,names(res$coefficients[-1])] <- res$coefficients[-1]
}

lres1 <- t(round(lres,2)) # transpose and round
lres2 <- t(round(lres2,2)) # transpose and round

lres <- cbind(lres1,lres2)
rownames(lres)[5] <- "P2N-ASR"

# # Create and save Stepwise table
# source("./code/table_stepwise.R")
# gtsave(tbl, "./tables/table_stepwise.html")
# gtsave(tbl, file = "./tables/table_stepwise.pdf")
# gtsave(tbl, file = "./tables/table_stepwise.png")
# gtsave(tbl, file = "./tables/table_stepwise.docx")

# Create and save Stepwise figure
data <- data.frame(t(lres))
colnames(data)[5] <- "P2N-ASR"
data$outcome <- rownames(data)

data_long <- melt(data, id.vars = "outcome", variable.name = "predictor", value.name = "coefficient")

xlabels <- c("GAD", "PHQ" ,"AAQ","FS","BRS","SWLS","DASSa","DASSd", "AAQ", 
             "BRS",  "SWLS")

p <- ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Stepwise regression",
       x = "Well-being Indicator",
       y = "Coefficient")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20,colour = "black"), 
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))

p <- p + geom_vline(xintercept = 6.5, linetype = "dashed", color = "black", size = 1)
p <- p + annotate("text", x = 3.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "Spanish", size = 8)
p <- p + annotate("text", x = 8.5, y = max(na.omit(data_long$coefficient)) * 0.95, label = "German", size = 8)

# ggsave("./figures/Stepwise.pdf", p, width = 10,
#        height = 6, units = "in")