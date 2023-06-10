source("./code/02_extract.R")
source("./code/auxFunctions.R")
source("./code/predictor_colors.R")

options(warn = -1)

library(relaimpo)
library(reshape2)
library(ggsci)
library(ggplot2)

set.seed(1)
## Study 1 ####
df <- subset(dfs,Study==1)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)

metrics <- df[c("mPA","sdPA","mNA","sdNA",
                "P2N_ASR")]
outcomes <- c("GAD", "PHQ", "AAQ", "FS", "BRS", "SWLS")

lres <- data.frame(matrix(ncol = length(metrics),
                          nrow = length(outcomes)))
colnames(lres) <- colnames(metrics)
rownames(lres) <- outcomes

for (i in 1:length(outcomes)){
  outcome <- outcomes[i]
  res <- relativeImportance(df[[outcome]],metrics)
  lres[i,names(res$lmg)] <- res$lmg
}

## Study 2 ####
df <- subset(dfs,Study==2)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)

metrics <- df[c("mPA","sdPA","mNA","sdNA",
                "P2N_ASR")]
outcomes <- c("DASSa","DASSd","AAQ","BRS","SWLS")

lres2 <- data.frame(matrix(ncol = length(metrics), nrow = length(outcomes)))
colnames(lres2) <- colnames(metrics)
rownames(lres2) <- outcomes

for (i in 1:length(outcomes)){
  outcome <- outcomes[i]
  res <- relativeImportance(df[[outcome]],metrics)
  lres2[i,names(res$lmg)] <- res$lmg
}

data <- rbind(lres,lres2)
colnames(data)[5] <- "P2N-ASR"
data$outcome <- rownames(data)

data_long <- melt(data, id.vars = "outcome", variable.name = "predictor", value.name = "importance")

xlabels <- c("GAD", "PHQ" ,"AAQ","FS","BRS","SWLS","DASSa","DASSd", "AAQ", 
             "BRS",  "SWLS")

p <- ggplot(data_long, aes(x = outcome, y = importance, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Relative importance",
       x = "Well-being Indicator",
       y = "Relative Importance")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"), 
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))

p <- p + geom_vline(xintercept = 6.5, linetype = "dashed", color = "black", size = 1)
p <- p + annotate("text", x = 3.5, y = max(na.omit(data_long$importance)) * 0.95, label = "Spanish", size = 8)
p <- p + annotate("text", x = 8.5, y = max(na.omit(data_long$importance)) * 0.95, label = "German", size = 8)


# ggsave("./figures/relImportance.pdf", p, width = 10,
#        height = 6, units = "in")