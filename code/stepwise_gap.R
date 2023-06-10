source("./code/08_stepwiseRegression.R")

library(patchwork)

xlabels1 <- c("GAD", "PHQ" ,"AAQ","FS","BRS","SWLS")
xlabels2 <- c("DASSa","DASSd", "AAQ", "BRS",  "SWLS")

# Study1 ####
rownames(lres1)[5] <- "P2N-ASR"
data <- data.frame(t(lres1))
colnames(data)[5] <- "P2N-ASR"
data$outcome <- rownames(data)

data_long <- melt(data, id.vars = "outcome", variable.name = "predictor", value.name = "coefficient")


p1=ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels1) +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1)) +  # Set specific tick values
  coord_cartesian(ylim = c(-3, 1)) +
  theme_minimal() +
  labs(x = "Well-being Indicator",
       y = "Coefficient") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"),
        axis.line.y = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        panel.grid = element_blank())
p1 <- p1 + geom_vline(aes(xintercept = as.numeric(predictor) + 0.5), 
                      color = "black", linetype = "dashed", size = 0.5)


p2=ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels1) +
  scale_y_continuous(breaks = c(10, 40)) +  # Set specific tick values
  coord_cartesian(ylim = c(6, 45)) +
  guides(x = "none")+ # remove x line
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Spanish study",
       x = "",
       y = "") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"),
        axis.line.y = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        plot.title = element_text(size = 24),
        panel.grid = element_blank())
p2 <- p2 + geom_vline(aes(xintercept = as.numeric(predictor) + 0.5), 
                      color = "black", linetype = "dashed", size = 0.5)

layout <- "
A
B
B
"
p <- p2/p1 +
  plot_layout( design = layout)

ggsave("./figures/Stepwise_gap_Study1.pdf", p, width = 10,
       height = 6, units = "in")




# Study2 ####
rownames(lres2)[5] <- "P2N-ASR"
data <- data.frame(t(lres2))
colnames(data)[5] <- "P2N-ASR"
data$outcome <- rownames(data)

data_long <- melt(data, id.vars = "outcome", variable.name = "predictor", value.name = "coefficient")

p1=ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels2) +
  scale_y_continuous(breaks = c(0, 1)) +  # Set specific tick values
  coord_cartesian(ylim = c(-0.5, 1)) +
  guides(x = "none") +
  theme_minimal() +
  labs(x = "",
       y = "Coefficient") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"),
        axis.line.y = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        panel.grid = element_blank())
p1 <- p1 + geom_vline(aes(xintercept = as.numeric(predictor) + 0.5), 
                      color = "black", linetype = "dashed", size = 0.5)

p2=ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels2) +
  scale_y_continuous(breaks = c(10, 30)) +  # Set specific tick values
  coord_cartesian(ylim = c(10, 30)) +
  guides(x = "none")+ # remove x line
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "German study",
       x = "",
       y = "") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"),
        axis.line.y = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        plot.title = element_text(size = 24),
        panel.grid = element_blank())
p2 <- p2 + geom_vline(aes(xintercept = as.numeric(predictor) + 0.5), 
                      color = "black", linetype = "dashed", size = 0.5)

p3=ggplot(data_long, aes(x = outcome, y = coefficient, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) + 
  scale_fill_manual(values = predictor_colors, name = "Predictor") +
  scale_x_discrete(limits = unique(data_long$outcome), labels = xlabels2) +
  scale_y_continuous(breaks = c(-23, -24)) +  # Set specific tick values
  coord_cartesian(ylim = c(-24, -23)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Well-being Indicator",
       y = "") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20, colour = "black"),
        axis.line.y = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        panel.grid = element_blank())
p3 <- p3 + geom_vline(aes(xintercept = as.numeric(predictor) + 0.5), 
                      color = "black", linetype = "dashed", size = 0.5)


layout <- "
A
B
B
C
"
p <- p2/p1/p3 +
  plot_layout( design = layout)

ggsave("./figures/Stepwise_gap_Study2.pdf", p, width = 10,
       height = 6, units = "in")