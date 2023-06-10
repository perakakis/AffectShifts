source("./code/02_extract.R")
library(ggsci)
library(ggpubr)
library(earlywarnings)
library(scales)

# Study 1 ####
df <- subset(dfs,Study==1)
y <- df$valence[!is.na(df$valence)]

basins <- livpotential_ews(y)

d <- data.frame(basins$grid.points,basins$pot)
colnames(d) <- c("gpoints","pot")
h <- hist(y, breaks = 40, plot = FALSE)
d$pot <- scales::rescale(d$pot, to = range(h$density), from= range(d$pot))
lm <- photobiology::get_valleys(d$gpoints,d$pot,
                                ignore_threshold = -0.2,
                                strict = TRUE,
                                span = 3)
basinsind <- which(d$gpoints %in% lm$x)
dbasins <- d[basinsind,]

p <- gghistogram(df,
                 x = c("valence"),
                 y = "..density..",
                 binwidth = 1,
                 bins = 40,
                 ylab = "Density",
                 xlab = "Valence",
                 title = "Study 1",
                 # rug = TRUE,
                 color = pal_jco("default")(9)[1],
                 fill = pal_jco("default")(9)[1],
                 font.label = list(size = 20)
)
p <- p + geom_line(data = d,aes(x = gpoints, y = pot)) +
  scale_x_continuous(limits = c(-51,51),
                     breaks = seq(from = -50, to = 50, by = 50)) +
  scale_y_continuous(limits = c(0,0.06),
                     breaks = seq(from = 0, to = 0.06, by = 0.02)) +
  geom_point(data = dbasins, aes(x = gpoints, y = pot), color = "black") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30))

# save plot as PDF file
# ggsave("./figures/S1_histogram.pdf", p, width = 8, height = 6, units = "in")

# Study 2 ####
df <- subset(dfs,Study==2)
y <- df$valence[!is.na(df$valence)]

basins <- livpotential_ews(y)

d <- data.frame(basins$grid.points,basins$pot)
colnames(d) <- c("gpoints","pot")
h <- hist(y, breaks = 40, plot = FALSE)
d$pot <- scales::rescale(d$pot, to = range(h$density), from= range(d$pot))
lm <- photobiology::get_valleys(d$gpoints,d$pot,
                                ignore_threshold = -0.2,
                                strict = TRUE,
                                span = 3)
basinsind <- which(d$gpoints %in% lm$x)
dbasins <- d[basinsind,]

p <- gghistogram(df,
                 x = c("valence"),
                 y = "..density..",
                 binwidth = 1,
                 bins = 40,
                 ylab = "Density",
                 title = "Study 2",
                 xlab = "Valence", 
                 # rug = TRUE,
                 color = pal_jco("default")(9)[1],
                 fill = pal_jco("default")(9)[1],
                 font.label = list(size = 20)
)

p <- p + geom_line(data = d,aes(x = gpoints, y = pot)) +
  scale_x_continuous(limits = c(-51,51),
                     breaks = seq(from = -50, to = 50, by = 50)) +
  scale_y_continuous(limits = c(0,0.06),
                     breaks = seq(from = 0, to = 0.06, by = 0.02)) +
  geom_point(data = dbasins, aes(x = gpoints, y = pot), color = "black")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30))

# save plot as PDF file
# ggsave("./figures/S2_histogram.pdf", p, width = 8, height = 6, units = "in")

#### bistable subject ####
df <- subset(dfs,PID==75)
y <- df$valence[!is.na(df$valence)]

basins <- livpotential_ews(y)

d <- data.frame(basins$grid.points,basins$pot)
colnames(d) <- c("gpoints","pot")
h <- hist(y, breaks = 40, plot = FALSE)
d$pot <- scales::rescale(d$pot, to = range(h$density), from= range(d$pot))
lm <- photobiology::get_valleys(d$gpoints,d$pot,
                                ignore_threshold = -0.2,
                                strict = TRUE,
                                span = 3)
basinsind <- which(d$gpoints %in% lm$x)
dbasins <- d[basinsind,]

p <- gghistogram(df,
                 x = c("valence"),
                 y = "..density..",
                 binwidth = 1,
                 bins = 40,
                 ylab = "Density",
                 xlab = "Valence",
                 title = "Bistable",
                 color = pal_jco("default")(9)[1],
                 fill = pal_jco("default")(9)[1],
                 font.label = list(size = 20)
)
p <- p + geom_line(data = d,aes(x = gpoints, y = pot)) +
  scale_x_continuous(limits = c(-51,51),
                     breaks = seq(from = -50, to = 50, by = 50)) +
  scale_y_continuous(limits = c(0,0.08),
                     breaks = seq(from = 0, to = 0.08, by = 0.02)) +
  geom_point(data = dbasins, aes(x = gpoints, y = pot), color = "black")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30))

# ggsave("./figures/BistableSubject.pdf", p, width = 8, height = 6, units = "in")

# Time series for bistable subject
p <- ggline(data = data.frame(x = seq_along(y), y = y),
            x = "x", y = "y",
            xlab = "Measurements", ylab = "Valence", 
            main = "",
            plot_type = "l",
            ylim = c(-50, 50)) +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# ggsave("./figures/BistableSubject_time.pdf", p, width = 8, height = 6, units = "in")

# Metrics illustration for bistable subject
x_values <- seq(from = 1, to = length(y))

line_data <- data.frame(x = x_values, y = y)

lp <- ggline(line_data, x = "x", y = "y", color = "black", size = 1,
             plot_type = "l",
             ylab = "Valence",
             xlab = "Time",
             title = "Bistable") +
  scale_x_continuous(limits = c(1,80))+
  
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30))

# ggsave("./figures/MetricsIllustration.pdf", lp, width = 8, height = 6, units = "in")

#### monostable subject ####
df <- subset(dfs,PID==21)
y <- df$valence[!is.na(df$valence)]

basins <- livpotential_ews(y)

d <- data.frame(basins$grid.points,basins$pot)
colnames(d) <- c("gpoints","pot")
h <- hist(y, breaks = 40, plot = FALSE)
d$pot <- scales::rescale(d$pot, to = range(h$density), from= range(d$pot))
lm <- photobiology::get_valleys(d$gpoints,d$pot,
                                ignore_threshold = -0.2,
                                strict = TRUE,
                                span = 3)
basinsind <- which(d$gpoints %in% lm$x)
dbasins <- d[basinsind,]

p <- gghistogram(df,
                 x = c("valence"),
                 y = "..density..",
                 binwidth = 1,
                 bins = 40,
                 ylab = "Density",
                 xlab = "Valence",
                 title = "Monostable",
                 color = pal_jco("default")(9)[1],
                 fill = pal_jco("default")(9)[1],
                 font.label = list(size = 20)
)
p <- p + geom_line(data = d,aes(x = gpoints, y = pot)) +
  scale_x_continuous(limits = c(-51,51),
                     breaks = seq(from = -50, to = 50, by = 50)) +
  scale_y_continuous(limits = c(0,0.08),
                     breaks = seq(from = 0, to = 0.08, by = 0.02)) +
  geom_point(data = dbasins, aes(x = gpoints, y = pot), color = "black")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30))

# ggsave("./figures/MonostableSubject.pdf", p, width = 8, height = 6, units = "in")

# Time series for monostable subject
p <- ggline(data = data.frame(x = seq_along(y), y = y),
            x = "x", y = "y",
            xlab = "Measurements", ylab = "Valence", 
            main = "",
            plot_type = "l",
            ylim = c(-50, 50)) +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20), 
        plot.title = element_text(size = 30)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# ggsave("./figures/MonostableSubject_time.pdf", p, width = 8, height = 6, units = "in")