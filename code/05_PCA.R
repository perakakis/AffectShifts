source("./code/02_extract.R")

library(psych)
library(ggsci)
library(factoextra)

df <- subset(dfs,Study==1)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)
metricsS1 <- df[c("P2N_ASR","N2P_ASR",
                  "mPRT","sdPRT","mNRT","sdNRT",
                  "mP2N","sdP2N","mN2P","sdN2P")]

df <- subset(dfs,Study==2)
df <- aggregate(df, list(df$PID), data = df, FUN = head,1)
metricsS2 <- df[c("P2N_ASR","N2P_ASR",
                  "mPRT","sdPRT","mNRT","sdNRT",
                  "mP2N","sdP2N","mN2P","sdN2P")]

data <- bind_rows(metricsS1,metricsS2)

data <- na.omit(data)
colnames(data)[1:2] <- c("P2N-ASR","N2P-ASR")
colnames(data)[7:10] <- c("mP2N-ASM","sdP2N-ASM","mN2P-ASM","sdN2P-ASM")
data_matrix <- cor(data) 

# checks https://lhbikos.github.io/ReC_Psychometrics/PCA.html
KMO(data) # sampling adequacy (above .7)
cortest.bartlett(data) # test identity (p<.05)
det(data_matrix) #multicolinearity (above 0.0001)

# PCA psych package to see correlation between dimensions
pca <- principal(data, nfactors=2, rotate="oblimin") #oblique(oblimin)

# PCA with prcomp
res.pca <- prcomp(data,scale. = T, center = T)

# fviz_screeplot(res.pca, ncp=10)
# get_eigenvalue(res.pca)

var <- get_pca_var(res.pca)

p <- fviz_pca_var(res.pca,
                  col.var = "contrib", # Color by contributions to the PC
                  gradient.cols = c(pal_jco("default")(1),
                                    pal_jco("default")(2)[2],"#FC4E07"),
                  repel = TRUE,
                  title = "",
                  xlab = "Component 1 (40.90%)",
                  ylab = "Component 2 (22.58%)"
) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14),
        text = element_text(size = 14),
        plot.title = element_text(size = 0)) 

ggsave(filename = "./figures/PCA.pdf", plot = p,width = 5, height = 5, units = "in")
