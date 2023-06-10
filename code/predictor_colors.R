library(ggsci)
library(scico)
library(RColorBrewer)

metrics <- c("Type",
             "P2N-ASR","N2P-ASR",
             "mPRT","sdPRT","mNRT","sdNRT",
             "mP2N-ASM","sdP2N-ASM","mN2P-ASM","sdN2P-ASM",
             "mPA","sdPA","mNA","sdNA")
num_colors <- length(metrics)
color_palette <- pal_d3("category20")(num_colors)
jco <- pal_jco()(5)
color_palette <- c(color_palette[15],
                   "#0073C2FF",
                   color_palette[6:14],
                   jco[2:5])

# color_palette <- scico(num_colors,palette = "berlin")
# 
# color_palette <- colorRampPalette(brewer.pal(15, "Paired"))(num_colors)

predictor_colors <- setNames(color_palette, metrics)
