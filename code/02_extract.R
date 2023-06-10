source("./code/01_preprocess.R")
options(warn = -1)
set.seed(1)

pl <- F

PIDs <- unique(dfs$PID)

for (i in seq(1, length(PIDs))) {
  df <- subset(dfs,PID==PIDs[i])
  study <- unique(df$Study)
  y <- df$valence # select the valence variable
  
  # histograms
  h <- hist(y, breaks = 40, plot = FALSE)
  
  # Basins
  y3 <- y[!is.na(y)] # y3 = remove NaNs
 
  basins <- earlywarnings::livpotential_ews(y3)
  lm <- photobiology::get_valleys(basins$grid.points,basins$pot,
                                  ignore_threshold = -0.2,
                                  strict = TRUE,
                                  span = 3)
  
  basinsind <- which(basins$grid.points %in% lm$x)
  basinlocs <- lm$x
  
  if (any(basinlocs>0) & any(basinlocs<0)){
    type <- 1 #bistable
  } else {
    type <- 0 #monostable
  }
  
  dfs$Type[dfs$PID==PIDs[i]] <- type
  
  # PA,NA stats calculated from valence
  if (any(y3>0)){
    dfs$mPA[dfs$PID==PIDs[i]] <-
      round(mean(y3[y3>0]),digits = 3)
    dfs$sdPA[dfs$PID==PIDs[i]] <-
      round(sd(y3[y3>0]),digits = 3)
  } else {
    dfs$mPA[dfs$PID==PIDs[i]] <- NA
    dfs$sdPA[dfs$PID==PIDs[i]] <- NA
  }
  
  if (any(y3<0)){
    dfs$mNA[dfs$PID==PIDs[i]] <-
      abs(round(mean(y3[y3<0]),digits = 3))
    dfs$sdNA[dfs$PID==PIDs[i]] <-
      round(sd(y3[y3<0]),digits = 3)
  } else {
    dfs$mNA[dfs$PID==PIDs[i]] <- NA
    dfs$sdNA[dfs$PID==PIDs[i]] <- NA
  }
  
  # bistability metrics
  p2n <- which(diff(sign(y))==-2)
  n2p<- which(diff(sign(y))==2)
  
  if (length(p2n>0)) {
    dfs$P2N_ASR[dfs$PID==PIDs[i]] <- 
      round(length(p2n)/length(which(y>0)),digits = 3)
    dfs$mP2N[dfs$PID==PIDs[i]] <- 
      round(mean(y[p2n]+abs(y[p2n+1])),digits = 3)
    dfs$sdP2N[dfs$PID==PIDs[i]] <- 
      round(sd(y[p2n]+abs(y[p2n+1])),digits = 3)
  }
  
  if (length(n2p>0)) {
    dfs$N2P_ASR[dfs$PID==PIDs[i]] <-
      round(length(n2p)/length(which(y<0)),digits = 3)
    dfs$mN2P[dfs$PID==PIDs[i]] <- 
      round(mean(abs(y[n2p])+y[n2p+1]),digits = 3)
    dfs$sdN2P[dfs$PID==PIDs[i]] <- 
      round(sd(abs(y[n2p])+y[n2p+1]),digits = 3)
  } 
  
  # residence times
  PAt <- rle(sign(y))$lengths[(rle(sign(y))$values==1)]
  NAt <- rle(sign(y))$lengths[(rle(sign(y))$values==-1)]
  
  # positive
  if (length(PAt)>0 & !all(is.na(PAt))){
    dfs$mPRT[dfs$PID==PIDs[i]] <- 
      round(mean(PAt,na.rm = TRUE),digits = 3)
    dfs$sdPRT[dfs$PID==PIDs[i]] <- 
      round(sd(PAt,na.rm = TRUE),digits = 3)
  }
  
  # negative
  if (length(NAt)>0 & !all(is.na(NAt))){
    dfs$mNRT[dfs$PID==PIDs[i]] <- 
      round(mean(NAt,na.rm = TRUE),digits = 3)
    dfs$sdNRT[dfs$PID==PIDs[i]] <- 
      round(sd(NAt,na.rm = TRUE),digits = 3)
  }
  
  if(pl){
    
    pdf(paste0("./figures/subjects/PID",PIDs[i], ".pdf"), width=8, height=8)
    par(mfrow=c(2,1),mar = c(5, 3, 3, 3))
    
    plot(y,type = "l",
         ylim = c(-50,50),
         main = paste0("PID: ",PIDs[i], " Study = ", study),
         xlab = "",
         ylab = "")
    
    # histograms and basins
    plot(h, main = paste0("type = ",type),
         xlim = c(-50,50),
         xlab = "")
    par(new = TRUE)
    plot(basins$grid.points,basins$pot,xlim = c(-50,50),axes = FALSE, xlab = "",
         ylab = "",type = "l")
    axis(side = 4, at = pretty(range(basins$pot)))
    par(new = TRUE)
    plot(basins$grid.points[basinsind],basins$pot[basinsind],
         col = 2, xlim = c(-50,50),axes = FALSE, xlab = "", ylab = "",
         ylim = range(basins$pot))
    dev.off()
  }
}
