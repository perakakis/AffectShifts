# Preprocessing step
source("./code/01_preprocess.R") # Comment this line if performing reliability analysis only

# Load the 'zoo' package for handling time series data
library(zoo)

# Suppress warnings for cleaner output
options(warn = -1)

# Set a seed for reproducibility in case of random processes
set.seed(1)

# Set the plot flag (pl) to FALSE. If TRUE, plots will be generated and saved.
pl <- FALSE

# Extract unique participant IDs (PIDs) from the dataframe 'dfs'
PIDs <- unique(dfs$PID)

# Initialize an empty vector to store participants with no valence shifts
Noshifts <- c()

# Loop through each participant
for (i in seq(1, length(PIDs))) {
  
  # Subset the data for the current participant
  df <- subset(dfs, PID == PIDs[i])
  
  # Identify the study (Study 1 or Study 2)
  study <- unique(df$Study)
  
  # Extract the valence variable for analysis
  y <- df$valence
  
  # Generate histograms for valence (without plotting for now)
  h <- hist(y, breaks = 40, plot = FALSE)
  
  # Remove missing values from the valence variable
  y3 <- y[!is.na(y)]  
  
  # Estimate basins using Early Warnings Signals' LivPotential function
  basins <- earlywarnings::livpotential_ews(y3)
  
  # Identify local minima (valleys) in the potential function
  lm <- photobiology::get_valleys(
    basins$grid.points, basins$pot, 
    ignore_threshold = -0.2, 
    strict = TRUE, 
    span = 3
  )
  
  # Extract indices of local minima (valleys)
  basinsind <- which(basins$grid.points %in% lm$x)
  basinlocs <- lm$x  # Coordinates of basin locations
  
  # Check if exactly one local minimum exists in the range [-5, 5]
  values_in_range <- sum(basinlocs >= -5 & basinlocs <= 5)
  if (length(basinlocs) > 0 && values_in_range == 1) {
    dfs$Mixed[dfs$PID == PIDs[i]] <- 1  # Mark as mixed valence
  } else {
    dfs$Mixed[dfs$PID == PIDs[i]] <- 0  # Mark as not mixed valence
  }
  
  # Classify the participant’s valence pattern based on the number and sign of local minima
  if (length(basinlocs) == 1) {
    if (basinlocs >= 0) {
      type <- "positive monostable"  # One local minimum, positive valence
    } else {
      type <- "negative monostable"  # One local minimum, negative valence
    }
  } else if (length(basinlocs) > 1) {
    if (all(basinlocs >= 0)) {
      type <- "positive multistable"  # Multiple local minima, all positive
    } else if (all(basinlocs <= 0)) {
      type <- "negative multistable"  # Multiple local minima, all negative
    } else if (any(basinlocs > 0) & any(basinlocs < 0)){
      type <- "bistable"  # Mixed positive and negative local minima
    }
  } else {
    type <- "undefined"  # No local minima found
  }
  
  # Store the type of valence breakdown in the dataset
  dfs$Type_breakdown[dfs$PID == PIDs[i]] <- type
  
  # Classify bistability (1 = bistable, 0 = monostable)
  if (any(basinlocs > 0) & any(basinlocs < 0)) {
    type2 <- 1  # Bistable
  } else {
    type2 <- 0  # Monostable
  }
  dfs$Type[dfs$PID == PIDs[i]] <- type2
  
  # Calculate statistics for positive and negative valence (PA and NA)
  if (any(y3 > 0)) {
    dfs$mPA[dfs$PID == PIDs[i]] <- round(mean(y3[y3 > 0]), digits = 3)
    dfs$sdPA[dfs$PID == PIDs[i]] <- round(sd(y3[y3 > 0]), digits = 3)
  } else {
    dfs$mPA[dfs$PID == PIDs[i]] <- NA
    dfs$sdPA[dfs$PID == PIDs[i]] <- NA
  }
  
  if (any(y3 < 0)) {
    dfs$mNA[dfs$PID == PIDs[i]] <- abs(round(mean(y3[y3 < 0]), digits = 3))
    dfs$sdNA[dfs$PID == PIDs[i]] <- round(sd(y3[y3 < 0]), digits = 3)
  } else {
    dfs$mNA[dfs$PID == PIDs[i]] <- NA
    dfs$sdNA[dfs$PID == PIDs[i]] <- NA
  }
  
  # Bistability metrics: Calculate positive-to-negative and negative-to-positive affect shift ratios (P2N_ASR and N2P_ASR)
  p2n <- which(diff(sign(y)) == -2)  # Positive-to-negative shifts
  n2p <- which(diff(sign(y)) == 2)   # Negative-to-positive shifts
  
  if (length(p2n) > 0) {
    dfs$P2N_ASR[dfs$PID == PIDs[i]] <- round(length(p2n) / length(which(y > 0)), digits = 3)
    dfs$mP2N[dfs$PID == PIDs[i]] <- round(mean(y[p2n] + abs(y[p2n + 1])), digits = 3)
    dfs$sdP2N[dfs$PID == PIDs[i]] <- round(sd(y[p2n] + abs(y[p2n + 1])), digits = 3)
  }
  
  if (length(n2p) > 0) {
    dfs$N2P_ASR[dfs$PID == PIDs[i]] <- round(length(n2p) / length(which(y < 0)), digits = 3)
    dfs$mN2P[dfs$PID == PIDs[i]] <- round(mean(abs(y[n2p]) + y[n2p + 1]), digits = 3)
    dfs$sdN2P[dfs$PID == PIDs[i]] <- round(sd(abs(y[n2p]) + y[n2p + 1]), digits = 3)
  }
  
  # Calculate residence times for positive and negative affect (PRT and NRT)
  PAt <- rle(sign(y))$lengths[(rle(sign(y))$values == 1)]  # Positive affect residence time
  NAt <- rle(sign(y))$lengths[(rle(sign(y))$values == -1)] # Negative affect residence time
  
  # Positive residence time statistics
  if (length(PAt) > 0 & !all(is.na(PAt))) {
    dfs$mPRT[dfs$PID == PIDs[i]] <- round(mean(PAt, na.rm = TRUE), digits = 3)
    dfs$sdPRT[dfs$PID == PIDs[i]] <- round(sd(PAt, na.rm = TRUE), digits = 3)
  }
  
  # Negative residence time statistics
  if (length(NAt) > 0 & !all(is.na(NAt))) {
    dfs$mNRT[dfs$PID == PIDs[i]] <- round(mean(NAt, na.rm = TRUE), digits = 3)
    dfs$sdNRT[dfs$PID == PIDs[i]] <- round(sd(NAt, na.rm = TRUE), digits = 3)
  }
  
  # Track participants with no valence shifts
  if (all(df$valence > 0, na.rm = TRUE) || all(df$valence < 0, na.rm = TRUE)) {
    Noshifts <- c(Noshifts, PIDs[i])
  }
  
  # Generate plots if the plot flag is set to TRUE
  if (pl) {
    pdf(paste0("./figures/subjects/PID", PIDs[i], ".pdf"), width = 8, height = 8)
    par(mfrow = c(2, 1), mar = c(5, 3, 3, 3))
    
    # Plot valence over time
    plot(y, type = "l", ylim = c(-50, 50), main = paste0("PID: ", PIDs[i], " Study = ", study), xlab = "", ylab = "")
    
    # Plot histograms and basins
    plot(h, main = paste0("type = ", type), xlim = c(-50, 50), xlab = "")
    par(new = TRUE)
    plot(basins$grid.points, basins$pot, xlim = c(-50, 50), axes = FALSE, xlab = "", ylab = "", type = "l")
    axis(side = 4, at = pretty(range(basins$pot)))
    par(new = TRUE)
    plot(basins$grid.points[basinsind], basins$pot[basinsind], col = 2, xlim  = c(-50, 50), axes = FALSE, xlab = "", ylab = "", ylim = range(basins$pot))
    
    # Close the PDF file for this participant
    dev.off()
  }
}