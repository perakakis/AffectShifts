## Code and Data for the article "Bistability and Affect Shift Dynamics in the Prediction of Psychological Well-Being"

### Citation

Goicoechea, C., Dakos, V., Sanabria, D., Heshmati, S., Westhoff, M., Banos, O., Pomares, H., Hofmann, S. G., & Perakakis, P. (2024). Bistability and affect shift dynamics in the prediction of psychological well-being. Emotion, 25(4), 982–996. https://dx.doi.org/10.1037/emo0001454

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15001651.svg)](https://doi.org/10.5281/zenodo.15001651)

### Repository Structure

```
Data/               # EMA and survey data for Spanish and German samples
Code/
  01_preprocess.R   # Data cleaning and preparation
  02_extract.R      # Bistability metrics extraction (basins, affect shifts, residence times)
  05_PCA.R          # Principal Component Analysis
  06_corrplot.R     # Correlation plots
  07_lasso.R        # LASSO regression
  08_stepwiseRegression.R
  09_relativeImportance.R
  auxFunctions.R    # Helper functions (LASSO, regression, nested models)
  predictor_colors.R
```

### Reproducibility

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies. To reproduce the analysis environment:

```r
# install.packages("renv")  # if not already installed
renv::restore()
```

This will install all required packages at the exact versions recorded in `renv.lock` (R 4.5.2).

Scripts are intended to be run from the project root directory.
