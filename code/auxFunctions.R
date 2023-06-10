lassomodel <- function(DV, predictors, train){
  data <- cbind(DV,predictors)
  data <- na.omit(data)
  
  if (train){
    training.samples <-
      createDataPartition(data$DV,
                          p=0.9,
                          list=FALSE)
    train.data <- data[training.samples,]
    test.data <- data[-training.samples,]
    
    y <- train.data$DV
    x <- model.matrix(DV~., train.data)[,-1]
  }  else{
    y <- data$DV
    x <- model.matrix(DV~., data)[,-1]
  }
  
  # LASSO (for elastic Net alpha=0.5, for ridge alpha=0)
  cv.lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
  lasso.model <- glmnet(x, y, alpha = 1,
                        lambda = cv.lasso$lambda.min)
  lasso.coefs <- coef(lasso.model)
  
  DV_lasso <- data.frame(
    predictor = row.names(lasso.coefs)[-1],
    coefficient = lasso.coefs[-1])
  DV_lasso <- DV_lasso[DV_lasso$coefficient != 0, ]
  
  if (train){
    # Make prediction on test data
    x.test_lasso <- model.matrix(DV ~., test.data)[,-1]
    predictions_lasso <- as.vector(predict(lasso.model, newx = x.test_lasso))
    
    # Model performance metrics
    RMSE_lasso = RMSE(predictions_lasso, test.data$DV)
    Rsquare_lasso = R2(predictions_lasso, test.data$DV)
    result <- list(DV_lasso=DV_lasso,
                   RMSE_lasso=RMSE_lasso,
                   Rsquare_lasso=Rsquare_lasso)
  } else{
    result <- list(DV_lasso=DV_lasso)
  }
  return(result)
}

regressionmodel <- function(DV, predictor1, predictor2 = NULL){
  
  if (!is.null(predictor2)){
    data <- cbind(DV,predictor1,predictor2)
  } else {
    data <- cbind(DV,predictor1)
  }
  data <- data.frame(na.omit(data))
  
  # Create training control with 10-fold cross-validation
  ctrl <- trainControl(method = "cv", number = 10)
  
  # Fit linear regression with single predictor
  lm1 <- train(DV ~ predictor1, data = data, method = "lm",
               trControl = ctrl)
  
  # Save accuracy metrics
  allRsq1 <- lm1$resample$Rsquared
  Rsq1 <- lm1$results$Rsquared
  RMSE1 <- lm1$results$RMSE
  AIC1 <- AIC(lm1$finalModel)
  BIC1 <- BIC(lm1$finalModel)
  
  if (!is.null(predictor2)){
    # Fit linear regression with two predictor
    lm2 <- train(DV ~ predictor1 + predictor2, data = data,
                 method = "lm", trControl = ctrl)
    
    # Print accuracy metrics
    allRsq2 <- lm2$resample$Rsquared
    Rsq2 <- lm2$results$Rsquared
    RMSE2 <- lm2$results$RMSE
    AIC2 <- AIC(lm2$finalModel)
    BIC2 <- BIC(lm2$finalModel)
    
    # compare the performance of the two models using a likelihood ratio test (LRT)
    lrtest_res <- lrtest(lm1$finalModel, lm2$finalModel)
    pval <- lrtest_res$`Pr(>Chisq)`[2]
    
    return(list(allRsq1=allRsq1,Rsq1=Rsq1,RMSE1=RMSE1,AIC1=AIC1,BIC1=BIC1,
                allRsq2=allRsq2,Rsq2=Rsq2,RMSE2=RMSE2,AIC2=AIC2,BIC2=BIC2,
                pval=pval))
  } else {
    return(list(allRsq1=allRsq1,Rsq1=Rsq1,RMSE1=RMSE1,AIC1=AIC1,BIC1=BIC1))
  }
}

nestedmodels <- function(df, metrics, outcome){
  
  # metrics <- rep(metrics,2)
  
  # Create an empty data frame to store the results
  results_df <- data.frame(result = numeric(length = length(metrics)),
                           metric = metrics)
  
  # Loop over each metric for the current outcome
  for (i in 1:length(metrics)) {
    metric <- metrics[i]
    
    # Apply your function to the current metric and outcome
    result <- regressionmodel(df[[outcome]],df$P2Nr,df[[metric]])
    # result <- round(result$allRsq1,2)
    result <- round(result$pval,3)
    
    # Add the result to the results data frame
    results_df[i,1] <- result
  }
  
  my_theme <- list(fill = pal_jco("default")(2)[1], width = 0.5)
  
  ggbarplot(results_df, x = "metric", y = "result",
            color = pal_jco("default")(2)[1],
            fill = pal_jco("default")(2)[1],
            width = 0.3,
            ggpar = my_theme, position = position_dodge()) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") + 
    ggtitle(outcome) +
    labs(x = "Metrics", y = "p-values")
  
  return(list(plot = last_plot(),results_over=results_df, outcome=outcome))
}

stepwiseRegression <- function(DV, metrics){
  data <- cbind(DV,metrics)
  data <- na.omit(data)
  
  full.model <- lm(DV ~., data = data)
  step.model <- stepAIC(full.model, direction = "both", 
                        trace = FALSE)
  return(step.model)
}

relativeImportance <- function(DV, metrics){
  data <- cbind(DV,metrics)
  data <- na.omit(data)
  
  full.model <- lm(DV ~., data = data)
  rel_imp <- calc.relimp(full.model,
                         type = "lmg", rela = TRUE)
  # bootresults<-boot.relimp(full.model, b=1000)
  # ci<-booteval.relimp(bootresults, norank=T)
  # return(list(rel_imp = rel_imp, bootresults = bootresults,
  #             ci = ci))
  return(rel_imp)
}
