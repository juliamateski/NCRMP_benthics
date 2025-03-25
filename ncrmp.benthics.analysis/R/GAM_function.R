GAM_function <- function(data, response, group_var = NULL, group_names = NULL, kvalue = 3){

  library(mgcv)


  #!!sym() allows us to generalize this function
  #it calls it as a column name



  if (!is.null(group_var)) {
  data <- data %>%
    filter(!!sym(group_var) %in% group_names) %>%
    mutate(
      !!sym(response) := as.numeric(!!sym(response)),
      SE = as.numeric(SE)
    )
  }

  #build and fit the model
  formula <- as.formula(paste(response, "~ s(YEAR, k = ", kvalue, ")"))
  model <- gam(formula, weights = 1/(SE^2), data = data)

  #Adjusted r squared
  adj_r <- summary(model)$r.sq

  #p-value for the smooth term
  p_val <- summary(model)$s.table[1, "p-value"]

  #R-squared and p-value
  result <- list(adj_r_squared = adj_r, p_value = p_val)

  # Statistical significance check
  if (p_val < 0.05) {
    result$significance <- "Relationship is significant"
  } else {
    result$significance <- "Relationship is not significant"
  }

  # Shapiro-Wilk Test (Normality Check)
  shap_wilk_test <- shapiro.test(residuals(model))

  result$shapiro_wilk <- shap_wilk_test$p.value

  # Kolmogorov-Smirnov Test (Normality Check)
  kol_smir_test <- ks.test(residuals(model), "pnorm", mean = mean(residuals(model)), sd = sd(residuals(model)))
  result$kol_smir_test <- kol_smir_test$p.value

  # Residual Diagnostics Plot
  resid_hist <- hist(residuals(model), main = "Histogram of Residuals", xlab = "Residuals")

  # Q-Q Plot for Residuals
  qqnorm(residuals(model))

  qqline(residuals(model))

  # Homoscedasticity Check (Constant Variance)
  plot(model, residuals = TRUE, pch = 20, col = "blue")
  abline(h = 0, lty = 2, col = "red")

  # GAM Check
  gam_check_results <- gam.check(model)

  # Concurvity Check
  concurv <- concurvity(b = model, full = TRUE)
  result$concurvity <- concurv



  # Return results
  return(list(model_summary = result, gam_check = gam_check_results))
}

