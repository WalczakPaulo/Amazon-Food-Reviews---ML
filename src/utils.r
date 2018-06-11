remove_missing_levels <- function(fit, test_data) {
  test_data %>%
    droplevels() %>%
    as.data.frame() -> test_data
  
  if (any(class(fit) == "glmmPQL")) {
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$contrasts))))
    if (length(factors) == 0) {
      return(test_data)
    }
    
    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>% str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels
    
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$xlevels))))
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }
  
  predictors <- names(test_data[names(test_data) %in% factors])
  
  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      var <- predictors[i]
      test_data[!found, predictors[i]] <- NA
      test_data %>%
        droplevels() -> test_data
    }
  }
  return(test_data)
}

convert <- function(x) {
  if (x > 0.5)
    'positive'
  else
    'negative'
}