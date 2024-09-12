recosinor.fit <- function(formula, data, tau = 24, k = 10, shape=3, scale=2) {

  s_variable <- sub("s\\((\\w+)\\)", "\\1", as.character(formula))

  cbt <- s_variable[2]
  hrs <- gsub(".*time\\((\\w+)\\).*", "\\1", s_variable[3])
  sw <- gsub(".*sleep\\((\\w+)\\).*", "\\1", s_variable[3])
  

  if (hrs == s_variable[3]) { # if this true mean no sleep input
    cat("Please input time as time(hrs)")
  } else{
    # recosinor functions
    x_terms <- recosinor_func2(data[[hrs]], tau, k)
    data$ysin <-  x_terms[["y_sin"]]
    data$ycos <-  x_terms[["y_cos"]]
  }
  
  formula_out <<- formula



  if (sw == s_variable[3]) { # if this true mean no sleep input
    sw <- NULL
    cat("Analysing without sleep-wake effects")
  } else{
    data <- sleep_function(data, shape, scale)
  }
  
  
  
  
  ## Overall prediction model
  # predictors <- setdiff(names(data), c("cbt", "hrs","sw",
  #                                      "ysin"   ,       "ycos", "datetime" ) )
  #predictors <- c("y_base")
  # formula <- as.formula(paste("cbt ~", paste(predictors, collapse = " + ")))
  # gam_model <- gam(formula, data = df_fit)
  
  id_cols <- grep("^y_sleep_|^y_wake_", colnames(data))
  

  if (!is.null(sw)) {
    predictors_string <-  paste(colnames(data)[id_cols], collapse = " + ")
    
    
    new_formula <- update(formula, . ~ . - time(hrs) - sleep(sw)
                          + ysin + ycos )

    new_formula <- update(new_formula, as.formula(paste(". ~ . +", predictors_string)))

  } else {
    new_formula <- update(formula, . ~ . - time(hrs) - sleep(sw)
                          + ysin + ycos)
  }
  
  



  model_recosinor <- bam(new_formula, #
                         data = data) # model fit
  
  print(model_recosinor)
  
  data_output <<- data


  return(model_recosinor)

}
