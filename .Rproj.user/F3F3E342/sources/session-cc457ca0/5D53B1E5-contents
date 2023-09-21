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


  if (sw == s_variable[3]) { # if this true mean no sleep input
    sw <- NULL
    cat("Analysing without sleep-wake effects")
  } else{
    data$sleep_response <- sleep_function(data, shape, scale)
  }

  if (!is.null(sw)) {
    new_formula <- update(formula, . ~ . - time(hrs) - sleep(sw)
                          + ysin + ycos + sleep_response)

  } else {
    new_formula <- update(formula, . ~ . - time(hrs) - sleep(sw)
                          + ysin + ycos)
  }




  model_recosinor <- bam(new_formula, #
                         data = data,
                         family = gaussian() ) # model fit


  return(model_recosinor)

}
