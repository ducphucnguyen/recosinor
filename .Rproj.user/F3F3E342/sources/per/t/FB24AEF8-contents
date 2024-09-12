## List of circadian model functions




####================== Van Der Pol model of endogenous circadian
van_der_pol_response <- function(df, opt_pars) {

  # Optimisable 3 paramters
  phase <- opt_pars[1]
  params <- c(mu = opt_pars[2], tau = opt_pars[3])


  # differiental equation
  van_der_pol_model <- function(t, x, params) {
    mu <- params["mu"]
    tau <- params["tau"]

    # defaul si = 1
    si <- 1

    dxdt <- c(x[2],
              mu * (2 * pi / tau) * (1 - (4 / si^2) * x[1]^2) * x[2]
              - (2 * pi / tau)^2 * x[1])

    list(dxdt)
  }


  # input including a datetime and cbt
  df$hrs <- as.numeric(difftime(df$Datetime, df$Datetime[1], units = "hours") )

  times <- seq(from = 0, to = max(df$hrs) + phase, by = diff(df$hrs)[1])
  out <- ode(y = c(1, 0), times = times, func = van_der_pol_model, parms = params)

  idx = (length(out[, "1"]) - nrow(df)+1) : length(out[, "1"])

  df$cir_response <- out[, "1"][idx]

  #return(root_mean_square_error)
  return( df )

}

#df <- van_der_pol_response(df, c(0, 0.2, 24.2))







#######================================== Sleep wake effect

homonstatic_response <- function(opt_pars) {

  # optimisable parameters
  shape1 <- opt_pars[1]
  scale1 <- opt_pars[2]
  shape2 <- opt_pars[3]
  scale2 <- opt_pars[4]



  ## Gamma function
  gamma_model <- function(time, shape,scale) {
    dgamma(time, shape, scale)
  }

  ## Sleep effect
  df$s_time <- 0
  indexs <- df$Datetime>=sw_datetime$from & df$Datetime<=sw_datetime$to
  s_time <- as.numeric(difftime(df$Datetime, sw_datetime$from, units = "hours") )
  df$s_time[indexs] <- s_time[indexs]

  df$w_time <- 0
  indexw <- df$Datetime>=sw_datetime$to
  w_time <- as.numeric(difftime(df$Datetime, sw_datetime$to, units = "hours") )
  df$w_time[indexw] <- w_time[indexw]



  df <- df %>%
    mutate(
      s_response = gamma_model(df$s_time, shape1, scale1),
      w_response = gamma_model(df$w_time, shape2, scale2)
    )

  df$s_response[!indexs] <- 0
  df$w_response[!indexw] <- 0

  return( df )

}



#df<- homonstatic_response(df,c(3,3,3,3))



sleep_function <- function(data, shape=3, scale=2) {

  # Find the indices where the binary column switches from 0 to 1
  sleep_onset <- which(diff(data$sw) == 1)

  # Find the indices where the binary column switches from 1 to 0
  sleep_offset <- which(diff(data$sw) == -1)


  s_time <- data$hrs[sleep_onset:sleep_offset] - data$hrs[sleep_onset]

  data$s_response <- 0 # initialise s_response

  data$s_response[sleep_onset:sleep_offset] <- dgamma(s_time, shape, scale)


  return( data$s_response )

}




#### Recursive Cosinor function
## Test circadian function
recosinor_func <- function(x, a1, a2,a3, k) {
  # x: hrs from recording
  # period: 23.8 to 25
  # k ~ -30 to 30

  x_samples <- seq(min(x), max(x), length.out=3)

  periodx <- c(a1, a2, a3)
  #freq_samples <- 1/periodx

  interpolation <- splinefun(x_samples, periodx)
  period <- interpolation(x)

  dx <- rep( 30/60/60, length(x) )
  x <- 2*pi*cumsum( (1/period) * dx )

  #x <- 2 * pi * x/ period
  n <- length(x)
  y_sin <- rep(0, n)
  y_cos <- rep(0, n)

  for (i in 2:n) {
    y_sin[i] <- sin(x[i] + y_sin[i-1]/k)

    y_cos[i] <- cos(x[i] + y_cos[i-1]/k)
  }

  return(list(y_sin = y_sin, y_cos = y_cos))
}






recosinor_obj_pred <- function(params) {



  shape1 <- params[5]
  scale1 <- params[6]
  shape2 <- params[7]
  scale2 <- params[8]


  df1 <- homonstatic_response(c(shape1, scale1, shape2, scale2))


  model_recosinor <- bam(cbt ~ cir_response + s_response + w_response
                         + s(MovingAvgHR), #
                         data = df1,
                         family = gaussian() ) # model fit

  # Save the best gam_model
  assign("best_recosinor_model", model_recosinor, envir = .GlobalEnv)

  return(sum(residuals(model_recosinor)^2))
}





recosinor_func2 <- function(x, n_f=24, k=10) {
  # x: hrs from recording
  # n_knot: define the number of frequency
  # period: 23.8 to 25
  # k ~ -30 to 30
  x_samples <- seq(min(x), max(x), length.out= length(n_f) )

  # <- c(a1, a2, a3)
  freq_samples <- n_f/24

  interpolation <- splinefun(x_samples, freq_samples)
  freq <- interpolation(x)

  dx <- rep( 30/60/60, length(x) )
  x <- 2*pi*cumsum( freq * dx )/24

  #x <- 2 * pi * x/ period
  n <- length(x)
  y_sin <- rep(0, n)
  y_cos <- rep(0, n)

  for (i in 2:n) {
    y_sin[i] <- sin(x[i] + y_sin[i-1]/k)

    y_cos[i] <- cos(x[i] + y_cos[i-1]/k)
  }

  #data$ysin <- y_sin
  #data$ycos <- y_cos
  return(list(y_sin = y_sin, y_cos = y_cos))
}



homonstatic_response2 <- function(opt_pars) {

  # optimisable parameters
  shape1 <- opt_pars[1]
  scale1 <- opt_pars[2]
  shape2 <- opt_pars[3]
  scale2 <- opt_pars[4]



  ## Gamma function
  gamma_model <- function(time, shape,scale) {
    dgamma(time, shape, scale)
  }

  ## Sleep effect
  df$s_time <- 0
  df$w_time <- 0
  ww <- c()
  ss <- c()

  for (i in 1:nrow(light_io.i) ) {

    indexs <- df$time>=light_io.i$lightout[i] & df$time<=light_io.i$lighton[i]
    s_time <- as.numeric(difftime(df$time, light_io.i$lightout[i], units = "hours") )
    df$s_time[indexs] <- s_time[indexs]
    ss <- c(ss, indexs)


    if (i<nrow(light_io.i)) {
      indexw <- df$time>=light_io.i$lighton[i]  & df$time<=light_io.i$lightout[i+1]
    }else{
      indexw <- df$time>=light_io.i$lighton[i]
    }

    w_time <- as.numeric(difftime(df$time, light_io.i$lighton[i], units = "hours") )
    df$w_time[indexw] <- w_time[indexw]

    ww <- c(ww, indexw) # save index

  }


  df <- df %>%
    mutate(
      s_response = gamma_model(df$s_time, shape1, scale1),
      w_response = gamma_model(df$w_time, shape2, scale2)
    )


  index_wake <- df$s_time == 0
  index_sleep <- df$s_time != 0

  df$s_response[!index_sleep] <- 0
  df$w_response[!index_wake] <- 0

  df_out <- df %>% select(time, s_response, w_response)

  return( df_out )

}







recosinor <- function(formula, data, tau = 24, k = 10, shape=3, scale=2) {

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






