
data("cbt_data")

head(cbt_data)


model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR),
                           cbt_data,
                           tau = c(24))

summary(model)


recosinor.plot(model, cbt_data)



## Optimise period, circadian shape and homeostatic shape:
recosinor_obj <- function(params) {

  period <- params[1] # this control tau
  k <- params[2] # control the shape of cosine function(skew vs symetric)
  shape <- params[3] # control the shape of sleep drive function

  model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR),
                         cbt_data,
                         tau = c(period),
                         k = k,
                         shape = shape)



  # Save the best model
  assign("best_model", model, envir = .GlobalEnv)

  return(sum(residuals(model)^2))
}

library(optimx)
# Use the optimx function
recosinor_opt <- optimx(c(24,10,3),
                        recosinor_obj,
                        method=c("L-BFGS-B","bobyqa"),
                        control=list(
                          save.failures=TRUE,
                          trace=1),
                        itnmax = 100,
                        lower = c(23, -100,1),
                        upper = c(25, 100, 5) )

params<- as.matrix(summary(recosinor_opt, order = value)[1, 1:3])
recosinor_obj(params) # get the best model parameters

summary(best_model)

recosinor.plot(best_model, cbt_data)


