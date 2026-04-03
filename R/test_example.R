
source("E:/Phuc Nguyen/dev/cirbug/recosinor-main/recosinor-main/R/recosinor.fit.R")
source("E:/Phuc Nguyen/dev/cirbug/recosinor-main/recosinor-main/R/cir_funs.R")
source("E:/Phuc Nguyen/dev/cirbug/recosinor-main/recosinor-main/R/recosinor.plot.R")

library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(lubridate)
library(imputeTS)
library(zoo)
library(mgcv)


cbt <- read_csv("data/P007/cbt.csv")
df_empatica <- read_csv("data/P007/df_empatica.csv")
cbt$time <- as.POSIXct(cbt$time, format="%d/%m/%Y %I:%M:%S %p", tz="ACST")
df_empatica$Datetime <- as.POSIXct(df_empatica$Datetime, format="%d-%b-%Y %H:%M:%S") + hours(9) +minutes(30)
colnames(cbt)[1] <- c("Datetime")

df_cbt <- left_join(cbt,df_empatica,by=c("Datetime"))

df_cbt <- df_cbt[490:11366,]

head(df_cbt)

df_cbt$PulseRate    <- na_interpolation(df_cbt$PulseRate)
df_cbt$AccelStdG    <- na_interpolation(df_cbt$AccelStdG)
df_cbt$TemperatureC    <- na_interpolation(df_cbt$TemperatureC)

df_cbt$PulseRate <- rollmean(df_cbt$PulseRate, k = 25, fill = NA, align = "center")
df_cbt$AccelStdG <- rollmean(df_cbt$AccelStdG, k = 25, fill = NA, align = "center")
df_cbt$TemperatureC <- rollmean(df_cbt$TemperatureC, k = 5, fill = NA, align = "center")


# Panel 1: Core Body Temperature (meanCBT + TemperatureC together)
p1 <- ggplot(df_cbt, aes(x = Datetime)) +
  geom_line(aes(y = meanCBT, color = "meanCBT"), na.rm = TRUE) +
  #geom_line(aes(y = TemperatureC, color = "SkinTemp"), na.rm = TRUE) +
  labs(y = "Temperature (°C)", x = "") +
  scale_color_manual(values = c("meanCBT" = "red", "SkinTemp" = "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Panel 2: Heart rate
p2 <- ggplot(df_cbt, aes(x = Datetime, y = PulseRate)) +
  geom_line(na.rm = TRUE, color = "blue") +
  labs(y = "Pulse Rate (bpm)", x = "") +
  theme_minimal()

# Panel 3: Activity
p3 <- ggplot(df_cbt, aes(x = Datetime, y = AccelStdG)) +
  geom_line(na.rm = TRUE, color = "green") +
  labs(y = "AccelStdG (g)", x = "Datetime") +
  theme_minimal()

# Arrange in 3 panels (stacked vertically)
p1 / p2 / p3


df_cbt$hrs <- as.numeric(difftime(df_cbt$Datetime, df_cbt$Datetime[1], units = "hours"))

# Create sleep/wake vector (1 = sleep between 22:00–23:59 and 00:00–06:00, else 0)
hrs <- hour(df_cbt$Datetime)

df_cbt$sw <- ifelse(hrs >= 22 | hrs < 6, 1, 0)

df <- df_cbt %>% select( meanCBT, hrs, sw, PulseRate,AccelStdG, TemperatureC)

colnames(df) <- c("cbt", "hrs","sw", "MovingAvgHR", "acc","tempC")

data("cbt_data")

head(cbt_data)

cbt_data <- df

model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR) + s(tempC) +s(acc),
                       cbt_data,
                           tau = c(24))

summary(model)


# Define start datetime
start_time <- df_cbt$Datetime[1]

# Create new datetime column
data_output$datetime <- start_time + data_output$hrs * 3600  # 3600 seconds per hour

recosinor.plot(model, data_output)




## Optimise period, circadian shape and homeostatic shape:
recosinor_obj <- function(params) {

  period <- params[1] # this control tau
  k <- params[2] # control the shape of cosine function(skew vs symetric)
  shape <- params[3] # control the shape of sleep drive function

  model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR) + s(acc) + s(tempC),
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

# Define start datetime
start_time <- df_cbt$Datetime[1]

# Create new datetime column
data_output$datetime <- start_time + data_output$hrs * 3600  # 3600 seconds per hour

recosinor.plot(best_model, data_output)


