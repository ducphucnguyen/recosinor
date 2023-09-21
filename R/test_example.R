
data("cbt_data")




model <- recosinor.fit(cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR),
                           cbt_data,
                           tau = c(24))

summary(model)


recosinor.plot(model, cbt_data)


