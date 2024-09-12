recosinor.plot <- function(model, data) {


  library(pracma)
  #library(ggpubr)
  
  cbt_data <- data


  cbt_data$cbt_pred <- predict(model, data)

  r2<-summary(model)[["r.sq"]]

  # Measure vs fitted plot
  p1 <- ggplot(cbt_data) +
    geom_line(aes(x=datetime, y=cbt, color="Measured")) +
    geom_line(aes(x=datetime, y=cbt_pred, color="Fitted Recosinor [1]")) +
    labs(x= "",
         y = "CBT, oC") +

    scale_color_manual(values = c("Measured" = "#636363", "Fitted Recosinor [1]" = "#e6550d")) +
    theme(legend.title = element_blank()) +  # Remove the legend title

    geom_text(aes(x = datetime[1], y = max(cbt,na.rm = TRUE), label = paste("R-squared =", 
                                                                  round(r2, 2))),
              color = "black", hjust = 0, vjust = 1, size = 4)

  p1




  # circadian pattern
  individual_contributions <- predict(model,data, type = "terms")
  individual_contributions <- as.data.frame(individual_contributions)

  cbt_data$c_process <- individual_contributions$ysin + individual_contributions$ycos
  
  id_cols <- grep("^y_sleep_", colnames(individual_contributions))
  
  cbt_data$s_process  <- rowSums(individual_contributions[, id_cols])


  ## Tmin
  result  = pracma::findpeaks(-as.numeric( cbt_data$c_process )) # troughs
  pks1    = result[,1]
  locs1   = result[,2]

  TROUGH <- data.frame(Datetime_loc = cbt_data$datetime[locs1], Peak_val = -pks1)



  # calcualte min for S process

  result  = pracma::findpeaks(-cbt_data$s_process ) # troughs
  pks1    = result[,1]
  locs1   = result[,2]

  TROUGH3 <- data.frame(Datetime_loc = cbt_data$datetime[locs1], Peak_val = -pks1)



  #
  p2<- ggplot(cbt_data, aes(x = datetime, y = cbt)) +

    geom_path(aes(x = datetime, y = c_process, color = "Endogenous circadian")) +
    geom_path(aes(x = datetime, y = s_process, color = "Homeostatic sleep")) +
    scale_color_manual(values = c("Endogenous circadian" = "#756bb1", "Homeostatic sleep" = "#31a354")) +
    labs(x = "", y = "cbt") +

    theme(legend.title = element_blank()) +  # Remove the legend title


    geom_point( data=TROUGH, aes(x=Datetime_loc, y=Peak_val ),
                colour = "#fdae6b",
                fill = "#e6550d",
                size=2.5,
                shape=21) +

    geom_text(data=TROUGH, aes(x=Datetime_loc, y=Peak_val,
                               label = paste(round(Peak_val,2),"oC",
                                             format(Datetime_loc, "%H:%M:%S"),sep = " | ") ),
              angle = 90,
              size = 3,
              hjust = -0.1 ) +


    geom_text(data=TROUGH3, aes(x=Datetime_loc, y=Peak_val,
                                label = paste(round(Peak_val,2),"oC",
                                              format(Datetime_loc, "%H:%M:%S"),sep = " | ") ),
              angle = 90,
              size = 3,
              hjust = -0.1 ) +


    labs(x="",
         y="CBT, oC")

  p2


  pp<- ggarrange(p1, p2,
                 labels = c(" ", " ", " "),
                 align = c("v"),
                 heights = c(0.5, 0.5),
                 ncol = 1, nrow = 2) # size 7-6.5 p

  return(pp)

}

