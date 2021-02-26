##  The present function runs the forecast methodology,
##  then selects the best method and use it to forecast
## t periods ahead

#Requirements
library(fpp2)


#Find the best method and returns an object of the class "forecast"
decideBestMethod <- function(sdt,tiempo=1){
    trainingData <- window(sdt) ##de dónde a dónde
    
    #Forecasting methods
    naiveMethod <- naive(trainingData, h = tiempo) # Naive 
    RMSE1 <- sqrt(mean(naiveMethod$residuals^2,na.rm = TRUE))
    MinRMSE <- RMSE1
    selectedMethod <- naiveMethod
    
    seasonalNaiveMethod <- snaive(trainingData, h = tiempo) # Estacional
    RMSE2 <- sqrt(mean(seasonalNaiveMethod$residuals^2,na.rm = TRUE))
    if(RMSE2<MinRMSE){
      selectedMethod <- seasonalNaiveMethod
      MinRMSE <- RMSE2
    }
    
    sesM <-ses(trainingData, initial = c("simple"),h= tiempo)
    RMSE3 <- sqrt(mean(sesM$residuals^2,na.rm = TRUE))
    if(RMSE3<MinRMSE){
      selectedMethod <- sesM
      MinRMSE <- RMSE3
    }
    
    holtF <- holt(trainingData,h=tiempo)
    RMSE4 <- sqrt(mean(holtF$residuals^2,na.rm = TRUE))
    if(RMSE4<MinRMSE){
      selectedMethod <- holtF
      MinRMSE <- RMSE4
    }
    
    hwAdditive <- hw(trainingData, seasonal = "additive", h=tiempo)
    RMSE5 <- sqrt(mean(hwAdditive$residuals^2,na.rm = TRUE))
    if(RMSE5<MinRMSE){
      selectedMethod <- hwAdditive
      MinRMSE <- RMSE5
    }
    
    hwMult <- hw(trainingData, seasonal = "multiplicative", h=tiempo)
    RMSE6 <- sqrt(mean(hwMult$residuals^2,na.rm = TRUE))
    if(RMSE6<MinRMSE){
      selectedMethod <- hwMult
      MinRMSE <- RMSE6
    }
    
    
    #returns selected method
    return(selectedMethod)
}

#Gives the forecast for the next t periods
forecastMethod <- function(sdt, t=1){
  ForecastMth <- decideBestMethod(sdt, t)
  pronostico <- ForecastMth$mean #Regresa una serie de tiempo con los valores
  return(pronostico)
}






