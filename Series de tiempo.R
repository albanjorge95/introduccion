#======================================================================
#=======================SERIES DE TIEMPO===============================
#======================================================================

# Instalación de librerías
if(!require(fpp3)) install.packages("fpp3", dependencies = TRUE)
if(!require(clipr)) install.packages("clipr", dependencies = TRUE)
if(!require(prophet)) install.packages("prophet", dependencies = TRUE)
if(!require(bsts)) install.packages("bsts", dependencies = TRUE)
if(!require(Metrics)) install.packages("Metrics", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", dependencies = TRUE)
if(!require(tseries)) install.packages("tseries", dependencies = TRUE)
if(!require(forecast)) install.packages("forecast", dependencies = TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", dependencies = TRUE)

# Librerías necesarias
library(fpp3)
library(clipr)
library(prophet)
library(bsts)
library(dplyr)
library(Metrics)
library(ggplot2)
library(gridExtra)
library(tseries)
library(forecast)
library(ggplot2)
library(gridExtra)
library(readxl)


#Decimales a 3 digitos, warn suprime advertencias
  options(digits = 3, warn = -1)  

# Cargar datos desde el portapapeles
  #Ojo debe estar en formato Anio mes y dia separado por guion (-)
  #Las cifras deben estar en valores (no porcentajes)
  #Cabeceras sin combirnar, nombres sin espacios y cortos
  data <- read.table(file = 'clipboard', header = TRUE, dec = ',')
data <- read_xlsx("DATOS.xlsx")
# Cargar datos desde el portapapeles
  data[,1] <- as.Date(data[,1], format = '%Y-%m-%d')
  colnames(data) <- c("Fecha", "Variable")

# Definir parametros de la serie de tiempo
  tipo_serie <- 12
    #12 para mensual, 365 para diaria, 4 para trimestral, etc.
  anio_inicio <- 2023
  mes_inicio <- 8
  anio_final <- 2025
  mes_final <- 8

# Define x como variable de serie de tiempo   #ts times series
  x <- ts(data$MORA,
          frequency = tipo_serie, 
          start = c(anio_inicio, mes_inicio), 
          end = c(anio_final, mes_final))

# Gráfico de la serie temporal
  ggplot(data, aes(x = FECHA, y = MORA)) +
    geom_line(color = "green", size = 1) +
    labs(title = "Serie Temporal", x = "Tiempo", y = "Valor de la Serie") +
    theme_minimal()

# Tiempo de predicción #####los 3 primeras predicciones ajustadas###
  tiempo_prediccion <- 4
  observed <- tail(data$MORA, tiempo_prediccion)

#=============Suavizamiento exponencial Simple===============================
  
  #Modelamiento
  se_s <- ses(x, h = tiempo_prediccion)
  summary(se_s)
  
  #Pronosticos
  f_se_s <- forecast(se_s, h = tiempo_prediccion)

  #Exporta resultaros
  write_clip(data.frame(f_se_s))

  # Últimos valores observados (sirve para comparar todos los modelos)
  observed <- tail(data$MORA, tiempo_prediccion)  

  #Valor pronosticados
  p_se_s <- f_se_s$mean
  
  #Metricas de error
  RMSE_se_s<-rmse(observed, p_se_s)
  MAE_se_s<-mae(observed, p_se_s)
  MAPE_se_s<-mape(observed,p_se_s)
  MASE_se_s<-mase(observed, p_se_s)
  #Consolido
  ajuste_se_s<-as.data.frame(rbind(RMSE_se_s,MAE_se_s,MAPE_se_s,MASE_se_s))
  colnames(ajuste_se_s)<-c("se_s") #renombro columna
  print(ajuste_se_s)

  ##Gráficos
  par(mfrow=c(1,1)) #1 fila y 1 columna
  plot(f_se_s, include=50)
  
#=============Suavizamiento exponencial Doble===============================
  
  #Modelamiento
  se_d <- holt(x, h = tiempo_prediccion)
  summary(se_d)
  
  #Pronosticos
  f_se_d <- forecast(se_d, h = tiempo_prediccion)

  #Exporta resultaros
  write_clip(data.frame(f_se_d))
  
  #Valor pronosticados
  p_se_d <- f_se_d$mean
  
  #Metricas de error
  ajuste_se_d<- as.data.frame(
    rbind(rmse(observed, p_se_d), #rbind unir por fila (row)
    mae(observed, p_se_d),
    mape(observed,p_se_d),
    mase(observed, p_se_d)))
  colnames(ajuste_se_d)<-c("se_d")
  print(ajuste_se_d)

  ##Gráficos
  par(mfrow=c(1,2)) #1 fila y 2 columna
  plot(f_se_s, include=50) #Grafico se_s
  plot(f_se_d, include=50) #Grafico se_d
    
#=============Suavizamiento exponencial Triple===============================
  
  #Modelamiento
  se_t <- HoltWinters(x, alpha = NULL, beta=NULL, gamma = TRUE) ####SOLO SI DA ERROR FALSE 
  # Ver los parámetros ajustados del modelo
  se_t$alpha  # Parámetro alpha
  se_t$beta   # Parámetro beta
  se_t$gamma  # Parámetro gamma
  
  #Pronosticos
  f_se_t <- forecast(se_t, h = tiempo_prediccion)
  
  #Exporta resultaros
  write_clip(data.frame(f_se_t))
  
  #Valor pronosticados
  p_se_t <- f_se_t$mean
  
  #Metricas de error ######AQUÍ CONSOLIDAS LOS ERRORES 
  ajuste_se_t<- as.data.frame(
    rbind(rmse(observed, p_se_t), #rbind unir por fila (row)
          mae(observed, p_se_t),
          mape(observed,p_se_t),
          mase(observed, p_se_t)))
  colnames(ajuste_se_t)<-c("se_t")
  print(ajuste_se_t)
#RBIND filas 
#robind columnas 
  
  ##Gráficos
  par(mfrow=c(1,3)) #1 filas y 3 columnas
  plot(f_se_s, include=50) #Grafico se_s
  plot(f_se_d, include=50) #Grafico se_d
  plot(f_se_t, include=50) #Grafico se_t
  
#=======================SARIMA===============================
  
#Modelamiento
  arima <- auto.arima(x, seasonal = TRUE)
  summary(arima)

#Pronosticos
  f_arima <- forecast(arima, h = tiempo_prediccion)
  
#Exporta resultaros
  write_clip(data.frame(f_arima))
  
#Valor pronosticados
  p_arima <- f_arima$mean

#Metricas de error
  ajuste_arima<- as.data.frame(
    rbind(rmse(observed, p_arima), #rbind unir por fila (row)
          mae(observed, p_arima),
          mape(observed,p_arima),
          mase(observed, p_arima)))
  colnames(ajuste_arima)<-c("arima")
  #Exporto a portapapeles
  write_clip(ajuste_arima)

  ##Gráficos
  par(mfrow=c(2,2)) #2 filas y 2 columna
  plot(f_se_s, include=25) #Grafico se_s
  plot(f_se_d, include=25) #Grafico se_d
  plot(f_se_t, include=25) #Grafico se_t
  plot(f_arima, include=25) #Grafico arima
  
#=====================Prophet==================================
  
#Modelamiento
  prophet_data <- data.frame(ds = data$FECHA, 
                             y = data$CARTERA_TOTAL)
  m_prophet <- prophet(prophet_data) ###y_HAT ES EL QUE ME SIRVE

  print(m_prophet)   #####Difícil de interpretar# 
  
  
#Pronosticos #####make_future_dataframe es el que se USA PARA PROPET
  future <- make_future_dataframe(m_prophet, 
            periods = tiempo_prediccion, freq = "month")
  f_prophet <- predict(m_prophet, future)
  
  r_prophet <- list(pred = tail(f_prophet$yhat, tiempo_prediccion),
                    LS = tail(f_prophet$yhat_upper),
                    LI = tail(f_prophet$yhat_lower))
  
#Exporta resultaros
  write_clip(data.frame(r_prophet))
  
#Valor pronosticados
  p_prophet <- f_prophet$yhat
  
#Metricas de error
  ajuste_prophet<- as.data.frame(
    rbind(rmse(observed, p_prophet), #rbind unir por fila (row)
          mae(observed, p_prophet),
          mape(observed,p_prophet),
          mase(observed, p_prophet)))
  colnames(ajuste_prophet)<-c("Prophet")
  print(ajuste_prophet)
  
# Graficar el ajuste del modelo y las predicciones
  
  
  
  plot(m_prophet, f_prophet)

  
# Visualizar los componentes del modelo (tendencia, estacionalidad)
  prophet_plot_components(m_prophet, f_prophet)
  
#Gráficos
  par(mfrow=c(1,1)) 
  plot(p_prophet, include=25) #Grafico prophet  
  
  
  
  
  
  
  ######################PROPHET AJUSTADO########################333
  
  
  library(prophet)
  library(Metrics)   # para rmse, mae, mape, mase
  library(clipr)     # para write_clip
  
  # Prepara los datos
  prophet_data <- data.frame(ds = data$FECHA, 
                             y = data$MORA)
  m_prophet <- prophet(prophet_data)
  
  # Crear fechas futuras
  future <- make_future_dataframe(m_prophet, 
                                  periods = tiempo_prediccion, 
                                  freq = "month")  # asegúrate que el tipo de fecha es mensual
  
  f_prophet <- predict(m_prophet, future)
  
  # Resultados pronosticados
  r_prophet <- list(pred = tail(f_prophet$yhat, tiempo_prediccion),
                    LS = tail(f_prophet$yhat_upper, tiempo_prediccion),
                    LI = tail(f_prophet$yhat_lower, tiempo_prediccion))
  
  write_clip(data.frame(r_prophet))
  
  # Valores pronosticados
  p_prophet <- tail(f_prophet$yhat, tiempo_prediccion)
  
  # Observados reales (debes ajustar esto según tus datos)
  observed <- tail(data$CARTERA_TOTAL, tiempo_prediccion)
  
  # Métricas de error
  ajuste_prophet <- as.data.frame(
    rbind(rmse(observed, p_prophet),
          mae(observed, p_prophet),
          mape(observed, p_prophet),
          mase(observed, p_prophet)))
  colnames(ajuste_prophet) <- c("Prophet")
  print(ajuste_prophet)
  
  # Gráfico del modelo
  plot(m_prophet, f_prophet)
  
#=============Bayesian Structural Time Series (BSTS)==================================
  
#Modelamiento
  ss <- AddLocalLinearTrend(list(), x)
  bsts_model <- bsts(x, state.specification = ss, niter = 1000)
  summary(bsts_model)
  
#Pronosticos
  f_bsts <- predict(bsts_model, horizon = tiempo_prediccion)
  r_bsts <- list(pred = f_bsts$mean,
                           LS = f_bsts$interval[2,],
                           LI = f_bsts$interval[1,])
    #Exporta resultaros
  write_clip(data.frame(r_bsts))
  
  #Valor pronosticados
  p_bsts <- f_bsts$mean
  
  #Metricas de error
  ajuste_bsts<- as.data.frame(
    rbind(rmse(observed, p_bsts), #rbind unir por fila (row)
          mae(observed, p_bsts),
          mape(observed,p_bsts),
          mase(observed, p_bsts)))
  colnames(ajuste_bsts)<-c("bsts")
  print(ajuste_bsts)

  #Gráficos
  par(mfrow=c(1,2)) 
  plot(p_prophet, include=25) #Grafico prophet  
  plot(f_bsts, include=25)#Grafico Bsts 
  
#=============Locally Estimated Scatterplot Smoothing (LOESS))==================================

#Modelamiento
  loess_model <- tslm(x ~ trend + season)
  summary(loess_model)  

#Pronosticos
  f_loess <- forecast(loess_model, h = tiempo_prediccion)
  #Exporta resultaros
  write_clip(data.frame(f_loess))
  
  #Valor pronosticados
  p_loess <- f_loess$mean
  
  #Metricas de error
  ajuste_loess<- as.data.frame(
    rbind(rmse(observed, p_loess), #rbind unir por fila (row)
          mae(observed, p_loess),
          mape(observed,p_loess),
          mase(observed, p_loess)))
  colnames(ajuste_loess)<-c("loess")
  print(ajuste_loess)
    
  #Gráficos
  par(mfrow=c(1,1)) 
  plot(f_loess, include=25) #Grafico Loess
  

#==========Red Neuronal tipo Feed Forward Neural (NAR)==================================
  
#Modelamiento
  #Para de modelo arima
  summary(arima) #Tengo un ma(1) y ma(2)
  #Fijo semilla
  set.seed(786)
  #Realizo red neuroal con parametros arima
  #p para AR, q para MA
  rn <- nnetar(x,p=2,P=1,lambda = TRUE) #Transformacipn Box cox se usa para estabilizar la serie
  rn
  #Probar mejor ajuste
  autoplot(forecast(rn,PI=FALSE, h=tiempo_prediccion), include=50)
  
  #Pronosticos
  f_rn<-forecast(rn,h=tiempo_prediccion,
                 PI=TRUE) #PI para intervalos de confianza
  
  #Exporta resultaros
  write_clip(data.frame(f_rn))
  
  #Valor pronosticados
  p_rn <- f_rn$mean
  
  #Metricas de error
  ajuste_rn<- as.data.frame(
    rbind(rmse(observed, p_rn), #rbind unir por fila (row)
          mae(observed, p_rn),
          mape(observed,p_rn),
          mase(observed, p_rn)))
  colnames(ajuste_rn)<-c("rn")
  print(ajuste_rn)
  
  #Gráficos
  par(mfrow=c(1,2)) 
  plot(f_loess, include=25) #Grafico Loess
  plot(f_rn, include=25) #Grafico rn
  
#===============Resultado final==================================
  
# Consolidación de métricas
ajuste <- as.data.frame(cbind(
  ajuste_se_s,ajuste_se_d,ajuste_se_t,
  ajuste_arima,ajuste_prophet,ajuste_bsts,
  ajuste_loess,ajuste_rn))
  
print(ajuste)

#Exporto resultado final de metricas de error
write_clip(ajuste)

#Gráficos de todos los modelos
par(mfrow=c(2,4)) #2 filas y 3 columnas
plot(f_se_s, include=25) #Grafico se_s
plot(f_se_d, include=25) #Grafico se_d
plot(f_se_t, include=25) #Grafico se_t
plot(f_arima, include=25) #Grafico arima
plot(p_prophet, include=25) #Grafico prophet  
plot(f_bsts, include=25) #Grafico Bsts 
plot(f_loess, include=25) #Grafico LOESS 
plot(f_rn, include=25) #Grafico rn

#Guardar el grafico
setwd("C:/Users/Alexander/OneDrive - UASB/U. Andina/Estadística aplicada a las finanzas/Bases y sintaxis R/Series de tiempo")
dir<-"C:/Users/Alexander/OneDrive - UASB/U. Andina/Estadística aplicada a las finanzas/Bases y sintaxis R/Series de tiempo"
ggsave("Morosidad.png", plot = last_plot(), 
       path = dir, 
       width = 10, height = 6, units = "in")

# Cargar librerías necesarias
library(ggplot2)

# Configurar el layout de los gráficos
par(mfrow = c(2, 4)) # 2 filas y 4 columnas

# Crear gráficos
plot(f_se_s, include = 25, main = "SE Simple")
plot(f_se_d, include = 25, main = "SE Doble")
plot(f_se_t, include = 25, main = "SE Triple")
plot(f_arima, include = 25, main = "Arima")
plot(p_prophet, include = 25, main = "Prophet")
plot(f_bsts, include = 25, main = "BSTS")
plot(f_loess, include = 25, main = "LOESS")
plot(f_rn, include = 25, main = "Red Neuronal")

# Establecer el directorio donde se guardará el gráfico
dir <- "C:/Users/Alexander/OneDrive - UASB/U. Andina/Estadística aplicada a las finanzas/Bases y sintaxis R/Series de tiempo"

# Guardar el gráfico usando ggsave()
ggsave(
  filename = "Morosidad.png",
  path = dir,
  width = 12, height = 6, units = "in",
  dpi = 300
)

AIC(se_t)

se_s$model$aic
se_s$model$bic

se_d$model$aic
se_d$model$bic

str(se_t)

se_t$model$aic
se_t$model$bic