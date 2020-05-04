
# -- ----------------------------------------------------------------------------------------- -- #
# -- Inicializador general de sistema ------------------------------------------------ ETAPA 0 -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Remover Objetos de Environment
rm(list=ls())

# -- -------------------------------------------------------------------------- ETAPA 0.0 -- #
# -- ---------------------------------------------------------------- Paquetes a utilizar -- #

# Codigo para cargar paquetes, si no estan instalados, los instala en automatico
pkg <- c("dplyr", "forecast", "fUnitRoots", "ggplot2", "gridExtra", "TSA", "ts", "zoo")

# Modificar verbose = TRUE para visualizar mensajes de consola

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0)
  install.packages(pkg[!inst], quiet = TRUE, verbose = FALSE)
instpackages <- lapply(pkg, library, character.only=TRUE)

# si se hace "source" del script, utilizar la siguiente linea
# setwd(getSrcDirectory()[1])

# si se hace "run" del script, utilizar la siguiente linea
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -- ------------------------------------------------------------------- Scripts de apoyo -- #
# -- ------------------------------------------------------------------------------------ -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
DataVisualization <- paste(RawGitHub,"RDataVisualization/master/RDataVisualization.R",sep="")
downloader::source_url(DataVisualization,prompt=FALSE,quiet=TRUE)

# -- ------------------------------------------------------------ Cargar y preparar datos -- #
# -- ------------------------------------------------------------------------------------ -- #

data        <- read.csv("datos/examen_iteso.csv")
no_examen   <- 's1'
data_examen <- data[, c('dia', no_examen)]
data_examen$dia <- as.Date(data_examen$dia)
data_examen$s1  <- data_examen$s1

# -- ------------------------------------------------------------------------- BoxJenkins -- #
# -- ------------------------------------------------------------------------------------ -- #

# -- ---------------------------------------------------------- Paso 0: Graficar la serie -- #

# -- grafica base
grafico <- ggplot(data_examen, aes(x=dia, y=s1)) + 
  geom_line(colour = 'steel blue', size = 1)  + 
  scale_x_date(date_labels = "%d-%b-%y") + 
  scale_y_continuous(breaks = round(seq(min(data_examen$s1), max(data_examen$s1), 
                                        (max(data_examen$s1)-min(data_examen$s1))/10),2))

# -- cambiar el tema de la grafica
grafico <- grafico + theme(panel.background = element_rect(fill="white"),
           panel.grid.minor.y = element_line(size = .25, color = "dark grey"),
           panel.grid.major.y = element_line(size = .25, color = "dark grey"),
           panel.grid.minor.x = element_line(size = .25, color = "dark grey"),
           panel.grid.major.x = element_line(size = .25, color = "dark grey"),
           axis.text.x =element_text(colour = "black",size = 12, hjust =.5, vjust = 0),
           axis.text.y =element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
           axis.title.x=element_text(colour = "black",size = 16, hjust =.5,vjust = 0),
           axis.title.y=element_text(colour = "black",size = 16, hjust =.5,vjust = 1),
           title = element_text(colour = "black", size = 18, hjust = 0, vjust = 1), 
           panel.border = element_rect(linetype = 1, colour = "dark grey", fill = NA))

# -- etiquetas para grafica de serie original
grafico_original <- grafico + labs(title = 'Serie original', x = 'fechas', y = 'valores') +
                    geom_hline(yintercept=mean(data_examen$s1),
                               linetype="dashed", color = "red", size=1) +
                    geom_text(aes(data_examen$dia[1], 0.5,
                                  label = "media", vjust = -1), color = "red") + 
                    geom_hline(yintercept=var(data_examen$s1),
                               linetype="dashed", color = "blue", size=1) +
                    geom_text(aes(data_examen$dia[1], 0.75,
                                  label = "varianza", vjust = -1), color = "blue")

# -- grafico con medias moviles
ventanas <- c(10, 40, 80)
rolling_mean_a  <- c(rep(0, ventanas[1]-1),
                     rollapply(data_examen$s1, ventanas[1], function(x) mean(x)))
rolling_mean_b <- c(rep(0, ventanas[2]-1),
                    rollapply(data_examen$s1, ventanas[2], function(x) mean(x)))
rolling_mean_c <- c(rep(0, ventanas[3]-1),
                    rollapply(data_examen$s1, ventanas[3], function(x) mean(x)))

grafico_media <- grafico + geom_line(aes(y=rolling_mean_a), colour = 'blue', size = 1) +
                 geom_line(aes(y=rolling_mean_b), colour = 'green', size = 1) + 
                 geom_line(aes(y=rolling_mean_c), colour = 'orange', size = 1) +
                 labs(title = 'Serie 1 + 3 Medias (10, 40, 80)', x = 'fechas', y = 'valores')

# -- grafico con varianzas moviles
ventanas <- c(10, 40, 80)
rolling_var_a  <- c(rep(0, ventanas[1]-1),
                    rollapply(data_examen$s1, ventanas[1], function(x) var(x)))
rolling_var_b <- c(rep(0, ventanas[2]-1),
                   rollapply(data_examen$s1, ventanas[2], function(x) var(x)))
rolling_var_c <- c(rep(0, ventanas[3]-1),
                   rollapply(data_examen$s1, ventanas[3], function(x) var(x)))

grafico_var <- grafico + geom_line(aes(y=rolling_var_a), colour = 'blue', size = 1) +
               geom_line(aes(y=rolling_var_b), colour = 'green', size = 1) + 
               geom_line(aes(y=rolling_var_c), colour = 'orange', size = 1) + 
               labs(title = 'Serie 1 + 3 Varianzas (10, 40, 80)', x = 'fechas', y = 'valores')

# -- Acomodar todas las graficas en una matriz de (3, 1)
grid.arrange(grafico_original, grafico_media, grafico_var, nrow=3, ncol=1)


# --------------------
# Conclusiones PASO 0:
# --------------------

# En la grafica de "Serie original" se puede apreciar que, tanto la media como la varianza
# estan muy cercanas a 0

# En las graficas de medias y varianzas moviles se puede apreciar que, tanto la media como
# la varianza parece no tener mucha variacion conforme pasa el tiempo, aun y calculando
# estas medidas para distintas ventanas de datos se observan, ambas, relativamente constantes
# y alrededor de 0. 

# -------------------
# CONCLUSION PARCIAL:
# -------------------
# Visualmente no se observa un cambio de la media y de la varianza 
# conforme al tiempo, se observan "constantes".


# -- -------------------------------------------------- Paso 1: Prueba de estacionariedad -- #

# obtener orden de diferenciacion sugerido
ord = ar(diff(data_examen$s1), method="mle")

# prueba de estacionariedad (HO = la serie tiene raiz unitaria)
adfTest(data_examen$s1, lags=ord$order) # se rechaza HO, no es integrada de orden 1.

# --------------------
# Conclusiones PASO 1:
# --------------------

# la serie de tiempo de los datos tiene un orden de integracion de 0, es decir, 
# podemos decir que se valida lo observado visualmente en el paso anterior, que es,
# por lo menos los 2 primeros momentos estadisticos de la serie no varian conforme el tiempo.

# -------------------
# CONCLUSION PARCIAL:
# -------------------

# La informacion obtenida de las visualizaciones y la de la prueba de estacionariedad son
# consistentes entre si, también, si la media de la serie de tiempo es 0 y la varianza
# tambien es constante, se podria estar frente a un proceso de ruido blanco. Solo que esto
# se podra probar en los siguientes pasos

# -- --------------------------------------------------------- Paso 2: Pruebas FAC y FACP -- #

acf(data_examen$s1)    # Todos los resagos resultan ser no significativos
pacf(data_examen$s1)   # Todos los resagos resultan ser no significativos

# --------------------
# Conclusiones PASO 2:
# --------------------

# no se tienen resagos significativos para la componente AR ni para la MA, además, que
# en el paso anterior se probó que el orden de integración de la serie es 0, por lo tanto
# el modelo que se ajusta es un ARIMA(0, 0, 0). 

# -------------------
# CONCLUSION PARCIAL:
# -------------------

# resulto visualmente bastante claro que ningun resago esta cerca de considerarse 
# significativo, tanto para la FAC como para la FACP los ordenes son 0.

# -- --------------------------------------------------- Paso 3: Estimación de parámetros -- #

modelo <- arima(data$s1, order = c(0,0,0))

# --------------------
# Conclusiones PASO 3:
# --------------------

# Busque intentar ajustar un modelo con (0, 0, 0) para conocer que hace la funcion de arima 
# en estos casos, y al final, el resultado sigue siendo consistente, 
# no hay parametros significativos y no ajusta ningun coeficiente. Además, el coeficiente
# que se sugiere es de un valor muy pequeño y con un s.e. también pequeño.

# -------------------
# CONCLUSION PARCIAL:
# -------------------

# Se tiene definitivamente un proceso de ruido blanco, en el que las variables generadas
# por el proceso son independientes e identicamente distribuidas entre si, con media 0 
# y varianza sigma^2. Entonces, por definición, estos datos son completamente aleatorios 
# y no se puede razonablemente modelarlos y hacer predicciones.

# -- ------------------------------------------------------- Paso 4: Pruebas a residuales -- #

acf(modelo$residuals)
pacf(modelo$residuals)

# --------------------
# Conclusiones PASO 4:
# --------------------

# Solo por curiosidad, los residuales de lo que seria un modelo, siguen siendo 
# no significativos.

# -------------------
# CONCLUSION PARCIAL:
# -------------------

# Todo apunta a que es un ruido blanco, aunque, me gustaria hacer un par de pruebas mas
# 1) ver que opina el auto.arima, 2) hacer una prueba de normalidad de los datos para ver
# si es un ruido blanco gaussiano.

# -- ------------------------------- Validaciones extra: AutoArima y prueba de normalidad -- #
# -- ------------------------------------------------------------------------------------ -- #

modelo_autoarima <- auto.arima(data_examen$s1)
print(modelo_autoarima)

# auto.arima arroja tambien un modelo ARIMA(0,0,0), con media cero. Otra validacion 
# de que la serie de tiempo es ruido blanco

shapiro.test(data_examen$s1)

# el valor de p-value > 0.05, por lo tanto, con un 95% de confianza se puede decir que
# la distribucion de los datos no es significativamente distinta que una distribucion normal
# asi que, se puede asumir normalidad

# -- ------------------------------------------------------------------------------------- -- #
# CONCLUSION FINAL
# -- ------------------------------------------------------------------------------------- -- #

# La serie de tiempo son datos del tipo ruido blanco gaussiano
