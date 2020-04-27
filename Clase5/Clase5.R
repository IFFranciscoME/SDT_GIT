
# -- ------------------------------------------------------------------------------------------------------ #
# -- Clase Series de Tiempo
# -- Ejercicios en R
# -- Francisco Muñoz - franciscome@iteso.mx
# -- ------------------------------------------------------------------------------------------------------ #

rm(list=ls())
library(quantmod)

# Valor de Cierre del Bitcoin.
getSymbols("BTC-USD", src="yahoo")
btc <- data.frame(date=index(`BTC-USD`), coredata(`BTC-USD`))
btc_cierre <- btc$BTC.USD.Close
plot(btc_cierre, type="l")
# Producto Interno Bruto Estados Unidos, trimestral no ajustado
getSymbols("ND000334Q", src="FRED")
gdp <- data.frame(date=index(`ND000334Q`), coredata(`ND000334Q`))
plot(gdp, type="l")
# DowJones Industrial Average
getSymbols("DJIA", src="FRED")
dja <- data.frame(date=index(`DJIA`), coredata(`DJIA`))
plot(dja, type="l")
g_dja <- log(dja$DJIA)-log(dja$DJIA[-1])
g_dja[length(g_dja)]<- NA
plot(g_dja, type="l")

getSymbols("AAPL", src="yahoo")
apl <- data.frame(date=index(`AAPL`), coredata(`AAPL`))
apl_cierre <- apl$AAPL.Close
apple <- tail(apl_cierre,1000)
dow <- tail(dja$DJIA, 1000)
plot(apple, dow)

autocor <- function(x,lag){
  xbar <- mean(x)
  numerador <- sum((x[2:length(x)]-xbar)*(x[1:(length(x)-1)]-xbar))
  denominador <- sum((x-xbar)^2)
  return(numerador/denominador)
}

cor(apl$AAPL.Adjusted[1:10], DJIA[1:10])
cor(apl$AAPL.Adjusted[1:10], gdp$ND000334Q[1:10])

tautocor <- function(x, lag){
  rho <- NA
  t <- NA
  for(i in 1:lag){
    rho[i]<- autocor(x,i)
    rho2 <- rho^2
    t[i] <- rho[i]/sqrt((1+2*sum(rho2[1:(i-1)]))/length(x))
  }
  return(cbind(rho,(1-pnorm(abs(t)))/2))
}

tautocor(apl$AAPL.Close, 10)


