if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantmod") }
library(quantmod)
if ("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr") }
library(stringr)

downloadable_stocks <- c("ATVI", "^IXIC")
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01"))


df <- data.frame(get(downloadable_stocks[1]))
downloadable_stocks<-stringr::str_remove(downloadable_stocks, "[:punct:\\^]")

arifm <- function(x, dt = 1) {
  min_elem <- min(x)
  x <- sapply(x, function(x) x + min_elem + 1)
  y <- rep(c(0), length(x) - 2*dt)
  
  for (t in (1 + dt):(length(x) - dt)) {
    y[t - dt] <- log((x[t - dt] + x[t + dt]) / (2 * x[t]))
  }
  
  return(y)
}

geom <- function(x, dt = 1) {
  min_elem <- min(x)
  x <- sapply(x, function(x) x + min_elem + 1)
  y <- rep(c(0), length(x) - 2*dt)
  
  for (t in (1 + dt):(length(x) - dt)) {
    y[t - dt] <- log((x[t - dt] * x[t + dt]) / (x[t]^2))
  }
  
  return (y)
}

garm <- function(x, dt = 1) {
  min_elem <- min(x)
  x <- sapply(x, function(x) x + min_elem + 1)
  y <- rep(c(0), length(x) - 2*dt)
  
  for (t in (1 + dt):(length(x) - dt)) {
    y[t - dt] <- log((x[t - dt] * x[t + dt] * 2) / 
                      (x[t] * (x[t - dt] + x[t + dt])))
  }
  
  return (y)
}

out_of_trend <- function(x, dt = 1, method="Arifm") {
  stopifnot(is.numeric(x))
  stopifnot(length(x) > 2 * dt)
  
 if (method == "Arifm") {
   return(arifm(x, dt))
 } else if (method == "Geom") {
   return(geom(x, dt))
 } else if (method == "Garm") {
   return(garm(x, dt))
 } else {
   stop(paste('Unknown method: ', method))
 }
}

t = seq(0, 10, 0.1)
x = 2* t + 3 + sin(2*t)
mean(x)
xn <- out_of_trend(x, 2, "Garm")
mean(xn)

alter_johns <- function(y) {
  stopifnot(length(y) > 1)
  
  a <- numeric(length(y) - 1)
  for (t in 1:(length(y)-1)) {
    a[t] <- 1 / (length(y) - t) * sum(abs(y[(t + 1):(length(y))] - y[1:(length(y) - t)]))
  }
  return(a)
}

alter_johns(xn)
plot(alter_johns(xn), type="l")