id <- 1:3
country <- as.factor(c("Flatland", "Wonderland", "Sphereland"))
craziness <- c(20, 15, 18)
region_type <- c("A", "B", "A")
author <- as.factor(c("Abbot", "Carroll", "Burger"))
size <- c(10, 100, 30)

df <- data.frame(id, country, craziness, region_type, author, size)

s <- sapply(df, class)

first <- function(df) {
  classes <- c()
  for (i in 1:length(df[1,])) {
    classes <- cbind(classes, class(df[,i]))
  }
  
  return (classes)
}

#2
s <- df[sapply(df, is.numeric)]

second <- function(df) {
  result <- data.frame(temp = rep(c(0), length(df[,1])))
  for (i in 1:length(df[1,])) {
    if (class(df[,i]) == "numeric" || class(df[,i]) == "integer") {
      result[colnames(df[i])] <- df[i]
    }
  }
  
  result <- result[-1]
  
  return (result)
}

#3
median <- function(vec) {
  if (is.numeric(vec)) {
    return(median(vec))
  } else {
    stop("Vector is not numeric, cannot compute the median")
  }
}