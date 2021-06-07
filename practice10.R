covid_data <- read.csv("covid.csv")

library(ggplot2)
colnames(covid_data) <- gsub('[.]',' ', colnames(covid_data))

#plotting geom_point
ggplot(covid_data, mapping = aes(x=rownames(covid_data),y=Russia))+
  geom_point(col='yellow', size=5)

ggplot(covid_data, mapping=aes(x=rownames(covid_data), y=Austria))+
  geom_point(col='purple', size=5)

ggplot(covid_data, mapping=aes(x=rownames(covid_data), y=Netherlands))+
  geom_point(col='green', size=5)

#plotting hist 
ggplot(data = covid_data, mapping=aes(Japan)) +
  geom_histogram(aes(y=..density..),
                 bins=15,
                 color="green", 
                 fill="white") +
  geom_density(fill="black", alpha = 0.5)

ggplot(data=covid_data, mapping=aes(Zambia)) +
  geom_histogram(aes(y=..density..),
                 bins=15,
                 color="red", 
                 fill="pink") +
  geom_density(fill="blue", alpha=0.5)

ggplot(data=covid_data, mapping=aes(France)) +
  geom_histogram(aes(y=..density..),
                 bins=15,
                 color="orange", 
                 fill="yellow") +
  geom_density(fill="blue", alpha=0.5)

dev.off()