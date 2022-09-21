
library(tidyverse)



dados<- read.csv("house_data.csv",h=T) 

glimpse(dados)
summary(dados)
#id, date, zipcode

attach(dados)