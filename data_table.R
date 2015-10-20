
library(data.table)

rm(list  = ls())

data(cars)

data_table <- data.table(cars)

sapply(data_table,class)

data_1 <- data.table(x=seq(1,10,1),y = c("a","a","z","b","f","z"))

