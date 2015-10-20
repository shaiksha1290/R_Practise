library(ggplot2)

loan_data <- read.csv("Subset of Loans Modified fall 2015.csv")

names(loan_data)
loan_data$int_rate <- as.numeric(strsplit(as.character(loan_data$int_rate),split = "%"))

qplot(x=loan_data$int_rate, data = loan_data) 