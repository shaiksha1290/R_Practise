x <- rnorm(n = 12,mean = rep(c(1:3),each= 4), sd = 0.2)
y <- rnorm(n = 12,mean = rep(c(1,2,1),each= 4), sd = 0.2)
#plot(x,y,col = "red",pch=20,cex=2)
dataframe <- data.frame(x,y)
kmeansob <- kmeans(dataframe,centers = 3)
names(kmeansob)
kmeansob$cluster
