library(data.table)
library(ggplot2)
air_delay <- data.table(read.csv("C:\\Users\\shaik\\Downloads\\45901452_T_ONTIME\\45901452_T_ONTIME.csv"))

head(air_delay)
 colnames(air_delay)
 
 air_delay_ids <- air_delay[DEP_DELAY > 0,
           c(AIRLINE_ID,ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID),
           by = MONTH]
 
air_delay <- air_delay[DEP_DELAY >0]

ggplot(aes(x = DAY_OF_MONTH,y=DEP_DELAY),data = air_delay) +
  geom_point(alpha = 1/100) +
  geom_line(stat="summary",fun.y = mean,color = "red")+
  coord_cartesian(ylim = c(0,100))

air_delay_1 <- air_delay[,,by=(ORIGIN_CITY_NAME)]

head(air_delay_1)
air_delay[,mean(DEP_DELAY),by=DAY_OF_MONTH]

mean(air_delay[DAY_OF_MONTH == 1,DEP_DELAY,])

ss <- lapply(unique(air_delay$ORIGIN_AIRPORT_ID), 
       function(x){air_delay[i = ORIGIN_AIRPORT_ID == x,j = .(count = .N),by =DEST_AIRPORT_ID ]
      })

lapply(air_delay, class)

library(caret)

air_delay <- transform(air_delay,DEP_DEL15 = as.factor(DEP_DEL15))


air_Delay_train <- createDataPartition(y=air_delay$DEP_DEL15,p=0.75,list = FALSE)

train_air <- air_delay[air_Delay_train[,1]]
val_air <- air_delay[-air_Delay_train[,1]]


table(train_air$DEP_DEL15,train_air$ORIGIN_AIRPORT_ID)

train_pre <- preProcess(train_air[,j=-c(train_air$DEP_DEL15),],method = c("center","scale"))


nearZeroVar(train_air,saveMetrics = TRUE)


write_data <- air_delay[ORIGIN == "JFK"]

write.csv(write_data,file = "output.csv")
