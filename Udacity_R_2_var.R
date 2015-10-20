library(ggplot2)

setwd("E:\\R p")
data_user <- read.csv("pseudo_facebook.tsv",sep = "\t")

qplot(x=age,y=friend_count,data=data_user)

#using ggplot
ggplot(aes(x=age,y=friend_count),data = data_user) + geom_point() +
  xlim(13,90)

#using alpha 1/20...(it takes 20 points to make a dot in graph)
ggplot(aes(x=age,y=friend_count),data=data_user)+
  geom_point(alpha = 1/20) +
  xlim(13,90)+
  ylim(0,1000)

#using geom_jitter instead of geom_point
ggplot(aes(x=age,y=friend_count),data = data_user) +
  geom_jitter(alpha=1/20) +
  xlim(13,90)

#using coord_trans
ggplot(aes(x=age,y=friend_count),data = data_user) +
  geom_point(alpha=1/20,position = position_jitter(h=0)) +
  xlim(13,90)+
  coord_trans(y="sqrt")

#using Alpha and jitter "aes" aesthatic wrapper
ggplot(aes(x=age,y = friendships_initiated),data=data_user) +
  geom_point(alpha = 1/10) +
  xlim(13,90)+
  coord_trans(y="sqrt")

ggplot(aes(x=age,y = friendships_initiated),data=data_user) +
  geom_jitter(alpha = 1/10) +
  xlim(13,90)

#dplyr package
install.packages("dplyr")
library(dplyr)
d_age <- group_by(data_user,age)
data_user.fc_by_age <- summarise(d_age,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          N = n())


data_user.fc_by_age <- arrange(data_user.fc_by_age,age)


#plot mean friend count vs age
ggplot(aes(x = age , y = friend_count_mean),data = data_user.fc_by_age)+
  geom_point() +
  xlim(13,90)

#Overalying summary with raw data
ggplot(aes(x=age,y=friend_count),data = data_user)+
  geom_point(alpha = 1/10,position = position_jitter(h=0),color="red")+
  coord_trans(y="sqrt")+
  geom_line(stat = "summary", fun.y=mean)+
  geom_line(stat = "summary", fun.y = quantile,probs = 0.1,
            linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y=quantile,prob=0.9,
            linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y=quantile,prob=0.5,
            linetype = 1, color = "black")+
  coord_cartesian(xlim=c(13,90))
  
#finding correlation
cor.test(x = data_user$age,y=data_user$friend_count,data = data_user,method="pearson")
#using with
with(data_user,cor.test(x=age,y=friend_count,method="pearson"))

#scatter plot ofr likes_recived and www_likes_received
ggplot(aes(x=likes_received,y=www_likes_received), data = data_user)+
  geom_point()+
  coord_cartesian(xlim=c(0,quantile(data_user$likes_received,0.95)),
                  ylim =c(0,quantile(data_user$www_likes_received,0.95)) )
  


#Strong correlation
with(data_user,cor.test(x=likes_received,y=www_likes_received))

#deceptive correlation
install.packages("alr3")
library(alr3)
data(Mitchell)

ggplot(aes(x=Month,Y=Temp),data = Mitchell)+
  geom_point(alpha = 1/20)+
  scale_x_discrete(breaks=seq(0,203,12))

#friend count mean and age




with(Mitchell,cor.test(Month,Temp))



#users ages in months
data_user$age_in_months <- data_user$age + 1 - (1/12)*data_user$dob_month

suppressMessages(library(dplyr))
pf.fc_by_age_month <- data_user %>%
                    group_by(age_in_months)%>%
                    summarise(fiend_mean = mean(friend_count),
                              friend_median = median(friend_count),
                              n = n())%>%
                    arrange(age_in_months)
  
ggplot(aes(x=age_in_months,y=fiend_mean),data = pf.fc_by_age_month) +
  geom_line()+
  coord_cartesian(xlim = c(13,77),ylim = c(0,500)) 


#problem set with Diamond
data(diamonds)

ggplot(aes(x=diamonds$price,y = diamonds$x),data=diamonds) +
  geom_point()

cor.test(x=diamonds$price,y=diamonds$x)
cor.test(x=diamonds$price,y=diamonds$y)
cor.test(x=diamonds$price,y=diamonds$z)


ggplot(aes(x=diamonds$price,y=diamonds$depth),data= diamonds) +
  geom_jitter(alpha = 1/100) +
  xlim(0,5000)+
  ylim(55,65)+
  #(aplha = 1/1000) +
  scale_x_continuous(breaks=seq(0,2000,2))
  
cor.test(x=diamonds$price,diamonds$depth,method = "pearson")

ggplot(aes(x=diamonds$price,y=diamonds$carat),
       data = subset(diamonds,
                     quantile(diamonds$price,probs = 0.99) & 
                       quantile(diamonds$carat,probs = 0.99))) +
  geom_point()

diamonds$volumne <- diamonds$x*diamonds$y*diamonds$z

ggplot(aes(x=diamonds$price,y=diamonds$volumne),data = diamonds) +
  geom_point(alpha = 1/10) +
  coord_cartesian(ylim = c(0,500) )

diamonds_sub <- subset(diamonds,volumne <= 800 & volumne >0)

cor(diamonds_sub$price,diamonds_sub$volumne)

ggplot(aes(x=diamonds_sub$price,y=diamonds_sub$volumne), 
       data = subset(diamonds_sub,diamonds_sub$volumne <= 800 & diamonds_sub$volumne >0)) +
  geom_point(alpha = 1/100) +
  coord_cartesian(ylim = c(0,400))+
  geom_smooth() +
  geom_line(stat = "summary",y.fun=mean,color="red")



diamondsByClarity <- diamonds %>%
  group_by(clarity)%>%
  summarise(price_mean = mean(price),
           price_median = mean(price),
           min_price = min(price),
           max_price = max(price),
           n = n()) %>%
  arrange(clarity)


diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- ggplot(aes(x=mean_price),data = diamonds_mp_by_clarity ) +
  geom_bar()

p2 <- ggplot(aes(x=mean_price),data = diamonds_mp_by_color ) +
  geom_bar()

grid.arrange(p1,p2,ncol=1)






