library(ggplot2)

setwd("E:\\R p")
data_user <- read.csv("pseudo_facebook.tsv",sep = "\t")
head(data_user,n = 3)
names(data_user)
cat("Rows ",nrow(data_user)," Colums = ",ncol(data_user))

qplot(x = dob_day,data = data_user) +
  scale_x_discrete(breaks=1:31)+
  facet_wrap(~dob_month,ncol = 3)

qplot(x = friend_count,data = data_user,xlim = c(1,500))


qplot(x = friend_count,data = data_user) +
  scale_x_continuous(limits = c(1,100)) 

#grouping based on gender
qplot(x = friend_count,data=data_user,xlim=c(1,1000)) + 
  scale_x_continuous(breaks=seq(1,1000,50)) +
  facet_wrap(~gender)

#using subset to remove NA
qplot(x = friend_count,data=subset(data_user,!is.na(gender)),xlim=c(1,1000)) + 
  scale_x_continuous(limits = c(0,1000),breaks=seq(1,1000,50)) +
  facet_wrap(~gender)

#using na.omit
qplot(x = friend_count,data=na.omit(data_user),xlim=c(1,1000)) + 
  scale_x_continuous(breaks=seq(1,1000,50)) +
  facet_wrap(~gender)


table(gender)

by(friend_count,gender,summary)

#using color and fill
qplot(x=tenure,data=data_user, binwidth = 30,
           color = I("black"), fill = I("blue")) +
  scale_x_continuous(breaks = seq(1,1000,50))

#measure tenure in years
qplot(x=tenure/365,data=data_user,binwidth = 1,
      color = I("red"),fill=I("black")) +
  scale_x_continuous(breaks=seq(1,7,1),lim = c(0,7))

#naming X and Y lables
qplot(x=tenure/365,data=data_user,
      xlab = "NUmber of years",
      ylab = "Number of user",
      color = I("Red"), fill = I("black")) +
  scale_x_continuous(breaks = seq(1,7,1),lim=c(0,7))

#related to age
qplot(x=age,data=data_user,binwidth = 5,
      color = I("red"), fill = I("black"),
      xlab = "Age of users",
      ylab = "NUmber of users" ) +
  scale_x_continuous(breaks=seq(1,100,5),lim=c(15,100))


#creating 3 histograms in one output pane
install.packages(gridExtra)
library(gridExtra)

p1 = qplot(x=friend_count,data=data_user,
           color=I("red"),fill=I("black"))
p2 = qplot(x=log10(friend_count+1),data=data_user,
           color=I("red"),fill=I("black"))
p3 = qplot(x=sqrt(friend_count),data=data_user,
           color=I("red"),fill=I("black"))

grid.arrange(p1,p2,p3,ncol = 1)


#method 2

pp1 <- ggplot(aes(x= friend_count),data=data_user) + geom_histogram()
pp2 <- pp1 + scale_x_log10()
pp3 <- pp1 + scale_x_sqrt()

grid.arrange(pp1,pp2,pp3,ncol=1)


#Frequency Polygons

qplot(x=friend_count, y = ..count../sum(..count..),
      data=subset(data_user,!is.na(gender)),binwidth=10,
      xlab = "Number of Friends",
      ylab = "Frequency of Users",
      geom = "freqpoly",color = gender) +
  scale_x_continuous(lim = c(0,1000),breaks = seq(0,1000,50)) 

#Applying log
qplot(x=log10(friend_count+1), y = ..count../sum(..count..),
      data=subset(data_user,!is.na(gender)),binwidth=0.5,
      xlab = "Number of Friends",
      ylab = "Frequency of Users",
      geom = "freqpoly",color = gender) 
  #scale_x_continuous(lim = c(0,1000),breaks = seq(0,1000,50)) 

#Which gender makes more likes 

qplot(x=www_likes,data=data_user,geom="freqpoly",color=gender)+
  scale_x_continuous()+
  scale_x_log10()

qplot(x=www_likes,
      data=data_user,geom="freqpoly",color=gender)+
  scale_x_continuous(lim = c(0,500),breaks = seq(0,500,50))+
  scale_x_log10()


qplot(x=www_likes,y = ..count../sum(..count..),
      data=data_user,geom="freqpoly",color=gender)+
  scale_x_continuous(lim = c(5,50),breaks = seq(5,50,5))

#Who has more likes
by(www_likes,gender,sum)
sum(subset(data_user,gender == "female" & www_likes >0 ,select = www_likes))


#BoxPLots

qplot(x=gender,y=friend_count,data=subset(data_user,!is.na(gender)),
      geom="boxplot") +
  scale_y_continuous(lim = c(0,500))

qplot(x=gender,y=friend_count,data=subset(data_user,!is.na(gender)),
      geom="boxplot") +
  coord_cartesian(ylim=c(0,1000))

#WHo initiated friendship
qplot(x=gender,y=friendships_initiated,data = subset(data_user,!is.na(gender)),
      geom="boxplot")+
  coord_cartesian(ylim = c(0,500))


#Diamond
data("diamonds")
colnames(diamonds)
qplot(diamonds$price,data=diamonds)

qplot(diamonds$price,data=diamonds,binwidth=100,color=I("red"),fill=I("black"))+
  scale_x_continuous(breaks=)

qplot(x=diamonds$price,data=diamonds)+
  facet_wrap(~cut)
  
nrow(subset(diamonds,diamonds$price >=15000))
help(subset)
