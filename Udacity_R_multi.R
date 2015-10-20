library(ggplot2)

setwd("E:\\R p")
data_user <- read.csv("pseudo_facebook.tsv",sep = "\t")


library(dplyr)
pf.fc_by_age_gender <- data_user %>%
  group_by(age,gender)%>%
  summarise(mean_friend_count  = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>%
  ungroup()%>%
  arrange(age)

ggplot(aes(x=age,y=median_friend_count), data = pf.fc_by_age_gender) +
  geom_line(aes(color=gender))

#Reshape library
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = "median_friend_count")


ggplot(aes(x=age, y = male/female),
       data = pf.fc_by_age_gender.wide) +
  geom_line()+
  geom_hline(yintercept = 1)






