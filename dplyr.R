library(dplyr)

data(faithful)
hf <- tbl_df(faithful)

glimpse(hf)


#Filter
filter(hf,eruptions >5,waiting >3)

#Select
select(hf,eruptions)
select(hf,c(eruptions,waiting))

#reorder
arrange(hf,eruptions)

#Chaining
hf %>%
  select(waiting,eruptions) %>%
  filter(waiting > 70) %>%
  arrange(eruptions)

#mutate
hf %>%
  select(waiting,eruptions) %>%
  mutate(div = waiting/eruptions) %>%
  arrange(div)



