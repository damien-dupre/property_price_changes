if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, party, psych, rio, tidyverse, dplyr)

library(stringr)
(daftdb <- rio::import("data/daftdb.rds") %>% as_tibble())

df<-daftdb

(df$price<-str_sub(df$price,2,nchar(df["price"])))


text<-"138 Church Road, East Wall, Dublin 3"
pos<-str_locate(pattern="Dublin", text)[1]
str_view(a)

df


df %>%
  select(price,address) %>%
  mutate(price= as.numeric(price)) %>%
  print()


?mutate


# Clear environment
rm(list = ls()) 
