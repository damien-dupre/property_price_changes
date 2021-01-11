
# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyverse,stringr,lubridate)
# Load contributed packages with pacman

(daftdb <- rio::import("data/daftdb.rds") %>% as_tibble())


daftdb %>%
  select(price)


df<-daftdb %>%
  mutate(price=iconv(enc2utf8(daftdb$price),sub="byte")) %>%
  mutate(address=iconv(enc2utf8(daftdb$address),sub="byte")) %>%
  mutate(sale_type= case_when(str_detect(price,"^Reserve") == TRUE ~ "Auction", TRUE ~ "Sale")) %>%
  mutate(price=str_remove_all(price,"[:alpha:]|[:]|[,]|[\u20AC]|[\u0020]")) %>%
  mutate(price=as.numeric(price)) %>%
  mutate_at(vars(price),~ if_else(is.na(.),0,.)) %>%
  mutate(region=str_extract(address, "Dublin [0-9]+")) %>%
  mutate(region= as.factor(region), structure= as.factor(structure)) %>%
  mutate(date= as.Date(date, format = "%m/%d/%y")) %>%
  print()



df %>%
  group_by(month(date)) %>%
  select(price) %>%
  summarise_each(funs(mean)) %>%
  plot()

df %>%
  group_by(month=floor_date(date, "week")) %>%
  summarise(amount=mean(price)) %>%
  plot()

df %>%
  group_by(month=floor_date(date, "week")) %>%
  summarise(amount=mean(price),structure) %>%
  plot()



df %>%
  mutate(month= month(date)) %>%
  group_by(day) %>%
  summarise( avg_price= mean(price, na.rm = TRUE)) %>%
  ggplot(aes(month, avg_price)) +
  geom_line()

#bymonth <- aggregate(cbind(Melbourne,Southern,Flagstaff)~month(Date),
#data=data,FUN=sum)
# Clear environment
rm(list = ls()) 
