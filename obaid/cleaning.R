
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
  mutate_at(vars(region),~ if_else(is.na(.),"Co. Dublin",.)) %>%
  mutate(region= as.factor(region), structure= as.factor(structure)) %>%
  mutate(date= as.Date(date, format = "%d/%m/%y")) %>%
  print()


df %>%
  group_by(region) %>%
  summarise(amount=mean(price)) %>%
  ggplot(aes(x=region,y=amount/100000)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    y = "Mean Price (in Hundred Thousands Euros)",
    x = "Regions",
    title = paste(
      "Mean House Prices regionwise during First Lockdown in Dublin"
    )
  )
  


df %>%
  group_by(month(date)) %>%
  select(price) %>%
  summarise_each(funs(mean)) %>%
  plot()

df %>%
  group_by(month=floor_date(date, "month")) %>%
  summarise(amount=mean(price),region) %>%
  print()

#Bar Pplot
df %>%
  group_by(week=floor_date(date, "week")) %>%
  summarise(amount=mean(price)) %>%
  ggplot(aes(x=week,y=amount/100000, fill=week)) +
    geom_bar(stat = "identity") +
    theme_classic()
 
#line Chart  


date_diff<-date("2020/03/20")-date("1970-01-01")
date_diff<-as.numeric(str_remove(date_diff,"[:alpha:]|[:]|[,]|[\u20AC]|[\u0020]"))
date_diff
duration(20,units = "days")

base_year<-date("1970-01-01")
(base_year %--% date("2020/03/20")) %/% days(1)


df %>%
  group_by(week=floor_date(date, "week")) %>%
  summarise(amount=mean(price)) %>%
  ggplot(aes(x=week,y=amount/100000, fill=week)) +
  geom_line(stat = "identity", col="#00AFBB",size=2) +
  theme_classic()+
  labs(
    y = "Mean Price (in Hundred Thousands Euros)",
    x = "Weeks",
    title = paste(
      "Trend in House Prices (mean) during First Lockdown in Dublin"
    )
  )+
  geom_vline(xintercept =(base_year %--% date("2020/03/20")) %/% days(1), col="#d90429",lab="abcd")+
  geom_label(
    label="20th March", 
    x=(base_year %--% date("2020/03/20")) %/% days(1),
    y=4.8,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "#edf2f4",
    fill="#ef233c"
  )

?geom_vline

df %>%
  group_by(region) %>%
  summarise(amount=mean(price)) %>%
  arrange(desc(amount)) %>%
  ggplot(aes(x=region,y=amount/100000)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    y = "Mean Price (in Hundred Thousands Euros)",
    x = "Regions",
    title = paste(
      "Mean House Prices regionwise during First Lockdown in Dublin"
    )
  ) 

  





#bymonth <- aggregate(cbind(Melbourne,Southern,Flagstaff)~month(Date),
#data=data,FUN=sum)
# Clear environment
rm(list = ls()) 
