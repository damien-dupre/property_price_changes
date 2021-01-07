
# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyverse,stringr)
# Load contributed packages with pacman

(daftdb <- rio::import("data/daftdb.rds") %>% as_tibble())


df<-daftdb %>%
  mutate(price=iconv(enc2utf8(daftdb$price),sub="byte")) %>%
  mutate(address=iconv(enc2utf8(daftdb$address),sub="byte")) %>%
  mutate(sale_type= case_when(str_detect(price,"^Reserve") == TRUE ~ "Auction", TRUE ~ "Sale")) %>%
  mutate(price=str_remove_all(price,"???")) %>%
  mutate(price=str_remove_all(price,",")) %>%
  mutate(price=str_remove(price,"^Reserve:")) %>%
  mutate(price=str_remove(price,"^AMV:")) %>%
  mutate(price=str_replace(price,"Price On Application","0")) %>%
  mutate(price=str_remove(price,"Reserve not to exceed:")) %>%
  mutate(price=as.numeric(price)) %>%
  mutate(region=str_extract(address, "Dublin [0-9]+")) %>%
  mutate(region= as.factor(region), structure= as.factor(structure)) %>%






# Clear environment
rm(list = ls()) 
