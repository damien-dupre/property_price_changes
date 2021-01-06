library(tidyverse)
library(DBI)
library(RMySQL)
library(tidyquant)
library(dplyr)
library(dbplyr)

cn <- dbConnect(
  drv      = RMySQL::MySQL(), 
  username = "admin",
  password = "XXXXXXXX",
  host     = "daft-mysql.XXXXXX.eu-west-1.rds.amazonaws.com", 
  port     = 3306, 
  dbname   = "daftdb"
  )
dbListTables(cn)
daftdb <- tbl(cn, "daftdb")
real_tibble <- collect(daftdb) %>%
  readr::type_convert()
write_rds(real_tibble, "daftdb.rds")

dbDisconnect(cn)

