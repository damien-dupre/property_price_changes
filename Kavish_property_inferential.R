library(tidyverse)
library(psych)
library(lubridate)
library(here)


daftdb <- here("data/daftdb.rds") %>%
  read_rds()

#New data frame created ----------------------------------------------------------------
(dfdb <- daftdb) %>%
  as_tibble()

#remove houses on auction or price not available ---------------------------------------
#removing them and NOT putting 0's as this would skew the data -------------------------
dfdb <- dfdb %>%
  filter(str_detect(price, "Reserve") == "FALSE") %>%
  filter(str_detect(price, "AMV") == "FALSE") %>%
  filter(str_detect(price, "Price") == "FALSE") %>% 
  print()

#replaced any string or commas --------------------------------------------------------
dfdb$price <- str_replace_all(dfdb$price, "[a-z,A-Z]","")

#euro sign removed-------------------------------------------------------------------
dfdb$price <- str_sub(dfdb$price,2)

#conversion time-------------------------------------------------------------------
dfdb$price=as.double(dfdb$price)
class(dfdb$price) #successful
dfdb
summary(dfdb$price)

#separating areas and adding "Co. Dublin" in place of NAs --------------------------
dfdb <- dfdb %>%
  mutate(dublin_code = str_extract(address, "Dublin [0-9]+")) %>%
  mutate(dublin_code = as.factor(dublin_code)) %>%
  mutate(dublin_code = fct_explicit_na(dublin_code, na_level = "Co. Dublin")) %>%
  print()

# #trimmed the house structure string and converted to factor ----------------------------
# dfdb <- dfdb %>% 
#   mutate(structure = str_trim(structure)) %>%
#   mutate(structure = as.factor(structure)) %>%
#   print()

#tabular info of structure code-wise --------------------------------------------------
dfdb %>%
  select(dublin_code, structure) %>%
  table()

#correlation ----------------------------------------------------------------------
cor.test(dfdb$bathroom, dfdb$price) #p-value < 0.05 : statistically significant
cor.test(dfdb$bedroom, dfdb$price) #p-value < 0.05 : statistically significant


#calculating difference in prices --------------------------------------------------
diffdb <- dfdb %>%
  group_by(address, dublin_code) %>%
  summarise(price_diff = max(price) - min(price), bath=max(bathroom), 
            bed=max(bedroom), price=max(price)) %>%
  ungroup()


#creating new column to determine high and low-priced houses ---------------------
summary(diffdb$price)
diffdb$price_cat <- ifelse(diffdb$price > 400000,'High price','Low price') 
#another way of coding
#diffdb$price_cat <- ifelse(diffdb$price > 400000,1,0) #1=High, 0 = Low
diffdb <- diffdb %>%
  mutate(price_cat = as.factor(price_cat))

#creating new columns to determine higher and lower no. of bedrooms and bathrooms -----
summary(diffdb$bed)
diffdb$bed_cat <- ifelse(diffdb$bed > 3,'Higher bedrooms','Lower bedrooms')
diffdb <- diffdb %>%
  mutate(bed_cat = as.factor(bed_cat))

summary(diffdb$bath)
diffdb$bath_cat <- ifelse(diffdb$bath > 2,'Higher bathrooms','Lower bathrooms')
diffdb <- diffdb %>%
  mutate(bath_cat = as.factor(bath_cat))

#In real world, we ought to filter out houses with 0 bathrooms and 0 bedrooms
#Technically, this won't be possible!



#Conducting Independent Sample T-test price_diff vs house pricing ------------------
#If difference in prices is dependent on higher or lower house prices
#H0 - There is no effect on difference in prices due to house pricing 
#H1 - There is an effect on difference in prices due to house pricing
t.test(diffdb$price_diff ~ diffdb$price_cat)

#Since the p-value (0.0002812) < 0.05, then null-hypothesis is rejected.



#Conducting Independent Sample T-test: price_diff vs no. of bedrooms -----------------
#If difference in prices is dependent on no. of bedrooms
#H0 - There is no effect on difference in prices due to no. of  bedrooms 
#H1 - There is an effect on difference in prices due to no. of  bedrooms
t.test(diffdb$price_diff ~ diffdb$bed_cat)

#Since the p-value (0.001919) < 0.05, then null-hypothesis is rejected.



#Conducting Independent Sample T-test: price_diff vs no. of bathrooms -----------------
#If difference in prices is dependent on no. of bathrooms
#H0 - There is no effect on difference in prices due to no. of bathrooms 
#H1 - There is an effect on difference in prices due to no. of bathrooms
t.test(diffdb$price_diff ~ diffdb$bath_cat)

#Since the p-value (0.8833) > 0.05, then null-hypothesis is accepted.



#Conducting One-way ANOVA test (more then 2 levels) --------------------------------
#If difference in prices is dependent on different areas in Dublin
#H0 - There is no effect on difference in prices due to different areas in Dublin 
#H1 - There is an effect on difference in prices due to different areas in Dublin
anova_one_way <- aov(diffdb$price_diff ~ diffdb$dublin_code, data = diffdb)
summary(anova_one_way)

#Since the p-value (0.0203) < 0.05, then null-hypothesis is rejected.


#Conducting linear regression -----------------------------------------------------
fit1 <- lm(price_diff ~ price + bed + bath + dublin_code, data = diffdb)
summary(fit1)


