library(tidyverse)
library(here)
library(scales)
library(knitr)
library(modelr)
daftdb <- here("data/daftdb.rds") %>% read_rds()


df <- daftdb %>% 
  mutate(
    area = str_extract(address, "Dublin [0-9]+"),
    price_parsed = parse_number(price, locale = locale(grouping_mark = ","))
  ) %>%
  mutate(area2=parse_number(area))

daftdiff <- df %>% 
  group_by(address, area2) %>% 
  summarise(price_diff = max(price_parsed) - min(price_parsed),bathroom,bedroom,price_parsed,area,area2) %>%
  ungroup()

df2<- daftdiff %>%
  select(price_diff,price_parsed, bedroom,bathroom,area)

lm(df2)


fit2<-lm(price_diff~.,data=df2)

fit2
coef(fit2)
odel
summary(fit2)

fit1<-lm(price_diff~price_parsed+area,data=df2)

# Confidence intervals for coefficients
confint(fit2)

# Predict values of "volunteering"
predict_mod<- data.frame(predict_val=predict(fit2), price_diff=df2$price_diff)


# Prediction intervals for values of "volunteering"
predict(fit2, interval = "prediction")

# Regression diagnostics
lm.influence(fit2)
influence.measures(fit2)

grid <- df2 %>% 
  data_grid(price_diff,price_parsed,bedroom,bathroom,area2) 
grid


grid <- grid %>% 
  add_predictions(fit) 
grid


ggplot(data=df2,)
  
