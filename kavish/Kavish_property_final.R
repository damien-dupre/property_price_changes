if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, rio, psych, party)
(dfdb <- daftdb) %>%
  as_tibble()

# gsub () is used to substitute specific text from a string with other text, 
# and as.numeric () can coerce a variable to numeric.
# gsub(pattern, replacement, x, ignore.case = FALSE)

#testdf <- dfdb %>%
#  filter(index == 1 | index == 284 | index == 259) %>%
#  print()

#gsub("Terraced","Roof",testdf$structure, ignore.case = T)

class(dfdb$price)

#trimmed the string -------------------------------------------------------------------
testdf$structure <- str_trim(testdf$structure)
testdf

#replaced the string and trimmed--------------------------------------------------------
dfdb$price <- str_replace_all(dfdb$price, "[a-z,A-Z,:]","") #"[a-z,A-Z]",""
dfdb

dfdb$price <- str_trim(dfdb$price) #removed whitespaces
dfdb

#euro sign removed-------------------------------------------------------------------
dfdb$price <- str_sub(dfdb$price,2)
dfdb

#replaced empty spaces with 0--------------------------------------------------------
dfdb$price <- str_replace_all(dfdb$price, "^$","0")

#testdf$price2 <- gsub("^$", "0", testdf$price2) - This would work too

dfdb

#commas removed- not needed ---------------------------------------------------------
#testdf$price2 <- str_replace(testdf$price2, ",","")

#conversion time-------------------------------------------------------------------
dfdb$price=as.double(dfdb$price)
class(dfdb$price) #successful

summary(dfdb$price)

#Boxplot of price-------------------------------------------------------------------
boxplot(dfdb$price, notch = T, horizontal = T)
boxplot.stats(dfdb$price)

unique(testdf$address)

dfdb$dublin_code <- str_extract(dfdb$address, "Dublin (.)")

