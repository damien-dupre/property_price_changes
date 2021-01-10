if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, rio, psych, party)
(dfdb <- daftdb) %>%
  as_tibble()

# gsub () is used to substitute specific text from a string with other text, 
# and as.numeric () can coerce a variable to numeric.
# gsub(pattern, replacement, x, ignore.case = FALSE)

testdf <- dfdb %>%
  filter(index == 1 | index == 284 | index == 259) %>%
  print()

#gsub("Terraced","Roof",testdf$structure, ignore.case = T)

class(testdf$price)

#trimmed the string -------------------------------------------------------------------
testdf$structure <- str_trim(testdf$structure)
testdf

#replaced the string and trimmed--------------------------------------------------------
testdf$price <- str_replace_all(testdf$price, "[a-z,A-Z,:]","") #"[a-z,A-Z]",""
testdf

testdf$price <- str_trim(testdf$price) #removed whitespaces
testdf

#euro sign removed-------------------------------------------------------------------
testdf$price2 <- str_sub(testdf$price,2)
testdf

#replaced empty spaces with 0--------------------------------------------------------
testdf$price2 <- str_replace_all(testdf$price2, "^$","0") 

#testdf$price2 <- gsub("^$", "0", testdf$price2) - This would work too

testdf

#commas removed- not needed ---------------------------------------------------------
#testdf$price2 <- str_replace(testdf$price2, ",","")

#conversion time-------------------------------------------------------------------
testdf$price2=as.double(testdf$price2)
class(testdf$price2) #successful

summary(testdf$price2)

#separating area -----------------------------------------------------------------
#separate (data, col, into, sep)

#extract(data, col, into, regex = "")

unique(testdf$address)

testdf$dublin_code <- str_extract(testdf$address, "Dublin (.)")


