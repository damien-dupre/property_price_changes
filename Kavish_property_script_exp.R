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
testdf$price <- str_sub(testdf$price,2)
testdf

#replaced empty spaces with 0--------------------------------------------------------
testdf$price <- str_replace_all(testdf$price, "^$","0") 

#testdf$price2 <- gsub("^$", "0", testdf$price2) - This would work too

testdf

#commas removed- not needed ---------------------------------------------------------
#testdf$price2 <- str_replace(testdf$price2, ",","")

#conversion time-------------------------------------------------------------------
testdf$price=as.double(testdf$price)
class(testdf$price2) #successful

summary(testdf$price2)

#separating area -----------------------------------------------------------------
#separate (data, col, into, sep)

#extract(data, col, into, regex = "")

unique(testdf$address)

testdf$dublin_code <- str_extract(testdf$address, "Dublin (.)")

# Find mean in groups: method 1 --------------------------------------------------------
# aggregate(x = iris$Sepal.Length,      Specify data column
#           by = list(iris$Species),    Specify group indicator
#           FUN = mean)                 Specify function (i.e. mean)
# 
# Group.1     x
# setosa 5.006
# versicolor 5.936
# virginica 6.588

# Find mean in groups: method 2 --------------------------------------------------------
# iris %>%                              Specify data frame
#   group_by(Species) %>%               Specify group indicator
#   summarise_at(vars(Sepal.Length),    Specify column
#                list(name = mean))     Specify function

# A tibble: 3 x 2
# Species    Sepal.Length
# <fct>             <dbl>
# setosa             5.01
# versicolor         5.94
# virginica          6.59



