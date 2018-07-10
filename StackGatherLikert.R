#This is designed to work on the problem of having a dataset with groups of questions and stacking those questions on top of each other. 

library(tidyverse)
library(likert)

#Two datasets with very simple likert scales so I can play with joining together
#the data after setting the factor levels. 

Q1 <- c("One", "Two", "One", "Three", "Two")
Q2 <- c("One", "One", "Two", "One", "One")
Q3 <- c("Three", "One", "Two", "One", "One")
Q4 <- c("Two", "One", "Two", "One", "Two")
Q5 <- c("Three", "Two", "One", "Three", "Three")
Q6 <- c("Two", "Three", "Two", "Two", "Three")
Q7 <- c("Three", "One", "Two", "One", "Three")
Q8 <- c("Two", "Three", "Three", "Three", "Two")
df <- data.frame(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8)

#set the factor levels
levelset <- c("One", "Two", "Three")

#Create a function that sets the factor level each variable in the dataset
# Then unifies the factors so that they're all the same number of factors. 
factfunc <- function(mydata, factlevel){
  factor(mydata, 
         levels=factlevel, 
         ordered = TRUE)
  fct_unify(mydata, 
            levels=factlevel) 
}

#Run this function over dataset
df <-factfunc(df, levelset) %>% as.data.frame()

even_df <- df %>% 
  select(Q2, Q4, Q6, Q8) %>% 
  gather(key = "even", value = "Level")

odd_df <- df %>% 
  select(Q1, Q3, Q5, Q7) %>% 
  gather(key = "odd", value = "Level")