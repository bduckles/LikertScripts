#Toy Likert Dataset

library(tidyverse)
library(likert)

# Data --------------------------------------------------------------------


#Two datasets with very simple likert scales so I can play with joining together
#the data after setting the factor levels. 

AQ1 <- c("One", "Two", "One", "Three", "Two")
AQ2 <- c("One", "One", "Two", "One", "One")
AQ3 <- c("Three", "One", "Two", "One", "One")
AQ4 <- c("Two", "One", "Two", "One", "Two")
Adf <- data.frame(AQ1, AQ2, AQ3, AQ4)

BQ1 <- c("Three", "Two", "One", "Three", "Three")
BQ2 <- c("Two", "Three", "Two", "Two", "Three")
BQ3 <- c("Three", "One", "Two", "One", "Three")
BQ4 <- c("Two", "Three", "Three", "Three", "Two")
Bdf <- data.frame(BQ1, BQ2, BQ3, BQ4)


# levelset ----------------------------------------------------------------
#set the factor levels
levelset <- c("One", "Two", "Three")

#Adf <- fct_unify(Adf, levels=levelset)

# Create a function that sets the factor level each variable in the dataset
# Then unifies the factors so that they're all the same number of factors.
factfunc <- function(mydata, factlevel){
  factor(mydata, 
         levels=factlevel, 
         ordered = TRUE)
  fct_unify(mydata, 
            levels=factlevel)
}

#Run this function over each dataset
Adf <- factfunc(Adf, levelset) %>% as.data.frame()
Bdf <- factfunc(Bdf, levelset) %>% as.data.frame()


# nameID Fcn --------------------------------------------------------------

# nameID function adds in a column with an ID number and a group name that is 
# called at the start of the function. 

nameID <- function(mydata, gname = "gname"){
  quo_gname <- enquo(gname)
  mutate(mydata, 
         ID = 1:(nrow(mydata)), 
         Group = rep(!!quo_gname, nrow(mydata))) 
}

Adf <- nameID(Adf, gname = "A") %>% as.data.frame()
Bdf <- nameID(Bdf, gname = "B") %>% as.data.frame()


# join --------------------------------------------------------------------

#Syncrhonize the column names for the join. 
colnames(Adf) <- c("Q1", "Q2", "Q3", "Q4", "ID", "Grp")
colnames(Bdf) <- c("Q1", "Q2", "Q3", "Q4", "ID", "Grp")

# Full join of both datasets
Joindf <- full_join(Adf, Bdf)

#create a new level for new joined dataset to distinguish between groups A and B.  
GrpLevel <- c("B", "A")
Joindf$Grp <- factor(Joindf$Grp, levels=GrpLevel, ordered = TRUE)

#Turn new joined dataset into a likert plot by group
likJoin <- select(Joindf,
                  Q1, Q2, Q3, Q4) %>% as.data.frame()

JoinQs <- likert(likJoin, grouping = Joindf$Grp)
plot(JoinQs)

