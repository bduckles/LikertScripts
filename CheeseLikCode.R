#This is a way to think about combining multiple likert questions into groupings of questions and stacking them/graphing them. 

# Setup -------------------------------------------------------------------
library(tidyverse)
library(likert)
library(ggplot2)
library(forcats)

# Data --------------------------------------------------------------------

LikeCheesePreQ <- c("Strongly Agree", "Strongly Disagree", "Strongly Disagree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Strongly Disagree", "Disagree", "Agree", "Agree", "Strongly Disagree", "Agree", "Disagree", "Agree", "Agree", "Disagree", "Disagree","Agree")
LikeCheesePostQ <- c("Strongly Agree", "Agree", "Agree", "Disagree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Disagree", "Strongly Agree", "Disagree", "Agree", "Agree", "Disagree", "Strongly Agree", "Agree", "Agree", "Strongly Disagree", "Agree", "Strongly Agree", "Agree")
CheeseGoodPreQ <- c("Disagree", "Strongly Disagree", "Strongly Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Strongly Disagree", "Agree", "Agree", "Disagree", "Strongly Disagree", "Agree", "Disagree", "Agree", "Agree", "Strongly Disagree", "Disagree","Strongly Agree")
CheeseGoodPostQ <- c("Agree", "Disagree", "Agree", "Disagree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Disagree", "Strongly Agree", "Disagree", "Agree", "Agree", "Agree", "Strongly Agree", "Disagree", "Agree", "Strongly Disagree", "Agree", "Strongly Disagree", "Disagree")
AmazeCheesePreQ <- c("Agree", "Strongly Agree", "Strongly Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Disagree", "Agree", "Agree", "Disagree", "Strongly Disagree", "Disagree", "Disagree", "Agree", "Agree", "Strongly Disagree", "Disagree","Disagree")
AmazeCheesePostQ <- c("Agree", "Agree", "Agree", "Disagree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Agree", "Strongly Agree", "Disagree", "Agree", "Agree", "Agree", "Agree", "Disagree", "Agree", "Strongly Disagree", "Agree", "Agree", "Agree")

pre_df <- data.frame(LikeCheesePreQ, CheeseGoodPreQ,AmazeCheesePreQ)
post_df <- data.frame(LikeCheesePostQ, CheeseGoodPostQ, AmazeCheesePostQ)


LikeCheesePreQ <- factor(LikeCheesePreQ)
LikeCheesePostQ <- factor(LikeCheesePostQ)
CheeseGoodPreQ <- factor(CheeseGoodPreQ)
CheeseGoodPostQ <- factor(CheeseGoodPostQ)
AmazeCheesePreQ <- factor(AmazeCheesePreQ)
AmazeCheesePostQ <- factor(AmazeCheesePostQ)


agreelevel <- c("Strongly Disagree", "Disagree", "Agree","Strongly Agree")

factfunc <- function(mydata, factlevel){
  factor(mydata, 
         levels=factlevel, 
         ordered=TRUE, 
         exclude = NA)
  fct_unify(mydata,
            levels=factlevel)
}

pre_df <- factfunc(pre_df, agreelevel) %>% as.data.frame()
post_df <- factfunc(post_df, agreelevel) %>% as.data.frame()

# Now stack the pre and post questions on top of each other. 

pre_stk <- stack(pre_df)

stack(pre_df, select = c("LikeCheesePreQ", "CheeseGoodPreQ", "AmazeCheesePreQ"))

      poststack <- stack(LikeCheesePostQ, CheeseGoodPostQ, AmazeCheesePostQ)






names(dfPrePost) <- c(
  PreQ="1 Pre Workshop Questions",
  PostQ="2 Post Workshop Questions")

#Make it pretty with better colors! 
BlueFour <- c('#eff3ff', '#bdd7e7', '#6baed6', '#2171b5')


# Plot --------------------------------------------------------------------
#Plot 
Title <- "Likert Data Title"
d <- likert(items = dfPrePost, grouping=df$Gender)
plot(d, 
     colors = BlueFour) + ggtitle(Title)

