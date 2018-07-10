#How to stack likerts into one column for analysis 

# Data --------------------------------------------------------------------

PreQ1 <- c("Strongly Agree", "Disagree", "Strongly Disagree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Strongly Disagree", "Disagree", "Agree", "Agree", "Strongly Disagree", "Agree", "Disagree", "Agree", "Agree", "Disagree", "Disagree","Agree")

PreQ2 <- c("Agree", "Strongly Disagree", "Strongly Disagree", "Disagree", "Agree", "Disagree", "Agree", "Agree", "Disagree", "Disagree", "Agree", "Agree", "Strongly Disagree", "Disagree", "Disagree", "Disagree", "Agree", "Agree", "Strongly Agree", "Agree")

PreQ3 <- c("Agree", "Strongly Disagree", "Strongly Disagree", "Disagree", "Agree", "Disagree", "Agree", "Agree", "Disagree", "Disagree", "Agree", "Agree", "Strongly Disagree", "Disagree", "Disagree", "Disagree", "Agree", "Agree", "Strongly Agree", "Agree")
predf <- data.frame(PreQ1, PreQ2, PreQ3)

PostQ1 <- c("Strongly Agree", "Agree", "Agree", "Disagree", "Strongly Agree", "Strongly Agree", "Strongly Agree", "Disagree", "Strongly Agree", "Disagree", "Agree", "Agree", "Disagree", "Strongly Agree", "Agree", "Agree", "Strongly Disagree", "Disagree", "Disagree","Agree")

PostQ2 <- c("Strongly Disagree", "Agree", "Agree", "Disagree", "Strongly Agree", "Strongly Agree", "Strongly Disagree", "Disagree", "Strongly Agree", "Disagree", "Agree", "Strongly Agree", "Disagree", "Strongly Agree", "Agree", "Agree", "Strongly Disagree", "Agree", "Strongly Agree", "Agree")

PostQ3 <- c("Agree", "Agree", "Agree", "Disagree", "Strongly Agree", "Strongly Agree", "Agree", "Disagree", "Strongly Agree", "Disagree", "Agree", "Agree", "Disagree", "Agree", "Agree", "Agree", "Strongly Disagree", "Agree", "Strongly Agree", "Agree")

postdf <- data.frame(PostQ1, PostQ2, PostQ3)


# Level -------------------------------------------------------------------

agreelevel <- c("Strongly Disagree", "Disagree", "Agree","Strongly Agree")

#factfunc <- function(mydata, factlevel){
#  factor(mydata, 
#         levels=factlevel, 
#         ordered=TRUE, 
#         exclude = NA)
#  fct_unify(mydata,
#            levels=factlevel)
#}

# Analysis ----------------------------------------------------------------


bmd.datastack <- function(olddf, factlevel, valuecol = "Prevar", key = "Question") {
  olddf <- olddf %>% 
    gather(key = "Question", value = valuecol)
  olddf$valuecol <- 
    factor(olddf$valuecol, level = factlevel, 
           ordered = TRUE, 
           exclude = NA)
  return(olddf)
}



bmd.datastack <- function(olddf, factlevel, valuecol = "Prevar", key = "Question") {
  quo_valuecol <- enquo(valuecol)
  olddf <- olddf %>% 
    gather(key = "Question", value = !!quo_valuecol ) %>%
    mutate("factorcol" = factor(!!quo_valuecol))
  return(olddf)
}

postdf_stack <- bmd.datastack(postdf, agreelevel, Prevar)







d <- postdf %>% 
  gather(key = "Question", value = "PreVar")
d$PreVar <- 
  factor(d$PreVar, level = agreelevel, 
         ordered = TRUE, 
         exclude = NA)  
d <- d %>%  
  select(-Question)