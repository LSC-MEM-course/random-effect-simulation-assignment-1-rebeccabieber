library(tidyverse)

n_obs = 1000
beta_0 = 1.5
beta_1 = 3
sigma = 2

x = rnorm(n_obs)
errors = rnorm(n_obs, mean = 0, sd = sigma)

y = beta_0 + beta_1 * x + errors

summary(lm(y~x))

subjects = paste0("s", 1:10)
conditions = 0:1
df = expand.grid(subjects, conditions, stringsAsFactors = FALSE)
colnames(df) = c("subject", "condition")
print(df)

intercept_bysubject_mean = 800
intercept_bysubject_sd = 50

set.seed(84)

#makeing a variable for random effects from the distribution we determined above (800, 50)
ranefs_bysubject = data.frame(subject = subjects, 
                              intercept_bysubject = rnorm(length(subjects), 
                                                          intercept_bysubject_mean, intercept_bysubject_sd),
                              stringsAsFactors = FALSE)

print(ranefs_bysubject)

#join random effects we just made with our dataframe

df = df %>% left_join(ranefs_bysubject, by = "subject")
print(df)

#rounding out the model, simulating response values

beta_1 = 100
sigma = 50

#response vals
df$y = df$intercept_bysubject +  #random intercepts
  beta_1 * df$condition +      #effect of condition (slope*condition)
  rnorm(nrow(df), 0, sigma)     # residual errors

print(df)

library(lme4)

#starting out with a simple model
lm_fit = lm(y~1 + condition, data = df)
lmer_fit = lmer(y~1 + condition + (1|subject), data = df)

summary(lm_fit)
summary(lmer_fit)

library(lmerTest)

#Practice exercise 1
#1 - skipping 1-4 because I don't want to create functions

#5. 
mysleep = sleep
summary(mysleep)

ttest = t.test(extra~group, data = mysleep)
linmod = lm(extra ~ group, data = mysleep)
anovamod = aov(extra~group, data = mysleep)
paired.t = t.test(extra~group, data = mysleep, paired = TRUE)
within.aov = aov(extra~group +  Error(ID), data = mysleep)
mem.mod = lmer(extra~group + (1|ID), data = mysleep)

print(ttest)
summary(linmod)
summary(anovamod)
print(paired.t)
summary(within.aov)
summary(mem.mod)


#the first three are the same, as are the last three. First three suggest no difference in groups, last three find a group effect

#extension exercise 1
rbdata = read.csv(file.choose())
summary(rbdata)
rbdata$Subject = as.factor(rbdata$Subject)

#fit a simple linear model to my data, acknowledging that percent correct is wrong.

mod.simple = lm(PercentCorrect~Block + Condition + Group, data = rbdata)
mod.rand1 = lmer(PercentCorrect ~ Block + Condition + Group + (1|Subject), data = rbdata)

summary(mod.simple)
summary(mod.rand1, corr = FALSE)

###how much do the fixed effect parameters differ?
#estimates are the same 

###do the standard errors of these effects differ?
#I think the standard errors are larger for the model with the random effect?

###estimated standard deviation for the random effect?
#0.1278

####how do you interpret the size of the random effect variance in your data?


###OK, fine, taking the function from the solution file.

simulate_mem_bysubject <- function(beta_0, # still have a single fixed effect
                                          beta_1, 
                                          random_subject_sd, # still have subject ranef sd 
                                          #random_item_sd,    # added item ranef sd
                                          error_sd,
                                          n_subjects) {
                                         #n_items) { # added number of items
  subjects <- paste0("s", 1:n_subjects)
  #items <- paste0("i", 1:n_items)
  conditions <- 0:30
  df <- expand.grid(subjects, conditions, stringsAsFactors = FALSE)
  colnames(df) <- c("subject", "condition")
  
  # still create by-subject random effects table
  ranefs_bysubject <- data.frame(subject = subjects,
                                 intercept_bysubject = rnorm(length(subjects),
                                                             0,
                                                             random_subject_sd),
                                 stringsAsFactors = FALSE)
  
  # add another random effects table, this one by item
  #ranefs_byitem <- data.frame(item = items,
                              #intercept_byitem = rnorm(length(items),
                             #                         0,
                             #                          random_item_sd),
                              #stringsAsFactors = FALSE)
  
  # merge both ranef tables with data
  df <- df %>% left_join(ranefs_bysubject, by = "subject") 
    #left_join(ranefs_byitem, by = "item")
  
  df$y <- (beta_0 + df$intercept_bysubject) + 
             #df$intercept_byitem)   +  # need to add both ranefs to fixed intercept
     beta_1 * df$condition +             # effect of condition
    rnorm(nrow(df), 0, error_sd)        # residual errors
  
  return(df)
}

#ok, trying to simulate something with my data's parameters
#making a new model to fit the function parameters

#beta_1 is the effect of block (not condition, as it's called in the function)

mod.rand2 = lmer(PercentCorrect ~ Block + (1|Subject) , data = rbdata)
summary(mod.rand2)

simdata = simulate_mem_bysubject(beta_0 = 0.59,
                              beta_1 = 0.0025,
                              random_subject_sd = 0.1293,
                              error_sd = 0.3678,
                              n_subjects = 56)

summary(simdata)
ggplot(simdata, aes(condition, y)) + geom_point() + geom_smooth()
ggplot(rbdata, aes(Block, PercentCorrect)) + geom_point() + geom_smooth()


#----------------------------------------
#Practice exercise 2
simulate_mem_bysubject_byitem <- function(beta_0, # still have a single fixed effect
                                          beta_1, 
                                          random_subject_sd, # still have subject ranef sd 
                                          random_item_sd,    # added item ranef sd
                                          error_sd,
                                          n_subjects,
                                          n_items) { # added number of items
  subjects <- paste0("s", 1:n_subjects)
  items <- paste0("i", 1:n_items)
  conditions <- 0:1
  df <- expand.grid(subjects, items, conditions, stringsAsFactors = FALSE)
  colnames(df) <- c("subject", "item", "condition")
  
  # still create by-subject random effects table
  ranefs_bysubject <- data.frame(subject = subjects,
                                 intercept_bysubject = rnorm(length(subjects),
                                                             0,
                                                             random_subject_sd),
                                 stringsAsFactors = FALSE)
  
  # add another random effects table, this one by item
  ranefs_byitem <- data.frame(item = items,
                              intercept_byitem = rnorm(length(items),
                                                       0,
                                                       random_item_sd),
                              stringsAsFactors = FALSE)
  
  # merge both ranef tables with data
  df <- df %>% left_join(ranefs_bysubject, by = "subject") %>%
    left_join(ranefs_byitem, by = "item")
  
  df$y <- (beta_0 + df$intercept_bysubject +
             df$intercept_byitem)   +  # need to add both ranefs to fixed intercept
    beta_1 * df$condition +             # effect of condition
    rnorm(nrow(df), 0, error_sd)        # residual errors
  
  return(df)
}


#2 - 1
small = simulate_mem_bysubject_byitem(beta_0 = 500,
                              beta_1 = 100,
                              random_subject_sd = 150,
                              random_item_sd = 75,
                              error_sd = 50,
                              n_subjects = 3,
                              n_items = 3)
print(small)


large = simulate_mem_bysubject_byitem(beta_0 = 500,
                                      beta_1 = 100,
                                      random_subject_sd = 150,
                                      random_item_sd = 75,
                                      error_sd = 50,
                                      n_subjects = 200,
                                      n_items = 50)
summary(large)
mod2.1 = lmer(y~condition + (1|subject) + (1|item), data = large)
summary(mod2.1)


#extension
mod.rand.cross = lmer(PercentCorrect ~ Block + (1|Subject) + (1|Sentence), data = rbdata)

summary(mod.rand.cross, corr = FALSE)
summary(mod.rand2, corr = FALSE)



#-------------------RANDOM SLOPES---------------------#
beta_0 <- 600
beta_1 <- 75
beta_0_ranef_bysubj_sd <- 100
beta_1_ranef_bysubj_sd <- 25
error_sd <- 50

#creating a dataframe for this, multiple items per subject. 5 items x 2 conditions
subjects <- paste0("s", 1:5)
items <- paste0("i", 1:5)
conditions <- 0:1

df <- expand.grid(subjects, items, conditions,
                  stringsAsFactors = FALSE)
colnames(df) <- c("subject", "item", "condition")
df


