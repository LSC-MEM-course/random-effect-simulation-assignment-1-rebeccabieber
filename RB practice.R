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

