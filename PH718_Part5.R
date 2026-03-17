# Single imputation

# Illustrative example: airquality
## Preprocessing
old_df <- airquality
View(old_df)
old_df$lnOzone = log(old_df$Ozone)
colSums(is.na(old_df))

# Locations of missing values
missing_ozone <- which(is.na(old_df$Ozone))
missing_solar <- which(is.na(old_df$Solar.R))

## Simple imputation - To check if there's no empty data for the regression model. If we do not have missing values, no need to do imputation at all.
old_df$Ozone[missing_ozone] <- mean(old_df$Ozone, na.rm = TRUE)
old_df$Solar.R[missing_solar] <- mean(old_df$Solar.R, na.rm = TRUE)

## Set convergence criteria
tolerance <- 1e-4 # 10^-4
max_iterations <- 200

## Iterative regression
for (i in 1:max_iterations) {
  if (i==1) {
    curr_df <- old_df
  }else{
    curr_df <- next_df
  }

  #### Regressions
  model_ozone <- lm(
    lnOzone ~ Solar.R + Wind + Temp + as.factor(Month) + as.factor(Day), data = curr_df
  )
  model_solar <- lm(
    Solar.R ~ Ozone + Wind + Temp + as.factor(Month) + as.factor(Day), data = curr_df
  )
  
  #### Updating data
  next_df = curr_df # intialization of next_df
  next_df$Ozone[missing_ozone] <- exp(predict(model_ozone, newdata = curr_df[missing_ozone, ])) # we're gonna just update missing values
  next_df$lnOzone = log(next_df$Ozone)
  
  next_df$Solar.R[missing_solar] <- predict(model_solar, newdata = curr_df[missing_solar, ])
  
  #### Check the mean squared difference
  diff <- sum((curr_df$Ozone - next_df$Ozone)^2 + (curr_df$Solar.R - next_df$Solar.R)^2) # sum is more restrictive compared to mean
  # cat("Iteration:", i, "Difference:", diff, "\n")
  if(diff < tolerance) {
    cat("Convergence achieved at iteration", i, "th iteration")
    break
  }
}

# Multiple imputation

# Illustrative example: airquality
## Using mice::mice() with default Bayesian regression models:
### Predictive mean matching (pmm, for numeric data)
### logistic regression (logreg, for factors with 2 levels)
### polytomous regression (polyreg, for unordered factor > 2 levels)
### proportional odds model for (polr, ordered factors > 2 levels)
library(mice)
old_df = airquality
summary(old_df)
# colSums(is.na(old_df))
imp_obj <- mice(
  old_df,
  m = 2, # number of imputed data sets, ppl prefer 5 for some reason
  maxit = 200,
  method = c("pmm","pmm","", "", "", ""),
  seed = 123
)

## Check convergence visually
### Each trajectory should stabilize and mix well over iterations
### No upward/downward trends = good convergence.
### Wiggly lines or divergence = possible convergence issues.
plot(imp_obj)
## Extracting imputed data
for (i in 1:imp_obj$m) {
  data_name <- paste0("data_imp_", i)
  assign(data_name, complete(imp_obj, i))
}
plot(imp_obj) # Check convergence again after extracting data

for (i in 1:imp_obj$m) {
  data_names <- paste0("data_imp_", i) # generating names of data
  assign(data_names, complete(imp_obj, i))
}

summary(data_imp_2) # Check the random imputed data

## Model fitting with imputed data - Analyze each imputed dataset separatelyz
fit <- with(
  data = imp_obj,
  exp = lm(log(Ozone) ~ log(Solar.R) + Wind + Temp + Month + Day)
)

# Pool the results using Rubin's rules
pooled <- pool(fit, rule = "rubin1987")
summary(pooled, conf.int = T) # Summarize the pooled result

# Illustrative example: mice::nhanes2
library(mice)
old_df <- nhanes2
colSums(is.na(nhanes2))
imp_obj <- mice(
  nhanes2,
  m = 5,
  maxit = 200,
  method = c("", "pmm", "logreg", "pmm"),
  # Predictive mean matching (pmm, for continuous data)
  # logistic regression (logreg, for factors with 2 levels)
  # polytomous regression (polyreg, for unordered factor > 2 levels)
  # proportional odds model for (polr, ordered factors > 2 levels)
  seed = 123
)

plot(imp_obj)
for (i in 1:imp_obj$m) {
  data_names <- paste0("data_imp_", i)
  assign(data_names, complete(imp_obj, i))
}

fit <- with(data = imp_obj, exp = lm(chl ~ age + bmi + hyp))
pooled <- pool(fit, rule = "rubin1987")
summary(pooled, conf.int = T)