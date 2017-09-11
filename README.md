# bstsTest
Rapidly Test BSTS Models in R

# Installation
```R
require(devtools)
devtools::install_github('lissahyacinth/bstsTest')
```

# Usage
```R
set.seed(10)
library(bstsTest)
library(data.table)

bstsData <- melt(
                    data.table("London" = ts(rnorm(100) + 10,frequency = 7, start = Sys.Date()), 
                    "France" =  ts(rnorm(100) + 5,frequency = 7, start = Sys.Date()), 
                    "Manchester" =  ts(rnorm(100) +2,frequency = 7, start = Sys.Date()), 
                   "Date" = as.Date(1:100, origin = "2001-01-01")),
               measure.vars = c("London", "France", "Manchester"),
               variable.name = "City",
               value.name = "Sessions")

x = bsts_test(df = bstsData, 
              date_variable = "Date", 
              validate_n = 14, 
              na_fill = 0,
              model.options = bsts::BstsOptions(),
              response = "London", 
              group_variable = "City", 
              target_variable = "Sessions",
              nseasons = 28,
              niter = 1000,
              rebag_vars = T,
              rebag_mean_vars = T,
              inclusion_probability = 0.1)

# Mean Absolute Percentage Error and Root Mean Square Error are now visible on a 14 day cross validation. 
x$MAPE
> 0.12
x$RMSE
> 1.438
```

# Features
Rebag is a way of aggregating small predictors into one big predictor. It's sometimes helpful, and sometimes a hinderance when there's strong correlation between rebag and some of your core predictors. It's likely to get an exclusion list at some point in the future when you need to test just *some* predictors. 
Rebag comes in two forms - the aggregate sum, and the mean - I found adding the mean weakens the test case, but it can have some applications. 
