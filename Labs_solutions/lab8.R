my_dir <- "C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/Linear Regression anova/Lab8-9"
setwd(my_dir)
### question 1 
## Finding the max r square for each of the possible subset model
library(readxl)
library(olsrr)

###Question1
hald_C <-read_excel("Hald_Cement_Data.xlsx")
View(hald_C)
# Read your data from the CSV file into a dataframe (replace "data.csv" with your file path)
#data <- read.csv("data.csv")

# Specify the dependent variable (response) and the list of regressors
response_var <- "Y"
regressors <- c("X", "X2", "X3", "X4")
model <- lm(Y ~ X1 + X2+ X3+ X4 , data = hald_C)

# Calculate the maximum R-squared and the corresponding model
result <- ols_step_all_possible(model)
print(result)

## Best models for 1 , 2 and 3 and 4 variables corresponding 
best_models <- ols_step_best_subset(model)
print(best_models)

### plot between the R square and number of p regressor 
# Assuming you have the "best_models" dataframe from the results of ols_step_best_subset

# Assuming you have a dataframe named bestmodel with columns "rsq" and "n"

# Load the necessary library
library(ggplot2)

# Create a scatter plot
ggplot(data = best_models, aes(x = n, y = rsquare)) +
  geom_point() +
  labs(title = "R-squared vs. Number of Predictors",
       x = "Number of Predictors (n)",
       y = "R-squared (R^2)")
### the suggested best model is at n = 2 as it is an elbow point after that 
### the value of R^2 remains the same 

######################################################
#   Repeating the above using the adjusted R^2 
######################################################
# Calculate the maximum adjusted R-squared and the corresponding model
result <- ols_step_all_possible(model, criteria= "Adj. R-Square")
result

## Best models for 1 , 2 and 3 and 4 variables corresponding 
best_models <- ols_step_best_subset(model, criteria= "Adj. R-Square")
print(best_models)

# Create a scatter plot
ggplot(data = best_models, aes(x = n, y = adjr)) +
  geom_point() +
  labs(title = "R-squared vs. Number of Predictors",
       x = "Number of Predictors (n)",
       y = "Adjuste R-squared (R^2)")
### best suggested model on the basis of adjusted R square is
# with 3 number of predictor. 