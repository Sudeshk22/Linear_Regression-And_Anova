#######lab9####
setwd("C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/Linear Regression anova/Lab8-9")
library(readxl)
library(olsrr)

###Question1
hald_C <-read_excel("Hald_Cement_Data.xlsx")
View(hald_C)


model <- lm(Y ~ X1 + X2+ X3+ X4 , data = hald_C)

# Perform best subset selection and calculate R-squared for all subsets
subset_results <- ols_step_all_possible(model)

# Print the results
print(subset_results)

## Best models for 1 , 2 and 3 and 4 variables corresponding 
best_models <- ols_step_best_subset(model)

###Part(c) (mallow cp statistics)
#calculate Mallows' Cp for each model
ols_mallows_cp(model, model)


#part(d) (Forward selection )
FWDfit.p<-ols_step_forward_p(model,steps=2)
print(FWDfit.p)

#part(e) (backward selection)
backward_results <- ols_step_backward_p(model, criterion = "rsquared", steps = 2)

backward_results

#part(f) (subset selction)
# Perform stepwise selection to find the two best models
stepwise_results <- ols_step_best_subset(model, 
                                         criterion = "rsquared", 
                                         direction = "both", 
                                         steps = 2)

# Print the results
print(stepwise_results)


####Question2 

# best two variable selcted model 
model1 <- lm(Y ~ X1 + X2 ,data = hald_C)

# Calculate the PRESS statistic
press_statistics <- ols_press(model1)
print(press_statistics)

# Calculate AIC
aic_value <- AIC(model1)

# Calculate BIC
bic_value <- BIC(model1)




