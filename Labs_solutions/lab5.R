getwd()
mydir <- "C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/Linear Regression anova/lab5"
setwd(mydir)
library(readxl)

### Question1 

#a) residual plot 
elec <-read_excel("Electricity_Data.xlsx")
View(elec)

#fit a regression model
model1 <- lm(elec$Y~elec$X)

#get list of residuals 
res <- resid(model1)

#produce residual vs. fitted plot
plot(fitted(model1), res)

#add a horizontal line at 0 
abline(0,0)

#b) Applying boxcox transformation on response variable 
library(MASS)
response_variable <- elec$Y
boxcox_results <- boxcox(response_variable ~ elec$X)
lambda <- boxcox_results$x[which.max(boxcox_results$y)]

# Print the lambda value
print(lambda)

# Transform the response variable
transformed_response <- (response_variable^lambda - 1) / lambda
transformed_response 


#C) fitting the model after transformation 
tranformed_model <- lm(transformed_response~ elec$X)
tranformed_model


##################################################
##################################################
#Question 2 

wind <-read_excel("Wind_Mill_Data.xlsx")
View(wind)
## fiting the linear regression model on windmill dataset 
#fit a regression model
model2 <- lm(wind$Y~wind$X)

#get list of residuals 
res <- resid(model2)

#produce residual vs. fitted plot
plot(fitted(model2), res)

#add a horizontal line at 0 
abline(0,0)

#B) Applying boxtidwell transformation on the response variable 



#c)fitting the tranformed response variable on regressor of wind_mill dataset 




###################################################
###################################################
#Question 3 
weighted_LS <-read_excel("Weighted_Least_Squares_Data.xlsx")
View(weighted_LS)
#a)
#fit a regression model
model3 <- lm(weighted_LS$Y~weighted_LS$X)

#get list of residuals 
res <- resid(model3)

#produce residual vs. fitted plot
plot(fitted(model3), res)

#add a horizontal line at 0 
abline(0,0)



#b) Create subsets based on the regressor values
group_1 <- subset(weighted_LS, weighted_LS$X >= 1 & weighted_LS$X <= 2.00)
group_2 <- subset(weighted_LS, weighted_LS$X > 2.00 & weighted_LS$X <= 3.00)
group_3 <- subset(weighted_LS, weighted_LS$X > 3.00 & weighted_LS$X <= 6.00)
group_4 <- subset(weighted_LS, weighted_LS$X > 6.00 & weighted_LS$X <= 8.00)
group_5 <- subset(weighted_LS, weighted_LS$X > 8.00 & weighted_LS$X <= 10.00)
group_6 <- subset(weighted_LS, weighted_LS$X > 10.00 & weighted_LS$X <= 12.00)

#C) computing the mean and variance of each group of the dataset
# Compute sample mean and variance for each group
mean_1 <- mean(group_1$Y)
variance_1 <- var(group_1$Y)

mean_2 <- mean(group_2$Y)
variance_2 <- var(group_2$Y)

mean_3 <- mean(group_3$Y)
variance_3 <- var(group_3$Y)

mean_4 <- mean(group_4$Y)
variance_4 <- var(group_4$Y)

mean_5 <- mean(group_5$Y)
variance_5 <- var(group_5$Y)
 
mean_6 <- mean(group_6$Y)
variance_6 <- var(group_6$Y)





# Print the results
cat("Group 1: Mean =", mean_1, ", Variance =", variance_1, "\n")
cat("Group 2: Mean =", mean_2, ", Variance =", variance_2, "\n")
cat("Group 3: Mean =", mean_3, ", Variance =", variance_3, "\n")
cat("Group 4: Mean =", mean_4, ", Variance =", variance_4, "\n")
cat("Group 5: Mean =", mean_5, ", Variance =", variance_5, "\n")
cat("Group 6: Mean =", mean_6, ", Variance =", variance_6, "\n")

##scatter plots of the groups 
# Create scatter plots for each group
par(mfrow = c(2, 3))  # Set up a 3x2 grid of plots

# Scatter plot for Group 1
plot(group_1$X, group_1$Y, 
     main = "Group 1 Scatter Plot", 
     xlab = "Regressor Variable", 
     ylab = "Response Variable")

# Scatter plot for Group 2
plot(group_2$X, group_2$Y, 
     main = "Group 2 Scatter Plot", 
     xlab = "Regressor Variable", 
     ylab = "Response Variable")

# Scatter plot for Group 3
plot(group_3$X, group_3$Y, 
     main = "Group 3 Scatter Plot", 
     xlab = "Regressor Variable", 
     ylab = "Response Variable")


# Scatter plot for Group 4
plot(group_4$X, group_4$Y, 
     main = "Group 4 Scatter Plot", 
     xlab = "Regressor Variable", 
     ylab = "Response Variable")

# Scatter plot for Group 5
plot(group_5$X, group_5$Y, 
     main = "Group 5 Scatter Plot", 
     xlab = "Regressor Variable", 
     ylab = "Response Variable")

# Scatter plot for Group 6
plot(group_6$X, group_6$Y, 
     main = "Group 6 Scatter Plot", 
     xlab = "Regressor Variable", 
     ylab = "Response Variable")


# Reset the plot layout
par(mfrow = c(1, 1))

##D) 
# Fit a linear regression model for Group 1
linear_model_1 <- lm(group_1$Y ~ group_1$X, data = group_1)

# Summary of the linear model
summary(linear_model_1)


# Fit a linear regression model for Group 2
linear_model_2 <- lm(group_2$Y ~ group_2$X, data = group_2)

# Summary of the linear model
summary(linear_model_2)

# Fit a linear regression model for Group 3
linear_model_3 <- lm(group_3$Y ~ group_3$X, data = group_3)

# Summary of the linear model
summary(linear_model_3)

# Fit a linear regression model for Group 4
linear_model_4 <- lm(group_4$Y ~ group_4$X, data = group_4)

# Summary of the linear model
summary(linear_model_4)

# Fit a linear regression model for Group 5
linear_model_5 <- lm(group_5$Y ~ group_5$X, data = group_5)

# Summary of the linear model
summary(linear_model_5)

# Fit a linear regression model for Group 6
linear_model_6 <- lm(group_6$Y ~ group_6$X, data = group_6)

# Summary of the linear model
summary(linear_model_6)

#e) 
# Compute fitted values for the linear model using original regressor values
fitted_linear_1 <- predict(linear_model_1, newdata = data.frame(regressor_var = group_1$X))
fitted_linear_2 <- predict(linear_model_2, newdata = data.frame(regressor_var = group_2$X))
fitted_linear_3 <- predict(linear_model_3, newdata = data.frame(regressor_var = group_3$X))


fitted_linear_4 <- predict(linear_model_4, newdata = data.frame(regressor_var = group_4$X))
fitted_linear_5 <- predict(linear_model_5, newdata = data.frame(regressor_var = group_5$X))
fitted_linear_6 <- predict(linear_model_6, newdata = data.frame(regressor_var = group_6$X))

#f)
# Compute reciprocal weights from the fitted values
weights_linear_1 <- 1 / fitted_linear_1
weights_linear_2 <- 1 / fitted_linear_2
weights_linear_3 <- 1 / fitted_linear_3

weights_linear_4 <- 1 / fitted_linear_4
weights_linear_5 <- 1 / fitted_linear_5
weights_linear_6 <- 1 / fitted_linear_6

# Check for and handle negative or zero weights by taking the absolute value
weights_linear_1[weights_linear_1 <= 0] <- abs(weights_linear_1[weights_linear_1 <= 0])
weights_linear_2[weights_linear_2 <= 0] <- abs(weights_linear_2[weights_linear_2 <= 0])
weights_linear_3[weights_linear_3 <= 0] <- abs(weights_linear_3[weights_linear_3 <= 0])
weights_linear_4[weights_linear_4 <= 0] <- abs(weights_linear_4[weights_linear_4 <= 0])
weights_linear_5[weights_linear_5 <= 0] <- abs(weights_linear_5[weights_linear_5 <= 0])
weights_linear_6[weights_linear_6 <= 0] <- abs(weights_linear_6[weights_linear_6 <= 0])

# Fit the weighted least squares model
wls_model_linear_1 <- lm(Y ~ X, 
                         data = group_1, 
                         weights = weights_linear_1)
wls_model_linear_2 <- lm(Y ~ X, 
                         data = group_2, 
                         weights = weights_linear_2)
wls_model_linear_3 <- lm(Y ~ X, 
                         data =group_3, 
                         weights = weights_linear_3)
wls_model_linear_4 <- lm(Y ~ X, 
                         data =group_4, 
                         weights = weights_linear_4)
wls_model_linear_5 <- lm(Y ~ X, 
                         data =group_5, 
                         weights = weights_linear_5)

wls_model_linear_6 <- lm(Y ~ X, 
                         data =group_6, 
                         weights = weights_linear_6)
# Create a residual plot for each WLS model
par(mfrow = c(2, 3))  # Set up a 1x3 grid of plots

# Residual plot for Group 1
#produce residual vs. fitted plot
plot(fitted(wls_model_linear_1), resid(wls_model_linear_1),main = "Residual Plot - Group 1")



# Residual plot for Group 2
plot(fitted(wls_model_linear_2),resid(wls_model_linear_2), main = "Residual Plot - Group 2")

# Residual plot for Group 3
plot(fitted(wls_model_linear_3), resid(wls_model_linear_3), main = "Residual Plot - Group 3")

# Residual plot for Group 4
plot(fitted(wls_model_linear_4), resid(wls_model_linear_4), main = "Residual Plot - Group 4")

# Residual plot for Group 5
plot(fitted(wls_model_linear_5), resid(wls_model_linear_5), main = "Residual Plot - Group 5")

# Residual plot for Group 6
plot(fitted(wls_model_linear_6), resid(wls_model_linear_6), main = "Residual Plot - Group 6")

# Reset the plot layout
par(mfrow = c(1, 1))





