library(readxl)
#nstall.packages("openxlsx")
getwd()
my_dir <- "/users/math/msc/sudeshk22/Downloads"
setwd(my_dir)
library(openxlsx)
#file_path <- "/users/math/msc/sudeshk22/Downloads/RocketData"
#Problem 1
rocket_D <- read_excel("RocketData.xlsx")
getwd()
View(rocket_D)
Y<- rocket_D$Y
x <- rocket_D$X

# fitting the linear model 
model1 <- lm(Y~x)
summary(model1)

#problem2
TimeDelivery_D <- read_excel("TimeDeliveryData.xlsx")
Y <- TimeDelivery_D$Y
x1 <- TimeDelivery_D$X1
x2 <- TimeDelivery_D$X2

#fitting the linear model 
model2 <- lm(Y~ x1+x2)
summary(model2)

#problem 3
x <- cbind(1,rocket_D$X)
Y1 <- rocket_D$Y
beta_hat1 <- c( 2627.822, -37.154)
y_pred1 <- x%*% beta_hat1
SSE1 <- t(Y1-y_pred1)%*% (Y1-y_pred1)
# unbiased estimator of sigma 
u_est_sigma <- SSE1/(20-2)
print(u_est_sigma)

sigma <- 