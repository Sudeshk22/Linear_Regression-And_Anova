library(readxl)
#install.packages("openxlsx")
getwd()
my_dir <- "/users/math/msc/sudeshk22/Desktop/Linear_Regression_Anova"

setwd(my_dir)
library(openxlsx)
#file_path <- "/users/math/msc/sudeshk22/Downloads/RocketData"
#Problem 1
time_D <- read_excel("TimeDeliveryData .xlsx")
x1 <- time_D$X1
x2 <- time_D$X2
Y <- time_D$Y
model1 <- lm(Y~ x1+x2)
#hats1 <- as.data.frame(hatvalues(model1))
hats1 <- as.matrix(hatvalues((model1)))
diag(hats1)
leverage_points <- diag(hats1)
############
#b) cooks distance
cooks_distance <- cooks.distance(model1)
df <- as.data.frame(cooks_distance)

ifelse(df$cooks_distance>1,"Yes","No")
## 9th point is the influence point

#########
#c) DFBETAS
dfbetas <- as.data.frame(dfbetas(model1))
n <- 25
#calculate DFBETAS threshold value
thresh <- 2/sqrt(n)
#specify 2 rows and 1 column in plotting region
par(mfrow=c(2,1))

#plot DFBETAS for disp with threshold lines
plot(dfbetas$x1, type='h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

#plot DFBETAS for x2 with threshold lines
plot(dfbetas$x2, type='h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

#