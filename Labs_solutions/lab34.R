#Question 1
y = c(1, 4, 8, 9, 3, 8, 9)
x1 = c(-1, 1, -1, 1, 0, 0, 0)
x2 = c(-1, -1, 1, 1, 0, 1, 2)

library(Matrix)

A = matrix(c(1,0,0,1,0,-1,0,0),nrow=2)
q = rankMatrix(A)[1]

C = rep(0,2) 

model = summary(lm(y~x1+x2+I(x1^2)))

n = length(y)
p = 4

sigma2.hat = sum(model$residuals^2)/n

X = cbind(1,x1,x2,x1^2)

beta = model$coefficients[,1]
beta.r = beta + solve(crossprod(X)) %*% t(A) %*% solve(A %*% solve(crossprod(X)) %*% t(A)) %*% (C - A %*% beta)

sigmar2.hat = sum((y - X %*% beta.r)^2)/n

lambda = (sigma2.hat/sigmar2.hat)^(n/2)

F.stat = (n-p)/q * (lambda^(-2/n)-1)

F.crit = qf(0.95,q,n-p)

library(car)
linearHypothesis(lm(y~x1+x2+I(x1^2)), hypothesis.matrix = A)
#===================

#Question 2
A = matrix(c(0,0,0,1,0,0,0,1,0,0,0,1),nrow=3)
q = rankMatrix(A)[1]

C = rep(0,3) 

model = summary(lm(y~x1+x2+I(x1^2)))

n = length(y)
p = 4

sigma2.hat = sum(model$residuals^2)/n

X = cbind(1,x1,x2,x1^2)

beta = model$coefficients[,1]
beta.r = beta + solve(crossprod(X)) %*% t(A) %*% solve(A %*% solve(crossprod(X)) %*% t(A)) %*% (C - A %*% beta)

sigmar2.hat = sum((y - X %*% beta.r)^2)/n

lambda = (sigma2.hat/sigmar2.hat)^(n/2)

F.stat = (n-p)/q * (lambda^(-2/n)-1)
#===================================


#Question 3

x = c(1,1,2,3.3,3.3,4,4,4,4.7,5,5.6,5.6,5.6,6,6,6.5,6.9)
y = c(10.84,9.3,16.35,22.88,24.35,24.56,25.86,29.16,24.59,22.25,25.9,27.2,25.61,25.45,26.56,21.03,21.46)

library(olsrr)

ols_pure_error_anova(model = lm(y~x))
#====================================

#Question 4
library(readxl)
data = read_excel('RocketData.xlsx')

#bonferroni
model = summary(lm(Y~X, data = data))
n = nrow(data)
p = 2
k = 2
alpha = 0.05
cval = qt(1-alpha/(2*k), n-p)
beta = model$coefficients[,1]
beta.se = model$coefficients[,2]

beta0.lcl = beta[1] - cval * beta.se[1]
beta0.ucl = beta[1] + cval * beta.se[1]

beta1.lcl = beta[2] - cval * beta.se[2]
beta1.ucl = beta[2] + cval * beta.se[2]

#maximum modulus



#scheff's method
