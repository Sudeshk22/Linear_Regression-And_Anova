#install.packages("prospectr")
library(prospectr)
library(olsrr)
setwd("C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/Linear Regression anova/lab10")
TDF_Data_Raw1<-read_excel('Time_Del_Full_Data.xlsx')


X1<-(TDF_Data_Raw1$X1-mean(TDF_Data_Raw1$X1))/(sqrt(39*var(TDF_Data_Raw1$X1)))

X2<-(TDF_Data_Raw1$X2-mean(TDF_Data_Raw1$X2))/(sqrt(39*var(TDF_Data_Raw1$X2)))

TDF_Data_Raw2<-cbind(X1,X2)

Duplex_TDF<-duplex(TDF_Data_Raw2, k = 20, metric = "euclid")

Est_Data_Index<-Duplex_TDF$model

Pred_Data_Index<-Duplex_TDF$test

XE<-TDF_Data_Raw2[Est_Data_Index,]

XP<-TDF_Data_Raw2[Pred_Data_Index,]

eff<-((det((t(XE)%*%XE)))/(det(t(XP)%*%XP)))^(1/3)
eff

Y<-TDF_Data_Raw1[,1]

YE<-Y$Y[Est_Data_Index]

YP<- Y$Y[Pred_Data_Index]

model_X1_X2_E<-lm(YE~XE[,1]+XE[,2])

model_X1_X2_P<-lm(YP~XP[,1]+XP[,2])

summary(model_X1_X2_E)

summary(model_X1_X2_P)

model_X1_E<-lm(YE~XE[,1])

model_X1_P<-lm(YP~XP[,1])

summary(model_X1_E)

summary(model_X1_P)

model<-lm(Y~X1+X2, data = TDF_Data_Raw1)

ols_step_best_subset(model)

model_X1<-lm(Y~X1, data = TDF_Data_Raw1)

print(model_X1)
