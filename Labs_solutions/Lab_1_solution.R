install.packages("xlsx")

#library("xlsx")

#Data_exm <- read.xlsx("LRA_22144.xlsx",sheetIndex=1, header=TRUE)
#install.packages("openxlsx")
#library(openxlsx)

#Data_exm <- read.xlsx("C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/Linear Regression anova/LRA_221440.xlsx",sheetIndex=1, header=TRUE )

### Data file import 
#Question1 
install.packages("readxl")
library(readxl)
file_path <- "C:\\Users\\sudesh yadav\\OneDrive - IIT Kanpur\\Desktop\\Linear Regression anova\\LRA_221440.xlsx"
Data_exm <- read_excel(file_path, sheet = "Sheet1")
View(Data_exm)



## printing the Data_exm columna
Data_exm$Y1
Data_exm$X1
Data_exm$X2

##Question2 
# Data Export 
# creating the artificial data by squaring the columns 
Ys <-  Data_exm$Y1^2
X1s <- Data_exm$X1^2
X2s <- Data_exm$X2^2

###

E_Data1 <-  matrix(data=c(Ys, X1s, X2s), nrow=5, ncol=3)
colnames(E_Data1) <- c("Y1s","X1s","X2s")

## Exporting the excel file 
##write.xlsx(E Data 1, ”Path/Lab1 file1 export.xlsx”, sheetName = ”Sheet1”, col.names = TRUE, row.names
# = FALSE, append = FALSE)

#############
# Install and load the openxlsx package
install.packages("openxlsx")
library(openxlsx)

# Replace "Lab1 file1 export.xlsx" with the desired file name
output_file <- "Lab1_file1_export.xlsx"
E_Data1 <- data.frame(Y1s = Ys, X1s = X1s, X2s = X2s)

# Write the data frame to an Excel file
write.xlsx(E_Data1, file = output_file, sheetName = "Sheet1", col.names = TRUE, row.names = FALSE, append = FALSE)



##Question 3 
# Finiding the least Square estimate of the data matrix 
library(pracma)

col1<- matrix(1,nrow=5, ncol=1)
x <- cbind(col1, Data_exm$X1,Data_exm$X2 )


beta_hat <- inv(t(x)%*%x)%*%t(x)%*%Data_exm$Y1

##Question4
lm(Data_exm$Y1~Data_exm$X1+Data_exm$X2)
