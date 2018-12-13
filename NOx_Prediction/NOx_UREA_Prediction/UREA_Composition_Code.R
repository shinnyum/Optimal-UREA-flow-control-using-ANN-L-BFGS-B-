#install.packages("h2o")
#install.packages("methods")
#install.packages("evd")
#install.packages("xlsx")
library(h2o)
library(methods)
library(evd)
library(readxl)
library(xlsx)

## Read data from Excel worksheets
NOx.Emission_Trainset_NOX  <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)_ver5.xlsx", sheet = "NOX_train")
Modified_Trainset_NOX      <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)_ver5.xlsx", sheet = "Modified_NOX_train")
NOx.Emission_Trainset_UREA <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)_ver5.xlsx", sheet = "UREA_train")
NOx.Emission_Testset       <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)_ver6.xlsx", sheet = "test")

## Calculating average O2
Average.O2_Train_NOX           <- (subset(NOx.Emission_Trainset_NOX, select="O2_D") +
                                   subset(NOx.Emission_Trainset_NOX, select="O2_E") +
                                   subset(NOx.Emission_Trainset_NOX, select="O2_F")) / 3
Average.O2_Modified_Train_NOX  <- (subset(Modified_Trainset_NOX, select="O2_D") +
                                   subset(Modified_Trainset_NOX, select="O2_E") +
                                   subset(Modified_Trainset_NOX, select="O2_F")) / 3
Average.O2_Train_UREA          <- (subset(NOx.Emission_Trainset_UREA, select="O2_D") +
                                   subset(NOx.Emission_Trainset_UREA, select="O2_E") +
                                   subset(NOx.Emission_Trainset_UREA, select="O2_F")) / 3
Average.O2_Test                <- (subset(NOx.Emission_Testset, select="O2_D") +
                                   subset(NOx.Emission_Testset, select="O2_E") +
                                   subset(NOx.Emission_Testset, select="O2_F")) / 3

colnames(Average.O2_Train_NOX)           <- "Average_O2"
colnames(Average.O2_Modified_Train_NOX)  <- "Average_O2"
colnames(Average.O2_Train_UREA)          <- "Average_O2"
colnames(Average.O2_Test)                <- "Average_O2"

NOx.Emission_Trainset_NOX  <- cbind(NOx.Emission_Trainset_NOX[,1:10], Average.O2_Train_NOX)
Modified_Trainset_NOX      <- cbind(Modified_Trainset_NOX[,1:10], Average.O2_Modified_Train_NOX)
NOx.Emission_Trainset_UREA <- cbind(NOx.Emission_Trainset_UREA[,1:10], Average.O2_Train_UREA)
NOx.Emission_Testset       <- cbind(NOx.Emission_Testset[,1:10], Average.O2_Test)

# View(NOx.Emission_Trainset)
# View(NOx.Emission_Testset)

## Input data
train.data.frame          <- NOx.Emission_Trainset_NOX
train.data.frame.Modified <- Modified_Trainset_NOX
test.data.frame.for.LR    <- NOx.Emission_Trainset_UREA
test.data.frame.for.UREA  <- NOx.Emission_Testset

## Construct ANN Model
# source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/NOx_Prediction_Before_UREA_(Linear_Regression).R")
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/NOx_Prediction_ANN_Model_for_LR.R")
## Perform UREA Linear Regression
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/UREA_Linear_Regression.R")
summary(UREA_Flow_LR)
# save.image(file = "D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/Optimal_UREA.RData")

## Number Of Data Set
numOfDataSet <- 1200
## Set Target NOx
Target_NOx <- 50

# load(file = "D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/Optimal_UREA.RData")

## Perform "Optimal UREA Flow Prediction"
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/NOx_Prediction_(Linear_Regression).R")
# source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/NOx_Prediction_ANN_Model_for_UREA.R")
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/Optimal_UREA_Flow_Prediction.R")

