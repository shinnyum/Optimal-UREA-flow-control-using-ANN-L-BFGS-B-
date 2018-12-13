install.packages("h2o")
install.packages("methods")
install.packages("evd")
install.packages("xlsx")
library(h2o)
library(methods)
library(evd)
library(readxl)
library(xlsx)

## Read data from Excel worksheets
NOx.Emission_Trainset <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)_ver4.xlsx", sheet = "train")
NOx.Emission_Testset <- read_excel("D:/work/dataset/NOx_prediction_(Boiler_9)_ver4.xlsx", sheet = "test")

## Calculating average O2
Average.O2_Train <- (subset(NOx.Emission_Trainset, select="O2_D") +
                     subset(NOx.Emission_Trainset, select="O2_E") +
                     subset(NOx.Emission_Trainset, select="O2_F")) / 3
Average.O2_Test  <- (subset(NOx.Emission_Testset, select="O2_D") +
                     subset(NOx.Emission_Testset, select="O2_E") +
                     subset(NOx.Emission_Testset, select="O2_F")) / 3
colnames(Average.O2_Train) <- "Average_O2"
colnames(Average.O2_Test) <- "Average_O2"

NOx.Emission_Trainset <- cbind(NOx.Emission_Trainset[,1:10], Average.O2_Train)
NOx.Emission_Testset  <- cbind(NOx.Emission_Testset[,1:10], Average.O2_Test)

# View(NOx.Emission_Trainset)
# View(NOx.Emission_Testset)

## Input data
train.data.frame <- NOx.Emission_Trainset
test.data.frame <- NOx.Emission_Testset

## Construct ANN Model
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/Development Code/Average_O2/BoilerEFF_ANN_Model_NOx.R")

## Extract ANN's "Closed-form Formula (Feed-forward propagation) Formula"
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/Development Code/Average_O2/BoilerEFF_MultiLayer_Extracted_ANN_ClosedFormFormula_NOx.R")
# save.image(file = "D:/work/Rcode/BoilerEFF/NOx_Prediction/Average_O2/BoilerEFF.RData")

## Perform Non-linear Optimization (L-BFGS-B)
numOfDataSet <- 1440 # the number of data set
# load(file = "D:/work/Rcode/BoilerEFF/NOx_Prediction/Average_O2/BoilerEFF.RData")
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/Development Code/Average_O2/BoilerEFF_Optimization(L-BFGS-B)_NOx.R")

