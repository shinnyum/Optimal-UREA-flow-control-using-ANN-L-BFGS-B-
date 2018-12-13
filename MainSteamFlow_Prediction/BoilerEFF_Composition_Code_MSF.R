install.packages("h2o")
install.packages("methods")
install.packages("evd")
library(h2o)
library(methods)
library(evd)
library(readxl)

## Read data from Excel worksheets
Boiler8_Trainset <- read_excel("D:/work/dataset/boiler8.xlsx", sheet = "Sheet1")
Boiler8_Testset <- read_excel("D:/work/dataset/boiler8.xlsx", sheet = "Sheet2")
View(Boiler8_Trainset)
View(Boiler8_Testset)

h2o.init()

## Construct ANN Model
source("D:/work/Rcode/BoilerEFF/MainSteamFlow_Prediction/BoilerEFF_ANN_Model_MSF.R")

## Extract ANN's "Closed-form Formula (Feed-forward propagation) Formula"
source("D:/work/Rcode/BoilerEFF/MainSteamFlow_Prediction/BoilerEFF_Extracted_ANN_ClosedFormFormula_MSF.R")

## Perform Non-linear Optimization (L-BFGS-B)
source("D:/work/Rcode/BoilerEFF/MainSteamFlow_Prediction/BoilerEFF_Optimization(L-BFGS-B)_MSF.R")

