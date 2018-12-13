install.packages("h2o")
install.packages("methods")
install.packages("evd")
library(h2o)
library(methods)
library(evd)
library(readxl)

## Read data from Excel worksheets
Boiler_All_Test_Trainset <- read_excel("D:/work/dataset/Boiler_Controllable.xlsx", sheet = "Train")
Boiler_All_Test_Testset <- read_excel("D:/work/dataset/Boiler_Controllable.xlsx", sheet = "Test")
View(Boiler_All_Test_Trainset)
View(Boiler_All_Test_Testset)

h2o.init()

## Construct ANN Model
source("D:/work/Rcode/BoilerEFF/BoilerEFF_ANN_Model.R")

## Extract ANN's "Closed-form Formula (Feed-forward propagation) Formula"
source("D:/work/Rcode/BoilerEFF/BoilerEFF_Extracted_ANN_ClosedFormFormula.R")

## Perform Non-linear Optimization (L-BFGS-B)
source("D:/work/Rcode/BoilerEFF/BoilerEFF_Optimization(L-BFGS-B).R")

