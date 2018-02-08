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
 NOx.Emission_Trainset <- read_excel("D:/work/dataset/NOx_prediction.xlsx", sheet = "train")
 NOx.Emission_Testset <- read_excel("D:/work/dataset/NOx_prediction.xlsx", sheet = "test")
 View(NOx.Emission_Trainset)
 View(NOx.Emission_Testset)

 h2o.init()

 ## Construct ANN Model
 source("D:/work/Rcode/BoilerEFF/NOx_Prediction/BoilerEFF_ANN_Model_NOx.R")

 ## Extract ANN's "Closed-form Formula (Feed-forward propagation) Formula"
 source("D:/work/Rcode/BoilerEFF/NOx_Prediction/BoilerEFF_MultiLayer_Extracted_ANN_ClosedFormFormula_NOx.R")

 ## Perform Non-linear Optimization (L-BFGS-B)
 numOfDataSet <- 750 # the number of data set 
 source("D:/work/Rcode/BoilerEFF/NOx_Prediction/BoilerEFF_Optimization(L-BFGS-B)_NOx.R")

 
