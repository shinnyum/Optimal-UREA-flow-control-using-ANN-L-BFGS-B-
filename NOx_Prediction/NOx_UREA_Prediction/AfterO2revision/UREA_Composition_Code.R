#  ==================================================================================
#   * Program Name       	: 발전소 보일러효율향상 COMPOSITION RSCRIPT
#   * Source File Name   	: UREA_Composition_Code.R
#   * Author             	: 포스코 안전생산전략실
#   * Version           	: 1.0.0
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-11
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX 추가
#   * Description       	: 
#  ==================================================================================

# -----------------------------------------------------------------------------------
#  Package 설치
# -----------------------------------------------------------------------------------
# install.packages("h2o")
# install.packages("methods")
# install.packages("evd")
# install.packages("xlsx")

# -----------------------------------------------------------------------------------
# Package 라이브러리 로딩
# -----------------------------------------------------------------------------------
library(h2o)
library(methods)
library(evd)
library(readxl)
library(xlsx)

# -----------------------------------------------------------------------------------
# RData 로딩
# -----------------------------------------------------------------------------------
# load("~/Optimal_UREA.RData")
setwd("D:/work/dataset")

# -----------------------------------------------------------------------------------
# Data Frame 가공
# -----------------------------------------------------------------------------------
# ## Read data from Excel worksheets
NOx.Emission_Trainset_NOX  <- read_excel("./NOx_prediction_(Boiler_9)_ver5_(stack_O2).xlsx", sheet = "NOX_train")
Modified_Trainset_NOX      <- read_excel("./NOx_prediction_(Boiler_9)_ver5_(stack_O2).xlsx", sheet = "Modified_NOX_train")
NOx.Emission_Trainset_UREA <- read_excel("./NOx_prediction_(Boiler_9)_ver5_(stack_O2).xlsx", sheet = "UREA_train")
NOx.Emission_Testset       <- read_excel("./NOx_prediction_(Boiler_9)_ver5_(stack_O2).xlsx", sheet = "test4")

# ## Calculating average O2
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

# NOx revising
NOx.revising_Train          <- (subset(NOx.Emission_Trainset_NOX, select="FLUE_GAS_NOX") * 
                               (21 - 4) / (21 - subset(NOx.Emission_Trainset_NOX, select="STACK_O2")))
NOx.revising_Modified_Train <- (subset(Modified_Trainset_NOX, select="FLUE_GAS_NOX") * 
                               (21 - 4) / (21 - subset(Modified_Trainset_NOX, select="STACK_O2")))
NOx.revising_Train_UREA     <- (subset(NOx.Emission_Trainset_UREA, select="FLUE_GAS_NOX") * 
                               (21 - 4) / (21 - subset(NOx.Emission_Trainset_UREA, select="STACK_O2")))
NOx.revising_Test           <- (subset(NOx.Emission_Testset, select="FLUE_GAS_NOX") * 
                               (21 - 4) / (21 - subset(NOx.Emission_Testset, select="STACK_O2")))

colnames(NOx.revising_Train)           <- "Revised_NOX"
colnames(NOx.revising_Modified_Train)  <- "Revised_NOX"
colnames(NOx.revising_Train_UREA)      <- "Revised_NOX"
colnames(NOx.revising_Test)            <- "Revised_NOX"

NOx.Emission_Trainset_NOX  <- cbind(NOx.Emission_Trainset_NOX[,c(1:11,15)], Average.O2_Train_NOX, NOx.revising_Train)
Modified_Trainset_NOX      <- cbind(Modified_Trainset_NOX[,c(1:11,15)], Average.O2_Modified_Train_NOX, NOx.revising_Modified_Train)
NOx.Emission_Trainset_UREA <- cbind(NOx.Emission_Trainset_UREA[,c(1:11,15)], Average.O2_Train_UREA, NOx.revising_Train_UREA)
NOx.Emission_Testset       <- cbind(NOx.Emission_Testset[,c(1:11,15)], Average.O2_Test, NOx.revising_Test)

# View(NOx.Emission_Trainset)
# View(NOx.Emission_Testset)

# ## Input data
train.data.frame          <- NOx.Emission_Trainset_NOX
train.data.frame.Modified <- Modified_Trainset_NOX
test.data.frame.for.LR    <- NOx.Emission_Trainset_UREA
test.data.frame.for.UREA  <- NOx.Emission_Testset


# -----------------------------------------------------------------------------------
# 정적 모델 실행
# -----------------------------------------------------------------------------------
# ## Construct ANN Model
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/AfterO2revision/NOx_Prediction_ANN_Model_for_LR.R")
# # ## Perform UREA Linear Regression
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/AfterO2revision/UREA_Linear_Regression.R")
#
# ## Number Of Data Set
numOfDataSet <- 3915
# ## Set Target NOx
Target_NOx <- 50

# save.image(file = "./Optimal_UREA.RData")

# -----------------------------------------------------------------------------------
# 동적 모델 실행
# -----------------------------------------------------------------------------------
## Perform "Optimal UREA Flow Prediction"
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/AfterO2revision/NOx_Prediction_(Linear_Regression).R")
# source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/NOx_Prediction_ANN_Model_for_UREA.R")
source("D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/AfterO2revision/Optimal_UREA_Flow_Prediction.R")

result = FINAL_OUTPUT_UREA

