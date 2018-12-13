#  ==================================================================================
#   * Program Name       	: 발전소 보일러효율향상 UREA LINEAR REGRESSION RSCRIPT
#   * Source File Name   	: UREA_Linear_Regression.R
#   * Author             	: 포스코 안전생산전략실
#   * Version           	: 1.0.0
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-11
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX 추가
#   * Description       	: 
#  ==================================================================================

# -----------------------------------------------------------------------------------
#  NOx Gap 산정
# -----------------------------------------------------------------------------------
NOx_Prediction_Train_Gap            <- (predict.line.for.LR.h2o - NOx.Emission_Trainset_UREA$Revised_NOX)
colnames(NOx_Prediction_Train_Gap)  <- "NOx_Gap"
NOx.Emission_Trainset_UREA_Gap      <- cbind(NOx.Emission_Trainset_UREA, NOx_Prediction_Train_Gap)
NOx.Emission_Trainset_UREA_Gap      <- subset(NOx.Emission_Trainset_UREA_Gap, select = c(-FLUE_GAS_NOX, -Revised_NOX))

# write.xlsx(NOx_Prediction_Train_Gap, "D:/work/dataset/GapGapNOxGap.xlsx")

# -----------------------------------------------------------------------------------
#  Linear Regression
# -----------------------------------------------------------------------------------
UREA_Flow_LR <- lm(UREA_WATER_FLOW_CONTROL ~., data = NOx.Emission_Trainset_UREA_Gap)
summary(UREA_Flow_LR)

print("UREA Linear Regression complete.")

