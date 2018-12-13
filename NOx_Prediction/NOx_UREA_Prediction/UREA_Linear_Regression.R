############################
## UREA Linear_Regression ##
############################

NOx_Prediction_Train_Gap            <- (predict.line.for.LR.h2o - NOx.Emission_Trainset_UREA$FLUE_GAS_NOX)
colnames(NOx_Prediction_Train_Gap)  <- "NOx_Gap"
NOx.Emission_Trainset_UREA_Gap      <- cbind(NOx.Emission_Trainset_UREA, NOx_Prediction_Train_Gap)
NOx.Emission_Trainset_UREA_Gap      <- subset(NOx.Emission_Trainset_UREA_Gap, select = c(-FLUE_GAS_NOX))

UREA_Flow_LR <- lm(UREA_WATER_FLOW_CONTROL ~., data = NOx.Emission_Trainset_UREA_Gap)
summary(UREA_Flow_LR)

print("UREA Linear Regression complete.")

