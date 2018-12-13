###########################
## NOx Linear_Regression ##
###########################

train.data.frame.NOx.BeforeUREA_LR <- subset(train.data.frame, select = c(-UREA_WATER_FLOW_CONTROL))
test.data.frame.for.NOx.BeforeUREA_LR <- subset(test.data.frame.for.LR, select = c(-UREA_WATER_FLOW_CONTROL))
       
NOx_Prediction.BeforeUREA_LR <- lm(FLUE_GAS_NOX ~., data = train.data.frame.NOx.BeforeUREA_LR)
predict.line.for.LR.h2o <- as.data.frame(predict(NOx_Prediction.BeforeUREA_LR, newdata = test.data.frame.for.NOx.BeforeUREA_LR))

plot(test.data.frame.for.LR$FLUE_GAS_NOX, type="l", pch=16,
     col="blue", ylab="FLUE_GAS_NOX", xlab=NA, main="Linear Regression", lwd=3)
lines(predict.line.for.LR.h2o, col="red", lwd=2)

print("NOx Linear Regression complete.")

