###########################
## NOx Linear_Regression ##
###########################

train.data.frame.NOx_LR <- subset(train.data.frame.Modified, select = c(-UREA_WATER_FLOW_CONTROL))
test.data.frame.for.NOx_LR <- subset(test.data.frame.for.UREA, select = c(-UREA_WATER_FLOW_CONTROL))
       
NOx_Prediction_LR <- lm(FLUE_GAS_NOX ~., data = train.data.frame.NOx_LR)
predict.line.for.NOx.LR <- as.data.frame(predict(NOx_Prediction_LR, newdata = test.data.frame.for.NOx_LR))

plot(test.data.frame.for.NOx_LR$FLUE_GAS_NOX, type="l", pch=16,
     col="blue", ylab="FLUE_GAS_NOX", xlab=NA, main="Linear Regression", lwd=3)
lines(predict.line.for.NOx.LR, col="red", lwd=2)

print("NOx Linear Regression complete.")

