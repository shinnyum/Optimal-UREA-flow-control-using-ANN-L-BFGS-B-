#  ==========================================================================================
#   * Program Name       	: Î∞úÏ†Ñ?Üå Î≥¥Ïùº?ü¨?ö®?ú®?ñ•?ÉÅ NOx Prediction (Linear Regression) SCRIPT
#   * Source File Name   	: NOx_Prediction_(Linear_Regression).R
#   * Author             	: ?è¨?ä§ÏΩ? ?ïà?†Ñ?Éù?Ç∞?†Ñ?ûµ?ã§
#   * Version           	: 1.1.5
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-15
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX Ï∂îÍ?Ä
#   * Description       	: 
#  ==========================================================================================

# -------------------------------------------------------------------------------------------
# Data Modification
# -------------------------------------------------------------------------------------------
train.data.frame.NOx_LR <- subset(train.data.frame.Modified, select = c(-UREA_WATER_FLOW_CONTROL, -FLUE_GAS_NOX))
test.data.frame.for.NOx_LR <- subset(test.data.frame.for.UREA, select = c(-UREA_WATER_FLOW_CONTROL, -FLUE_GAS_NOX))


# -------------------------------------------------------------------------------------------
# Perform Linear Regression (Y = NOx Generation)
# -------------------------------------------------------------------------------------------
NOx_Prediction_LR <- lm(Revised_NOX ~., data = train.data.frame.NOx_LR)
predict.line.for.NOx.LR <- as.data.frame(predict(NOx_Prediction_LR, newdata = test.data.frame.for.NOx_LR))


# -------------------------------------------------------------------------------------------
# Visualize Predict Result
# -------------------------------------------------------------------------------------------
 plot(test.data.frame.for.NOx_LR$Revised_NOX, type="l", pch=16,
      col="blue", ylab="Revised_NOX", xlab=NA, main="Linear Regression", lwd=3)
 lines(predict.line.for.NOx.LR, col="red", lwd=2)
# 
# print("NOx Linear Regression complete.")

