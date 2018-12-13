#  ==========================================================================================
#   * Program Name       	: Î∞úÏ†Ñ?Üå Î≥¥Ïùº?ü¨?ö®?ú®?ñ•?ÉÅ BUILD ANN MODEL RSCRIPT
#   * Source File Name   	: NOx_Prediction_ANN_MOdel_for_LR.R
#   * Author             	: ?è¨?ä§ÏΩ? ?ïà?†Ñ?Éù?Ç∞?†Ñ?ûµ?ã§
#   * Version           	: 1.1.5
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-15
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX Ï∂îÍ?Ä
#   * Description       	: 
#  ==========================================================================================

# -------------------------------------------------------------------------------------------
# Data ?òï?ãù Î≥Ä?ôò (h2o)
# -------------------------------------------------------------------------------------------
## h2o Input data
h2o.init()
train.data.frame.h2o       <- as.h2o(subset(train.data.frame, select = c(-UREA_WATER_FLOW_CONTROL, -FLUE_GAS_NOX)))
test.data.frame.for.LR.h2o <- as.h2o(subset(test.data.frame.for.LR, select = c(-UREA_WATER_FLOW_CONTROL, -FLUE_GAS_NOX)))

y <- "Revised_NOX"
x <- setdiff(names(train.data.frame.h2o),y)

# -------------------------------------------------------------------------------------------
# Construct ANN Î™®Îç∏
# -------------------------------------------------------------------------------------------
## Perform ANN
NOx.Emission.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(15),
                                     export_weights_and_biases = FALSE, standardize = TRUE)

predicted.NOx.Emission.h2o   <- h2o.predict(NOx.Emission.h2o, test.data.frame.for.LR.h2o)
performance.NOx.Emission.h2o <- h2o.performance(NOx.Emission.h2o, test.data.frame.for.LR.h2o)
as.data.frame(h2o.cbind(predicted.NOx.Emission.h2o$predict, test.data.frame.for.LR.h2o$Revised_NOX))

## ANN prediction plot with test.data.frame.for.LR
predict.line.for.LR.h2o <- as.data.frame(h2o.cbind(predicted.NOx.Emission.h2o$predict))
test.FLUE_GAS_NOX       <- as.data.frame(as.numeric(test.data.frame.for.LR.h2o$Revised_NOX))
test.FLUE_GAS_NOX       <- test.FLUE_GAS_NOX[[1]]

# -------------------------------------------------------------------------------------------
# ?òàÏ∏°Í≤∞Í≥? ?ãúÍ∞ÅÌôî
# -------------------------------------------------------------------------------------------
## ANN prediction plot with test data
plot(test.FLUE_GAS_NOX, type="l", pch=16,
     col="blue", ylab="Revised_NOX", xlab=NA, main="DeepLearning Model", lwd=3)
lines(predict.line.for.LR.h2o, col="red", lwd=2)

print("Model construction complete.")

