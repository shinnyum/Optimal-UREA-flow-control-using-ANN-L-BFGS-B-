############################################
## Build and Train the ANN Model for UREA ##
############################################

## h2o Input data
h2o.init()
train.data.frame.h2o         <- as.h2o(subset(train.data.frame, select = c(-UREA_WATER_FLOW_CONTROL)))
test.data.frame.for.UREA.h2o <- as.h2o(subset(test.data.frame.for.UREA, select = c(-UREA_WATER_FLOW_CONTROL)))

y <- "FLUE_GAS_NOX"
x <- setdiff(names(train.data.frame.h2o),y)

## Perform ANN
NOx.Emission.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(15,15),
                                     export_weights_and_biases = TRUE, standardize = TRUE)
NOx.Emission.h2o #결과보기

predicted.NOx.Emission.h2o   <- h2o.predict(NOx.Emission.h2o, test.data.frame.for.UREA.h2o)
performance.NOx.Emission.h2o <- h2o.performance(NOx.Emission.h2o, test.data.frame.for.UREA.h2o)
as.data.frame(h2o.cbind(predicted.NOx.Emission.h2o$predict, test.data.frame.for.UREA.h2o$FLUE_GAS_NOX))

## ANN prediction plot with test.data.frame.for.UREA
predict.line.for.UREA.h2o <- as.data.frame(h2o.cbind(predicted.NOx.Emission.h2o$predict))
test.FLUE_GAS_NOX         <- as.data.frame(as.numeric(test.data.frame.for.UREA.h2o$FLUE_GAS_NOX))
test.FLUE_GAS_NOX         <- test.FLUE_GAS_NOX[[1]]

## ANN prediction plot with test data
plot(test.FLUE_GAS_NOX, type="l", pch=16,
     col="blue", ylab="FLUE_GAS_NOX", xlab=NA, main="DeepLearning Model", lwd=3)
lines(predict.line.for.UREA.h2o, col="red", lwd=2)

print("Model construction complete.")

