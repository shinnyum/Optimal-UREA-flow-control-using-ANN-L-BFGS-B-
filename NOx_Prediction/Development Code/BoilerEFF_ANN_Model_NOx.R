###################################
## Build and Train the ANN Model ##
###################################

## Input data
train.data.frame <- NOx.Emission_Trainset
test.data.frame <- NOx.Emission_Testset
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "FLUE_GAS_NOX"
x <- setdiff(names(train.data.frame.h2o),y)
N <- length(NOx.Emission_Trainset) - 1

## Perform ANN
NOx.Emission.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(10,10),
                                     export_weights_and_biases = TRUE, standardize = TRUE)
NOx.Emission.h2o #결과보기

predicted.NOx.Emission.h2o <- h2o.predict(NOx.Emission.h2o, test.data.frame.h2o)
performance.NOx.Emission.h2o <- h2o.performance(NOx.Emission.h2o, test.data.frame.h2o)
performance.NOx.Emission.h2o #결과보기
as.data.frame( h2o.cbind(predicted.NOx.Emission.h2o$predict, test.data.frame.h2o$FLUE_GAS_NOX) )

## ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.NOx.Emission.h2o$predict))
test.FLUE_GAS_NOX <- as.data.frame(as.numeric(test.data.frame.h2o$FLUE_GAS_NOX))
test.FLUE_GAS_NOX <- test.FLUE_GAS_NOX[[1]]

## ANN prediction plot with test data
plot(test.FLUE_GAS_NOX, type="l", pch=16,
     col="blue", ylab="FLUE_GAS_NOX", xlab=NA, main="DeepLearning Model", lwd=3)
lines(predict.line.h2o, col="red", lwd=2)

## Check error rate
error.rate.predicted.NOx.Emission.h2o <- (abs(predicted.NOx.Emission.h2o
                                             -test.data.frame.h2o$FLUE_GAS_NOX)
                                             /test.data.frame.h2o$FLUE_GAS_NOX) * 100
mean.error.rate.predicted.NOx.Emission.h2o <- mean(error.rate.predicted.NOx.Emission.h2o)
mean.error.rate.predicted.NOx.Emission.h2o

print(paste('ANN Percentage Error :', mean.error.rate.predicted.NOx.Emission.h2o, '%'))
print("Model construction complete.")

