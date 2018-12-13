###################################
## Build and Train the ANN Model ##
###################################

## Input data
train.data.frame <- Boiler_All_Test_Trainset
test.data.frame <- Boiler_All_Test_Testset
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "BoilerEFF"
x <- setdiff(names(train.data.frame.h2o),y)
N <- length(Boiler_All_Test_Trainset)-1

## Perform ANN
EFF.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(15), 
                            export_weights_and_biases = TRUE, standardize = TRUE)
EFF.h2o #결과보기

predicted.EFF.h2o <- h2o.predict(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o <- h2o.performance(EFF.h2o, test.data.frame.h2o)
performance.EFF.h2o #결과보기
as.data.frame( h2o.cbind(predicted.EFF.h2o$predict, test.data.frame.h2o$BoilerEFF) )

## ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.EFF.h2o$predict))
test.BoilerEFF <- as.data.frame(as.numeric(test.data.frame.h2o$BoilerEFF))
test.BoilerEFF <- test.BoilerEFF[[1]]

## ANN prediction plot with test data
plot(test.BoilerEFF, type="l", pch=16,
     col="blue", ylab="BoilerEFF", xlab=NA, main="DeepLearning Model", lwd=3)
lines(predict.line.h2o, col="red", lwd=2)

## Check error rate
error.rate.predicted.EFF.h2o<-(abs(predicted.EFF.h2o
                                   -test.data.frame.h2o$BoilerEFF)
                                   /test.data.frame.h2o$BoilerEFF)*100
mean.error.rate.predicted.EFF.h2o<-mean(error.rate.predicted.EFF.h2o)
mean.error.rate.predicted.EFF.h2o

print("Model construction is done")

