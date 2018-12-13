###################################
## Build and Train the ANN Model ##
###################################

## Input data
train.data.frame <- Boiler8_Trainset
test.data.frame <- Boiler8_Testset
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "MAIN_STEAM_FLOW_SUM"
x <- setdiff(names(train.data.frame.h2o),y)
N <- length(Boiler8_Trainset)-1

## Perform ANN
MSF.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(15), 
                            export_weights_and_biases = TRUE, standardize = TRUE)
MSF.h2o #결과보기

predicted.MSF.h2o <- h2o.predict(MSF.h2o, test.data.frame.h2o)
performance.MSF.h2o <- h2o.performance(MSF.h2o, test.data.frame.h2o)
performance.MSF.h2o #결과보기
as.data.frame( h2o.cbind(predicted.MSF.h2o$predict, test.data.frame.h2o$MAIN_STEAM_FLOW_SUM) )

## ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.MSF.h2o$predict))
test.MAIN_STEAM_FLOW_SUM <- as.data.frame(as.numeric(test.data.frame.h2o$MAIN_STEAM_FLOW_SUM))
test.MAIN_STEAM_FLOW_SUM <- test.MAIN_STEAM_FLOW_SUM[[1]]

## ANN prediction plot with test data
plot(test.MAIN_STEAM_FLOW_SUM, type="l", pch=16,
     col="blue", ylab="MAIN_STEAM_FLOW_SUM", xlab=NA, main="DeepLearning Model", lwd=3)
lines(predict.line.h2o, col="red", lwd=2)

## Check error rate
error.rate.predicted.MSF.h2o<-(abs(predicted.MSF.h2o
                                   -test.data.frame.h2o$MAIN_STEAM_FLOW_SUM)
                                   /test.data.frame.h2o$MAIN_STEAM_FLOW_SUM)*100
mean.error.rate.predicted.MSF.h2o<-mean(error.rate.predicted.MSF.h2o)
mean.error.rate.predicted.MSF.h2o

print("Model construction is done")

