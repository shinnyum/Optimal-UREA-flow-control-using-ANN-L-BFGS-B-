##############################################################
## Extract "Closed-form Formula (Feed-forward propagation)" ##
##############################################################

## Export weights and biases from trained ANN
# h2o.download_pojo(NOx.Emission.h2o)

first.Layer.Weights <- h2o.weights(NOx.Emission.h2o, matrix_id = 1)
second.Layer.Weights <- h2o.weights(NOx.Emission.h2o, matrix_id = 2)
third.Layer.Weights <- h2o.weights(NOx.Emission.h2o, matrix_id = 3)
first.Layer.Biases <- h2o.biases(NOx.Emission.h2o, vector_id = 1)
second.Layer.Biases <- h2o.biases(NOx.Emission.h2o, vector_id = 2)
third.Layer.Biases <- h2o.biases(NOx.Emission.h2o, vector_id = 3)

first.Layer.Weights_Num.vector <- t(as.matrix(as.numeric(first.Layer.Weights)))
second.Layer.Weights_Num.vector <- t(as.matrix(as.numeric(second.Layer.Weights)))
third.Layer.Weights_Num.vector <- t(as.matrix(as.numeric(third.Layer.Weights)))
first.Layer.Biases_Num.vector <- t(as.matrix(as.numeric(first.Layer.Biases)))
second.Layer.Biases_Num.vector <- t(as.matrix(as.numeric(second.Layer.Biases)))
third.Layer.Biases_Num.vector <- as.matrix(as.numeric(third.Layer.Biases))

## Define and initialize input dataset
X.input <- as.matrix(subset(test.data.frame, select=c(-FLUE_GAS_NOX)))
train.X.data <- as.matrix(subset(train.data.frame, select=c(-FLUE_GAS_NOX)))
normalized.X.input <- NULL
dataFrame.Y.output <- NULL
mean.X.input <- NULL
std.X.input <- NULL

## Get X's means and standard deviations from train dataset (for Normalization)
for(i in 1:ncol(train.X.data)){
  
  mean.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(mean(x, na.rm = TRUE)))
  std.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(sd(x, na.rm = TRUE)))
  mean.X.input <- cbind(mean.X.input, mean.data)
  std.X.input <- cbind(std.X.input, std.data)
}

## Perform Normalization (X data)
for(i in 1:ncol(X.input)){
  
  normalized.data <- apply(as.data.frame(X.input[,i]), MARGIN = 2, 
                           FUN = function(x, na.rm = TRUE)((x - mean.X.input[,i]) / std.X.input[,i]))
  normalized.X.input <- cbind(normalized.X.input, normalized.data)
}

## ANN "Feed-forward propagation" formula
for(i in 1:nrow(X.input)){
  
  input.x.matrix <- matrix(normalized.X.input[i,], nrow = 1)
  
  ## Matrix multiplication (First hidden layer)
  first.hidden.layer.beforeA.F. <- input.x.matrix %*% first.Layer.Weights_Num.vector + first.Layer.Biases_Num.vector
  ## Activation function (Tanh)
  first.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(first.hidden.layer.beforeA.F.)))
  ## Matrix multiplication (Second hidden layer)
  second.hidden.layer.beforeA.F. <- first.hidden.layer.afterA.F. %*% second.Layer.Weights_Num.vector + second.Layer.Biases_Num.vector
  ## Activation function (Tanh)
  second.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(second.hidden.layer.beforeA.F.)))
  ## Matrix multiplication (third hidden layer)
  third.hidden.layer.beforeA.F. <- second.hidden.layer.afterA.F. %*% third.Layer.Weights_Num.vector + third.Layer.Biases_Num.vector
  
  Y.output <- third.hidden.layer.beforeA.F.
  dataFrame.Y.output <- rbind(dataFrame.Y.output, Y.output)
}

## Get Y's means and standard deviations from train dataset (for Inverse Normalization)
mean.Y.output <- mean(train.data.frame$FLUE_GAS_NOX, na.rm = TRUE)
std.Y.output <- sd(train.data.frame$FLUE_GAS_NOX, na.rm = TRUE)
normalized.Y.output <- dataFrame.Y.output

## Perform Inverse normalization
inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2, 
                                              FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))

## Compare h2o's and ANN formula's output
cbind(as.data.frame(predicted.NOx.Emission.h2o$predict), inverseTransform.normalized.Y.output)
lines(inverseTransform.normalized.Y.output, col="green",lwd=2)

print("ANN-formula Extraction complete.")

