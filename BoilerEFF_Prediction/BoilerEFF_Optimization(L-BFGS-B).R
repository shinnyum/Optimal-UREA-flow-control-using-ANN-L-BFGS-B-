#######################################
## "L-BFGS-B" Nonlinear Optimization ##
#######################################

## Define ANN Closed-form function
Closed.form.function <- function(x){
  
  X.input <- as.matrix(subset(test.data.frame[1,], select=c(-BoilerEFF)))
  X.input[,1] <- x[1]
  X.input[,2] <- x[2]
  X.input[,3] <- x[3]
  X.input[,4] <- x[4]
  X.input[,10] <- x[5]
  X.input[,20] <- x[6]
  X.input[,21] <- x[7]
  
  train.X.data <- as.matrix(subset(train.data.frame, select=c(-BoilerEFF)))
  normalized.X.input <- NULL
  dataFrame.Y.output <- NULL
  mean.X.input <- NULL
  std.X.input <- NULL
  
  for(i in 1:ncol(train.X.data)){
    mean.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(mean(x, na.rm = TRUE)))
    std.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(sd(x, na.rm = TRUE)))
    mean.X.input <- cbind(mean.X.input, mean.data)
    std.X.input <- cbind(std.X.input, std.data)
  }
  
  for(i in 1:ncol(X.input)){
    normalized.data <- apply(as.data.frame(X.input[,i]), MARGIN = 2, 
                             FUN = function(x, na.rm = TRUE)((x - mean.X.input[,i]) / std.X.input[,i]))
    normalized.X.input <- cbind(normalized.X.input, normalized.data)
  }
  
  for(i in 1:nrow(X.input)){
    input.x.matrix <- matrix(normalized.X.input[i,], nrow = 1)
    first.hidden.layer.beforeA.F. <- input.x.matrix %*% first.Layer.Weights_Num.vector + first.Layer.Biases_Num.vector
    first.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(first.hidden.layer.beforeA.F.)))
    second.hidden.layer.beforeA.F. <- first.hidden.layer.afterA.F. %*% second.Layer.Weights_Num.vector + second.Layer.Biases_Num.vector
    Y.output <- second.hidden.layer.beforeA.F.
    dataFrame.Y.output <- rbind(dataFrame.Y.output, Y.output)
  }
  
  mean.Y.output <- mean(train.data.frame$BoilerEFF)
  std.Y.output <- sd(train.data.frame$BoilerEFF)
  normalized.Y.output <- dataFrame.Y.output
  inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2, 
                                                FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))
  
  return(inverseTransform.normalized.Y.output)
}

## Set initial values of Decision Variables
initial_par <- c(162,10545,89493,21950,50,70,54)

## Perform Optimization using "L-BFGS-B" method
optim_Result <- optim(initial_par, Closed.form.function, NULL, method = "L-BFGS-B", 
                      lower = c(800,4500,5700,10000,0,65,0), upper= c(2400,15000,130000,24000,60,80,200),
                      hessian = TRUE, control = list(fnscale = -1))

print(optim_Result)
print("BoilerEFF optimization is done")

