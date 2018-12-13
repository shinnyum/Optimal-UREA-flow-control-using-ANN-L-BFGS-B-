#######################################
## "L-BFGS-B" Nonlinear Optimization ##
#######################################

## Define ANN Closed-form function(1)
Closed.form.function.1 <- function(x){
  
  X.input <- as.matrix(subset(test.data.frame[i,], select=c(-FLUE_GAS_NOX)))
  X.input[,1] <- x[1]

  train.X.data <- as.matrix(subset(train.data.frame, select=c(-FLUE_GAS_NOX)))
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
    second.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(second.hidden.layer.beforeA.F.)))
    third.hidden.layer.beforeA.F. <- second.hidden.layer.afterA.F. %*% third.Layer.Weights_Num.vector + third.Layer.Biases_Num.vector
    Y.output <- third.hidden.layer.beforeA.F.
    dataFrame.Y.output <- rbind(dataFrame.Y.output, Y.output)
  }
  
  mean.Y.output <- mean(train.data.frame$FLUE_GAS_NOX, na.rm = TRUE)
  std.Y.output <- sd(train.data.frame$FLUE_GAS_NOX, na.rm = TRUE)
  normalized.Y.output <- dataFrame.Y.output
  inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2,
                                                FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))
  
  ## Setting the "Target NOx Emission" (50 ppm)
  target.NOx.Emission <- 55
  
  ## Define "Gap" between target value and predicted value
  predicted.NOx.Emission <- inverseTransform.normalized.Y.output
  gap.NOx.Emission <- abs(target.NOx.Emission - predicted.NOx.Emission)

  return(gap.NOx.Emission)
}

## Define ANN Closed-form function(2)
Closed.form.function.2 <- function(x){
  
  X.input <- as.matrix(subset(test.data.frame[i,], select=c(-FLUE_GAS_NOX)))
  X.input[,1] <- x[1]
  
  train.X.data <- as.matrix(subset(train.data.frame, select=c(-FLUE_GAS_NOX)))
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
    second.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(second.hidden.layer.beforeA.F.)))
    third.hidden.layer.beforeA.F. <- second.hidden.layer.afterA.F. %*% third.Layer.Weights_Num.vector + third.Layer.Biases_Num.vector
    Y.output <- third.hidden.layer.beforeA.F.
    dataFrame.Y.output <- rbind(dataFrame.Y.output, Y.output)
  }
  
  mean.Y.output <- mean(train.data.frame$FLUE_GAS_NOX, na.rm = TRUE)
  std.Y.output <- sd(train.data.frame$FLUE_GAS_NOX, na.rm = TRUE)
  normalized.Y.output <- dataFrame.Y.output
  inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2, 
                                                FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))
  
  ## Define "Gap" between target value and predicted value
  predicted.NOx.Emission <- inverseTransform.normalized.Y.output

  return(predicted.NOx.Emission)
}

## Set initial values of Decision Variables
initial_par <- c(0)
FinalOutput.Data.Frame <- NULL

for(i in 1:numOfDataSet){
  
  ## Perform Optimization using "L-BFGS-B" method
  optim_Result <- optim(initial_par, Closed.form.function.1 , NULL, method = "L-BFGS-B", 
                        lower = c(0), 
                        upper= c(400),
                        hessian = FALSE) #control = list(fnscale = -1))
  # initial_par <- optim_Result$par
  
  predicted.NOx.Emission_optim <- Closed.form.function.2(initial_par)
  
  ## Print Output Values
  writeLines(c(paste('[', i, 'th OUTPUT VALUES ]'),
               paste('UREA_WATER_FLOW :', optim_Result$par[[1]]),
             # paste('BFG_FUEL_GAS_FLOW :', test.data.frame$BFG_FUEL_GAS_FLOW_CONTROL[i]),
             # paste('LDG_FUEL_GAS_FLOW :', test.data.frame$LDG_FUEL_GAS_FLOW_CONTROL[i]),
             # paste('Combustion_AIR_Flow :', optim_Result$par[[2]]),
             # paste('Feed_Water_Flow :', optim_Result$par[[3]]),
             # paste('SH_Spray_Flow_Sum :', optim_Result$par[[4]]),
             # paste('Make-Up_Water_Flow :', optim_Result$par[[5]]),
             # paste('BFG_Temperature :', optim_Result$par[[6]]),
             # paste('Gap_Between_Target_NOx_Emission :', optim_Result$value),
               paste('Predicted_NOx_Emission :', predicted.NOx.Emission_optim),
               paste(' ')))
  
  ## Save Output Values
  Output.Data.Frame <- data.frame("UREA_WATER_FLOW" = optim_Result$par[[1]],
                                # "BFG_FUEL_GAS_FLOW" = test.data.frame$BFG_FUEL_GAS_FLOW_CONTROL[i],
                                # "LDG_FUEL_GAS_FLOW" = test.data.frame$LDG_FUEL_GAS_FLOW_CONTROL[i],
                                # "Combustion_AIR_Flow" = optim_Result$par[[2]],
                                # "Feed_Water_Flow" = optim_Result$par[[3]],
                                # "SH_Spray_Flow_Sum" = optim_Result$par[[4]],
                                # "Make-Up_Water_Flow" = optim_Result$par[[5]],
                                # "BFG_Temperature" = optim_Result$par[[6]]),
                                  "Predicted_NOx_Emission" =  predicted.NOx.Emission_optim)
                                  
  FinalOutput.Data.Frame <- rbind(FinalOutput.Data.Frame, Output.Data.Frame)

}

write.xlsx(FinalOutput.Data.Frame, "D:/work/Rcode/BoilerEFF/NOx_Prediction/Average_O2/Optimal UREA Values.xlsx")
print("BoilerEFF optimization complete.")

