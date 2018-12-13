#######################################
## "L-BFGS-B" Nonlinear Optimization ##
#######################################

## Define ANN Closed-form function
Closed.form.function <- function(x){
  
  X.input <- as.matrix(subset(test.data.frame[i,], select=c(-MAIN_STEAM_FLOW_SUM)))
  X.input[,8] <- x[1]
  X.input[,9] <- x[2]
  X.input[,10] <- x[3]
  X.input[,11] <- x[4]
  X.input[,14] <- x[5]
  X.input[,17] <- x[6]
  X.input[,18] <- x[7]
  X.input[,21] <- x[8]
  X.input[,22] <- x[9]
  X.input[,23] <- x[10]
  X.input[,24] <- x[11]
  X.input[,29] <- x[12]
  X.input[,31] <- x[13]

  train.X.data <- as.matrix(subset(train.data.frame, select=c(-MAIN_STEAM_FLOW_SUM)))
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
  
  mean.Y.output <- mean(train.data.frame$MAIN_STEAM_FLOW_SUM, na.rm = TRUE)
  std.Y.output <- sd(train.data.frame$MAIN_STEAM_FLOW_SUM, na.rm = TRUE)
  normalized.Y.output <- dataFrame.Y.output
  inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2, 
                                                FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))
  
  ## Set Decision Variables : BFG FLOW, COG FLOW, LDG FLOW, Calories
  BFG_FUEL_GAS_FLOW_CONTROL <- X.input[,4]
  COG_FUEL_GAS_FLOW_CONTROL <- X.input[,7]
  LDG_FUEL_GAS_FLOW_CONTROL <- X.input[,5]
  BFG_Calories <- X.input[,1]
  COG_Calories <- X.input[,2]
  LDG_Calories <- X.input[,3]
  
  ## Define "Thermal output" variables
  FEED_WATER_FLOW <- X.input[,25]
  SH_SPRAY_FLOW_SUM <- -X.input[,26]
  MAIN_STEAM_FLOW_SUM <- inverseTransform.normalized.Y.output
  
  ## BoilerEFF Equation of 75MW Generating Power
  BoilerEFF <- (((MAIN_STEAM_FLOW_SUM * 828.6 + (MAIN_STEAM_FLOW_SUM - 26) * 848.6
                - FEED_WATER_FLOW * 215.3 - (MAIN_STEAM_FLOW_SUM - 26) * 744.5 - SH_SPRAY_FLOW_SUM * 160.8) * 1000) /
                ((BFG_FUEL_GAS_FLOW_CONTROL * BFG_Calories) + 
                 (COG_FUEL_GAS_FLOW_CONTROL * COG_Calories) + 
                 (LDG_FUEL_GAS_FLOW_CONTROL * LDG_Calories)))
  
  return(BoilerEFF)
}

## Set initial values of Decision Variables
initial_par <- c(3.15,260,150,530,36,2,100,46,42,5,76,210,37)

for(i in 1:20){
  
  ## Perform Optimization using "L-BFGS-B" method
  optim_Result <- optim(initial_par, Closed.form.function, NULL, method = "L-BFGS-B", 
                        lower = c(3,100,40,300,25,0,80,0,20,2,50,150,0), 
                        upper= c(4,500,450,650,45,10,110,55,70,7,100,220,100),
                        hessian = FALSE) #control = list(fnscale = -1))
  initial_par <- optim_Result$par
  print(optim_Result$par)
  print(optim_Result$value)
  
}

print("BoilerEFF optimization is done")
