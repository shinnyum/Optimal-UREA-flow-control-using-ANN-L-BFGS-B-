install.packages("evd")
library(evd)

## Create "list" to storage optimal values
results <- list()

for(i in 1:length(Boiler_All_Test$BoilerEFF)){
  
  ## Define "BoilerEFF Equation"
  BoilerEFF <- function(x)
  {
    ## Set Decision Variables : BFG FLOW, COG FLOW, LDG FLOW
    BFG_FUEL_GAS_FLOW_CONTROL <- x[1]*1000
    COG_FUEL_GAS_FLOW_CONTROL <- x[2]*1000
    LDG_FUEL_GAS_FLOW_CONTROL <- x[3]*1000
    
    ## Define "Thermal output" equation (Linear Regression)
    FEED_WATER_FLOW <- 83.4 + 0.00587 * COG_FUEL_GAS_FLOW_CONTROL + 0.000593 * BFG_FUEL_GAS_FLOW_CONTROL + 0.00105 * LDG_FUEL_GAS_FLOW_CONTROL
    SH_SPRAY_FLOW_SUM <- -0.785 + 0.000210 * COG_FUEL_GAS_FLOW_CONTROL + 0.000047 * BFG_FUEL_GAS_FLOW_CONTROL + 0.000051 * LDG_FUEL_GAS_FLOW_CONTROL
    MAIN_STEAM_FLOW_SUM <- 48.5 + 0.00657 * COG_FUEL_GAS_FLOW_CONTROL + 0.000772 * BFG_FUEL_GAS_FLOW_CONTROL +0.00120 * LDG_FUEL_GAS_FLOW_CONTROL
    
    ## BoilerEFF Equation of 75MW Generating Power
    (((MAIN_STEAM_FLOW_SUM * 828.6 + (MAIN_STEAM_FLOW_SUM - 26) * 848.6
       - FEED_WATER_FLOW * 215.3 - (MAIN_STEAM_FLOW_SUM - 26) * 744.5 - SH_SPRAY_FLOW_SUM * 160.8) * 1000) /
        ((BFG_FUEL_GAS_FLOW_CONTROL * Boiler_All_Test$BFG_Calories[i]) + 
           (COG_FUEL_GAS_FLOW_CONTROL * Boiler_All_Test$COG_Calories[i]) + 
           (LDG_FUEL_GAS_FLOW_CONTROL * Boiler_All_Test$LDG_Calories[i])))
    
  }
  
  
  ## Set initial values of Decision Variables
  initial_par <- c(100,9,16)
  
  ## Perform Optimization using "L-BFGS-B" method
  optim_Result <- optim(initial_par, BoilerEFF, NULL, method = "L-BFGS-B", lower = c(40,3,7), upper= c(150,100,30), hessian = TRUE, control = list(fnscale = -1))
  results[[i]] <- optim_Result$par
  print(results[[i]])
}
