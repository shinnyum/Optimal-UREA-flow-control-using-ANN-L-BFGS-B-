####################################################
## Optimal UREA Prediction with Linear Regression ##
####################################################

## Prepare predict data set with NOx Gap 
NOx_Prediction_Test_Gap            <- (predict.line.for.NOx.LR - Target_NOx)
# NOx_Prediction_Test_Gap            <- (predict.line.for.UREA.h2o - Target_NOx)
colnames(NOx_Prediction_Test_Gap)  <- "NOx_Gap"
NOx.Emission_Testset_Gap           <- cbind(NOx.Emission_Testset, NOx_Prediction_Test_Gap)
NOx.Emission_Testset_Gap           <- subset(NOx.Emission_Testset_Gap, select = c(-FLUE_GAS_NOX))

predicted.UREA_Flow <- NULL

for(i in 1:numOfDataSet) {
  
  if (NOx.Emission_Testset_Gap$NOx_Gap[i] > 0) {
        predicted.UREA_Flow[i] <- ( 324   + 0.001070 * NOx.Emission_Testset_Gap$COG_FUEL_GAS_FLOW_CONTROL[i]
                                          + 0.297000 * NOx.Emission_Testset_Gap$GENERATOR_MW[i]
                                          + 2.780000 * NOx.Emission_Testset_Gap$COG_TEMP[i]
                                          - 0.368000 * NOx.Emission_Testset_Gap$LDG_TEMP[i]
                                          - 2.110000 * NOx.Emission_Testset_Gap$NG_TEMP[i]
                                          - 1.570000 * NOx.Emission_Testset_Gap$GAH_OUTLET_AIR_TEMP[i]
                                          - 0.013300 * NOx.Emission_Testset_Gap$GAH_OUTLET_GAS_TEMP[i]
                                          + 0.102000 * NOx.Emission_Testset_Gap$FEED_WATER_FLOW[i]
                                          - 3.270000 * NOx.Emission_Testset_Gap$Average_O2[i]
                                          + 2.790000 * NOx.Emission_Testset_Gap$NOx_Gap[i] )
      # predicted.UREA_Flow[i] <- predict(UREA_Flow_LR, newdata = NOx.Emission_Testset_Gap[i,])
        if (predicted.UREA_Flow[i] >= 400) {
              predicted.UREA_Flow[i] <- 390
        }
  }
  else {
      predicted.UREA_Flow[i] <- 0
  }
}


FINAL_OUTPUT_UREA            <- as.data.frame(predicted.UREA_Flow)
colnames(FINAL_OUTPUT_UREA)  <- "OPTIMAL_UREA_WATER_FLOW"

write.xlsx(FINAL_OUTPUT_UREA, "D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/Optimal_UREA_Flow.xlsx")
# write.xlsx(NOx_Prediction_Test_Gap, "D:/work/Rcode/BoilerEFF/NOx_Prediction/NOx_UREA_Prediction/Gap.xlsx")

print("UREA optimization complete.")

