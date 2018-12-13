#  ==========================================================================================
#   * Program Name       	: Î∞úÏ†Ñ?Üå Î≥¥Ïùº?ü¨?ö®?ú®?ñ•?ÉÅ Optimal UREA Prediction SCRIPT
#   * Source File Name   	: Optimal_UREA_Flow_Prediction.R
#   * Author             	: ?è¨?ä§ÏΩ? ?ïà?†Ñ?Éù?Ç∞?†Ñ?ûµ?ã§
#   * Version           	: 1.1.5
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-15
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX Ï∂îÍ?Ä
#   * Description       	: Min = 40ppm
#                           Max = 590ppm
#  ==========================================================================================

# -------------------------------------------------------------------------------------------
# Prepare predict data set with NOx Gap 
# -------------------------------------------------------------------------------------------
NOx_Prediction_Test_Gap            <- (predict.line.for.NOx.LR - Target_NOx)
# NOx_Prediction_Test_Gap            <- (predict.line.for.UREA.h2o - Target_NOx)
colnames(NOx_Prediction_Test_Gap)  <- "NOx_Gap"
NOx.Emission_Testset_Gap           <- cbind(NOx.Emission_Testset, NOx_Prediction_Test_Gap)
NOx.Emission_Testset_Gap           <- subset(NOx.Emission_Testset_Gap, select = c(-FLUE_GAS_NOX, -Revised_NOX))


# -------------------------------------------------------------------------------------------
# Substitute UREA Regression Model
# -------------------------------------------------------------------------------------------
predicted.UREA_Flow <- NULL

for(i in 1:numOfDataSet) {
  

  if (NOx.Emission_Testset_Gap$NOx_Gap[i] > 0) {
    predicted.UREA_Flow[i] <- ( 680.4 + 0.00142 * NOx.Emission_Testset_Gap$COG_FUEL_GAS_FLOW_CONTROL[i]
                                - 0.447100 * NOx.Emission_Testset_Gap$GENERATOR_MW[i]
                                + 5.783000 * NOx.Emission_Testset_Gap$COG_TEMP[i]
                                - 0.579200 * NOx.Emission_Testset_Gap$LDG_TEMP[i]
                                - 3.179000 * NOx.Emission_Testset_Gap$NG_TEMP[i]
                                - 2.423000 * NOx.Emission_Testset_Gap$GAH_OUTLET_AIR_TEMP[i]
                                - 2.365000 * NOx.Emission_Testset_Gap$GAH_OUTLET_GAS_TEMP[i]
                                + 0.878600 * NOx.Emission_Testset_Gap$FEED_WATER_FLOW[i]
                                + 5.975200 * NOx.Emission_Testset_Gap$SH_SPRAY_FLOW_SUM[i]
                                + 7.672000 * NOx.Emission_Testset_Gap$STACK_O2[i]
                                - 5.081000 * NOx.Emission_Testset_Gap$Average_O2[i]
                                + 3.223000 * NOx.Emission_Testset_Gap$NOx_Gap[i] )
    # predicted.UREA_Flow[i] <- predict(UREA_Flow_LR, newdata = NOx.Emission_Testset_Gap[i,])
    
    ## Set Minimum UREA Flow
    if (predicted.UREA_Flow[i] < 0) {
      predicted.UREA_Flow[i] <- 0
    }
    else if (predicted.UREA_Flow[i] > 0 && predicted.UREA_Flow[i] <= 30) {
      predicted.UREA_Flow[i] <- 0
    }
    else if (predicted.UREA_Flow[i] > 30 && predicted.UREA_Flow[i] <= 40) {
      predicted.UREA_Flow[i] <- 40
    }
    
    ## Set Maximum UREA Flow
    if (predicted.UREA_Flow[i] >= 600) {
      predicted.UREA_Flow[i] <- 590
    }
  }
  
  ## ?öî?Üå?àò 0 ?†ú?ïΩ
  else if (NOx.Emission_Testset_Gap$NOx_Gap[i] <= 0 && abs(NOx.Emission_Testset_Gap$NOx_Gap[i]) <= 2) {
    predicted.UREA_Flow[i] <- NOx.Emission_Testset_Gap$UREA_WATER_FLOW_CONTROL[i] * 1.5
  }
  ## ?öî?Üå?àò ?à¨?ûÖ Ï¶ùÎ?Ä ?†ú?ïΩ (NOx Î∞úÏÉù 57ppm?ù¥?ÉÅ ?òàÏ∏°Ïãú ?†Å?ö©)
  else if (NOx.Emission_Testset_Gap$NOx_Gap[i] >= 7) {
    predicted.UREA_Flow[i] <- ( 680.4 + 0.00142 * NOx.Emission_Testset_Gap$COG_FUEL_GAS_FLOW_CONTROL[i]
                                - 0.447100 * NOx.Emission_Testset_Gap$GENERATOR_MW[i]
                                + 5.783000 * NOx.Emission_Testset_Gap$COG_TEMP[i]
                                - 0.579200 * NOx.Emission_Testset_Gap$LDG_TEMP[i]
                                - 3.179000 * NOx.Emission_Testset_Gap$NG_TEMP[i]
                                - 2.423000 * NOx.Emission_Testset_Gap$GAH_OUTLET_AIR_TEMP[i]
                                - 2.365000 * NOx.Emission_Testset_Gap$GAH_OUTLET_GAS_TEMP[i]
                                + 0.878600 * NOx.Emission_Testset_Gap$FEED_WATER_FLOW[i]
                                + 5.975200 * NOx.Emission_Testset_Gap$SH_SPRAY_FLOW_SUM[i]
                                + 7.672000 * NOx.Emission_Testset_Gap$STACK_O2[i]
                                - 5.081000 * NOx.Emission_Testset_Gap$Average_O2[i]
                                + 5.223000 * NOx.Emission_Testset_Gap$NOx_Gap[i] )
  }
  else {
    predicted.UREA_Flow[i] <- 40
  }
  
  ## Set NOx Safty Logic
  if (test.data.frame.for.UREA$Revised_NOX[i] >= 59) {
    
    if (predicted.UREA_Flow[i] != 40 ) {
      predicted.UREA_Flow[i] <- predicted.UREA_Flow[i] * 2.0
    }
    else {
      predicted.UREA_Flow[i] <- 200
    }
  }
  
  if (test.data.frame.for.UREA$Revised_NOX[i] < 35) {
      predicted.UREA_Flow[i] <- 0
  }
  
  ## Set Maximum UREA Flow
  if (predicted.UREA_Flow[i] >= 600) {
      predicted.UREA_Flow[i] <- 590
  }
  
}


# -------------------------------------------------------------------------------------------
# Save Optimal UREA Flow
# -------------------------------------------------------------------------------------------
FINAL_OUTPUT_UREA            <- as.data.frame(predicted.UREA_Flow)
colnames(FINAL_OUTPUT_UREA)  <- "OPTIMAL_UREA_WATER_FLOW"

write.xlsx(FINAL_OUTPUT_UREA, "./Optimal_UREA_Flow.xlsx")
# print("UREA optimization complete.")

