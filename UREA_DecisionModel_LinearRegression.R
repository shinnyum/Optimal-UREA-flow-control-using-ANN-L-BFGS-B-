#  ==================================================================================
#   * Program Name       	: 諛쒖쟾?냼 蹂댁씪?윭?슚?쑉?뼢?긽 UREA LINEAR REGRESSION RSCRIPT
#   * Source File Name   	: UREA_Linear_Regression.R
#   * Author             	: ?룷?뒪肄? ?븞?쟾?깮?궛?쟾?왂?떎
#   * Version           	: 1.1.5
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-15
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX 異붽?
#   * Description       	: 
#  ==================================================================================

# -----------------------------------------------------------------------------------
#  NOx Gap ?궛?젙
# -----------------------------------------------------------------------------------
NOx_Prediction_Train_Gap            <- (predict.line.for.LR.h2o - NOx.Emission_Trainset_UREA$Revised_NOX)
colnames(NOx_Prediction_Train_Gap)  <- "NOx_Gap"
NOx.Emission_Trainset_UREA_Gap      <- cbind(NOx.Emission_Trainset_UREA, NOx_Prediction_Train_Gap)
NOx.Emission_Trainset_UREA_Gap      <- subset(NOx.Emission_Trainset_UREA_Gap, select = c(-FLUE_GAS_NOX, -Revised_NOX))

# write.xlsx(NOx_Prediction_Train_Gap, "D:/work/dataset/GapGapNOxGap.xlsx")

# -----------------------------------------------------------------------------------
#  Linear Regression
# -----------------------------------------------------------------------------------
UREA_Flow_LR <- lm(UREA_WATER_FLOW_CONTROL ~., data = NOx.Emission_Trainset_UREA_Gap)
summary(UREA_Flow_LR)

print("UREA Linear Regression complete.")

