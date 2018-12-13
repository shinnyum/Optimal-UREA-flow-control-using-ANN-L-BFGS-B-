#  ==================================================================================
#   * Program Name       	: 발전소 보일러효율향상 MAIN RSCRIPT
#   * Source File Name   	: BoilerEFF_PRED_MAIN.R
#   * Author             	: 포스코 ICT
#   * Version           	: 1.0.0
#   * Created Date       	: 2017-11-10
#   * Updated Date      	: 2017-11-11
#   * Last modifier      	: 
#   * Updated content    	: Revised NOX 추가
#   * Description       	: SQL생성 부분 수정 필요
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Package 라이브러리 로딩
# -----------------------------------------------------------------------------------
suppressMessages({
  library(h2o)
  library(methods)
  library(evd)
  library(readxl)
  library(xlsx)
  library(rJava)
  library(RJDBC)
})


# -----------------------------------------------------------------------------------
# Working Directory Setting
# -----------------------------------------------------------------------------------
Working_dir <- "/model_data/t/mm/PA_171017_001/"

setwd(Working_dir)


# -----------------------------------------------------------------------------------
# DB Connection : Posframe DB(PPAS)
# -----------------------------------------------------------------------------------
# Create connection driver
drv <- JDBC(driverClass="org.postgresql.Driver", classPath="/model_data/t/cm/lib/postgresql-9.4.1207.jre7.jar")

# DB Connection(개발계)
db_connect_PPAS <- dbConnect(drv = drv, "jdbc:postgresql://172.18.49.87:5470/tpsden" ,"posener","tpsden1" )
# DB Connection(가동계)
# db_connect_PPAS <- dbConnect(drv = drv, "jdbc:postgresql://172.18.49.85:5465/ppsden" ,"posmgener","sdenopen" )


# -----------------------------------------------------------------------------------
# load Model
# -----------------------------------------------------------------------------------
load(paste0(Working_dir, "Optimal_UREA.RData"))
Modified_Trainset_NOX <- read_excel("./NOx_prediction_(Boiler_9)_ver5_(stack_O2).xlsx", sheet = "Modified_NOX_train")
Target_NOx <- 53


# -----------------------------------------------------------------------------------
#  PPAS DB로부터 분단위 실적값 조회
# -----------------------------------------------------------------------------------
# --------------------------------------------------
# Workbench AP부터 인자값을 받기 위한 Argumnets 정의 
#  - @PARAM  START_TIME : 현재시작시각
# --------------------------------------------------
inputArgs  <- data.frame(df)
START_TIME <- inputArgs$START_TIME

# Argumnets 확인
print(paste(":::::: START_TIME ::::::", START_TIME))

# START_TIME Format 변경
START_TIME <- gsub("[[:punct:]]|[[:space:]]", "", as.character(as.POSIXct(strptime(START_TIME, "%Y%m%d%H%M%S"))))
START_TIME <- ifelse(nchar(START_TIME) == 8, paste0(START_TIME, "000000"), START_TIME)

# SQL 생성
sql_query <- paste0(" SELECT WORKS_CODE, MIDT_CLT_DT, MI_DT_CLT_SERV_TP,",
                    " ERGCG_BR_FG_O2_C_B_CA_NOXZ_QT,ERGCG_BR_SNCR_EMT_WTR_CA_FL, ERGCG_BR_CA_COG_FG_FL, ERG_GENT_CA_HPN_EFF_EPW, ERGCG_BR_ECO_EN_CA_UWA_TMP, ERGCG_BR_LDG_CA_FG_TMP,",
                    " ERGCG_BR_CA_NG_BRI_TMP, ERGCG_BR_CA_GAH_EX_AR_TMP, ERGCG_BR_CA_GAH_EX_GS_TMP, ERGCG_BR_FEED_UWA_CA_FL, ERGCG_BR_SUPC_HTR_CA_SP_FL, ERGCG_BR_CA_ECO_EX_WGAS_ODEN4,",
                    " ERGCG_BR_CA_ECO_EX_WGAS_ODEN5, ERGCG_BR_CA_ECO_EX_WGAS_ODEN6, ERGCG_BR_FG_O2_C_A_CA_NOXZ_QT ",
                    " FROM TB_E12_BOIL_TAG010 ",
                    " WHERE WORKS_CODE ='P'",
                    " AND MI_DT_CLT_SERV_TP = '9' ",
                    " AND MIDT_CLT_DT = TO_DATE('", START_TIME, "', 'YYYYMMDDHH24MISS') ")
# SQL문 실행
NOx.Emission_Testset_df <- dbGetQuery(db_connect_PPAS, sql_query)

if(nrow(NOx.Emission_Testset_df) == 0) {
  # 유효한 실적 데이터가 존재하지 않을 경우
  result <- "0"
  
} else {
  
  # delete Column : WORKS_CODE, MIDT_CLT_DT, MI_DT_CLT_SERV_TP  
  NOx.Emission_Testset <- NOx.Emission_Testset_df[,-c(1:3)]
  
  # Change Column Name 
  item <- c("FLUE_GAS_NOX","UREA_WATER_FLOW_CONTROL","COG_FUEL_GAS_FLOW_CONTROL","GENERATOR_MW","COG_TEMP",
            "LDG_TEMP","NG_TEMP","GAH_OUTLET_AIR_TEMP","GAH_OUTLET_GAS_TEMP","FEED_WATER_FLOW","SH_SPRAY_FLOW_SUM","O2_D","O2_E","O2_F","STACK_O2")
  
  for(i in 1:length(item)){
    names(NOx.Emission_Testset)[i] <- item[i]
  }
  
  
  # -----------------------------------------------------------------------------------
  # 데이터 가공
  # -----------------------------------------------------------------------------------
  # Calculating average O2
  Average.O2_Test               <- (subset(NOx.Emission_Testset, select="O2_D") + subset(NOx.Emission_Testset, select="O2_E") + subset(NOx.Emission_Testset, select="O2_F")) / 3
  Average.O2_Modified_Train_NOX <- (subset(Modified_Trainset_NOX, select="O2_D") + subset(Modified_Trainset_NOX, select="O2_E") + subset(Modified_Trainset_NOX, select="O2_F")) / 3
  
  # Change Column Name
  colnames(Average.O2_Test)               <- "Average_O2"
  colnames(Average.O2_Modified_Train_NOX) <- "Average_O2"
  
  # NOx revising
  NOx.revising_Test           <- (subset(NOx.Emission_Testset, select="FLUE_GAS_NOX") * (21 - 4) / (21 - subset(NOx.Emission_Testset, select="STACK_O2")))
  NOx.revising_Modified_Train <- (subset(Modified_Trainset_NOX, select="FLUE_GAS_NOX") * (21 - 4) / (21 - subset(Modified_Trainset_NOX, select="STACK_O2")))
  
  # Change Column Name
  colnames(NOx.revising_Test)           <- "Revised_NOX"
  colnames(NOx.revising_Modified_Train) <- "Revised_NOX"
  
  # Column Binding
  NOx.Emission_Testset  <- cbind(NOx.Emission_Testset[,c(1:11,15)], Average.O2_Test, NOx.revising_Test)
  Modified_Trainset_NOX <- cbind(Modified_Trainset_NOX[,c(1:11,15)], Average.O2_Modified_Train_NOX, NOx.revising_Modified_Train)
  
  # Input data
  test.data.frame.for.UREA  <- NOx.Emission_Testset
  train.data.frame.Modified <- Modified_Trainset_NOX
  
  
  # -----------------------------------------------------------------------------------
  # Perform "Optimal UREA Flow Prediction"
  # -----------------------------------------------------------------------------------
  # Perform NOx Linear_Regression
  source("NOx_Prediction_(Linear_Regression).R")
  
  # Perform "Optimal UREA Prediction with Linear Regression"
  source("Optimal_UREA_Flow_Prediction.R")
  
  
  # -----------------------------------------------------------------------------------
  # PPAS 결과 테이블에 Output Values 저장
  # -----------------------------------------------------------------------------------
  # 결과 데이터셋
  result_df <- data.frame(WORKS_CODE = NOx.Emission_Testset_df[1],
                          MIDT_CLT_DT = NOx.Emission_Testset_df[2],
                          MI_DT_CLT_SERV_TP = NOx.Emission_Testset_df[3],
                          ERGHG_BR_EMT_WTR_CA_FL = as.numeric(round(FINAL_OUTPUT_UREA,digits = 4)),
                          stringsAsFactors = FALSE)
  # 결과 등록 SQL
  insert_sql <- paste0("INSERT INTO TB_E12_BOIL_TAG020 VALUES('",result_df$works_code, "','",result_df$mi_dt_clt_serv_tp,"',TO_DATE('",result_df$midt_clt_dt,"', 'YYYY/MM/DD HH24:MI:SS'),",result_df$ERGHG_BR_EMT_WTR_CA_FL, ")")
  dbSendUpdate(db_connect_PPAS, insert_sql);
  
  # PPAS DB Connection close
  dbDisconnect(db_connect_PPAS)
  
  
  # -----------------------------------------------------------------------------------
  #  Return Result
  # -----------------------------------------------------------------------------------
  result <- unlist(result_df$ERGHG_BR_EMT_WTR_CA_FL)
}

