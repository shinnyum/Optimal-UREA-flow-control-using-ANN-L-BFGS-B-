install.packages("psych")
library(psych)
library(readxl)

NOx_train <- read_excel("D:/work/dataset/NOx_prediction.xlsx", sheet = "train")

color.index<-as.factor(NOx_train$Grade)
pairs.panels(NOx_train[,2:11], scale=FALSE, font.labels=2, cex.labels=1, bg=c("orange","blue","black")[color.index], pch=23, cex=2.5)
