################################################################
## Baseline Comparison by BMI missing and non-missing group
## 
## Dataset used: framingham_filtered.csv
################################################################



## Define file path here
path <- "/Users/chanter/Downloads"


framingham<- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_framingham_filtered.csv"),header = T,na.strings = ".")
framingham[,c("Sex","PrevCHD","PrevAP","PrevHYP","CurSmoke")] <- lapply(framingham[,c("Sex","PrevCHD","PrevAP","PrevHYP","CurSmoke")],factor)

## see the distribution of variables of observations with missing values and those who don't.
id1 <- unique(framingham$RANDID[which(is.na(framingham$TotalChol),arr.ind = T)])
id2 <- unique(framingham$RANDID[which(is.na(framingham$BMI),arr.ind = T)])

## 1 means missing and 0 means nonmissing
framingham$naindi_Chol <- ifelse(framingham$RANDID %in% id1,1,0)
framingham$naindi_BMI <- ifelse(framingham$RANDID %in% id2,1,0)
library(dplyr)
####Comparison of Baselinse Characteristics between Missing and Non-Missing Group####

## We compare the baseline characteristics for missing and nonmissing.(At time point 0)
framingham_base <- framingham[framingham$Time==0,]
library(tableone)
vars <- c("Sex", "Age", "SysBp", "CurSmoke","BMI")
tabmiss_chol <- CreateTableOne(vars = vars,strata = "naindi_Chol",data = framingham_base,test = T)
tabmiss_BMI <- CreateTableOne(vars = vars,strata = "naindi_BMI",data = framingham_base,test = T)
print(tabmiss_BMI)



## Baseline Comparison by TotalChol missing and non-missing group
print(tabmiss_chol)
## 0 means non-missing and 1 means missing




