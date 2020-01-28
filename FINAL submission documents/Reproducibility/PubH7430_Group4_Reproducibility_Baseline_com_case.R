################################################################
## 
## This file compares the baseline characteristics of case and control
## 
## Dataset used: Diabetes_nona_casecontrol.csv
##               MISTRK_nona_casecontrol.csv
################################################################


## Define file path here
path <- "/Users/chanter/Downloads"

diabetes <- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_Diabetes_nona_casecontrol.csv"))
MISTRK <- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_MISTRK_nona_casecontrol.csv"))
diabetes$time <- rep(c(0,6,12),times = nrow(diabetes)/3)
diabetes <- diabetes[,c("Age","BMI","TotalChol","CurSmoke","Sex","SysBp","Diabetes","time","RANDID")]
diabetes.wide <- reshape(diabetes,v.names = "Diabetes",timevar = "time",idvar = "RANDID",direction = "wide")
diabetes.wide$diaind <- ifelse(diabetes.wide$Diabetes.12==1|diabetes.wide$Diabetes.6==1,1,0)
library(tableone)
diabetes.wide$Sex <- factor(diabetes.wide$Sex)
diabetes.wide$CurSmoke <- factor(diabetes.wide$CurSmoke)
tab_diabetes <- CreateTableOne(vars = c("Age","BMI","TotalChol","CurSmoke","Sex","SysBp"),strata = "diaind",data = diabetes.wide,test = T)
print(tab_diabetes)

MISTRK$time <- rep(c(0,6,12),times = nrow(MISTRK)/3)
MISTRK <- MISTRK[,c("Age","BMI","TotalChol","CurSmoke","Sex","SysBp","MISTRK","time","RANDID")]
MISTRK.wide <- reshape(MISTRK,v.names = "MISTRK",timevar = "time",idvar = "RANDID",direction = "wide")
MISTRK.wide$MSind <- ifelse(MISTRK.wide$MISTRK.6==1|MISTRK.wide$MISTRK.12==1,1,0)
MISTRK.wide$Sex <- factor(MISTRK.wide$Sex)
MISTRK.wide$CurSmoke <- factor(MISTRK.wide$CurSmoke)
tab_MI <- CreateTableOne(vars = c("Age","BMI","TotalChol","CurSmoke","Sex","SysBp"),strata = "MSind",data = MISTRK.wide,test = T)
print(tab_MI)


