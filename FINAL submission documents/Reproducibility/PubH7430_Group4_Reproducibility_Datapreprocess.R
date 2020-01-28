#############################################################
## Data preprocess for Group4 project
##
## This file mainly has 2 parts. 
## 1. Data cleaning
##    1) including comparing the baseline characteristics of missing data group
##    and non-missing data group (for BMI or TotalChol)
##    2) and comparing the baseline characteristics of cases and
##    control group(MI/stroke or diabetes); 
## 2. Random Samping
##
## Dataset used: framingham_filtered.csv
##               Diabetes_nona_casecontrol.csv
##               MISTRK_nona_casecontrol.csv
########################################################


## Define file path here
path <- "D:/UMN COURSE/3rd_term/PUBH7430/project/"
#############################################################
## Data Cleaning Part I 
## This step is to exclude subjects who only have two measurements 
#  and who had diabetes, MI or stroke at study entry time.
#############################################################
##### Import the raw data into R#####
framingham<- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_framingham.csv"))
library(dplyr)
attach(framingham)
df <- framingham %>% select(c("Obs","RANDID","PrevMI","PrevSTRK","Diabetes","Time"))
n2 <- df %>% group_by(RANDID) %>% count()
## This gives us the total number of subjects in the raw data.
n <- nrow(n2)
n
## Exclude subjects who have only two measurments.
randid <- n2$RANDID[which(n2$n>=3)]
## This is the number of subjects who only has two measurements 
n-length(randid)


## df_final are observations who have 3 measurements.
df_final <- df[which(df$RANDID %in% randid),]
df_final_wide <-reshape(df_final,idvar = "RANDID",timevar = "Time",
                        direction = "wide")
df_final_wide <- df_final_wide %>% select(c("RANDID","PrevMI.0","PrevSTRK.0","Diabetes.0"))
df_final_wide <- df_final_wide[which(df_final_wide$PrevMI.0==0 &df_final_wide$PrevSTRK.0==0 &df_final_wide$Diabetes.0==0),]

framingham_filtered <- framingham[which(df$RANDID %in% randid),]
framingham_filtered <- framingham_filtered[which(framingham_filtered$RANDID %in% df_final_wide$RANDID),]

## check if we excluded the right thing
dd <- framingham_filtered
length(which(dd$PrevMI[which(dd$Time==0)]==1))
length(which(dd$PrevSTRK[which(dd$Time==0)]==1))
length(which(dd$Diabetes[which(dd$Time==0)]==1))

## This generated the dataset that has no prevelance of 
## diabetes, MI or stroke at study entry and all participants have 3 measurements. 
## You don't have to run this line.
## write.csv(framingham_filtered,file = paste0(path,"/framingham_filtered.csv")


##############################################################
## Missing and Non-missing group comparison
## This step is to compare the baseline characteristics for BMI missing and nonmissing subjects 
## and TotalChol missing and nonmissing subjects.
################################################################

framingham<- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_framingham_filtered.csv"),header = T,na.strings = ".")
framingham[,c("Sex","PrevCHD","PrevAP","PrevHYP")] <- lapply(framingham[,c("Sex","PrevCHD","PrevAP","PrevHYP")],factor)

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
print(tabmiss_chol)
print(tabmiss_BMI)


## However we did not take into consideration of
## which time point the value of those two variables are missed.


#############################################################
## Data Cleaning Part II 
## This step is to exclude subjects who had missing BMI 
## and TotalChol values.
#############################################################
framingham<- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_framingham_filtered.csv"),header = T,na.strings = ".")
#framingham$Time <- round(framingham$Time/365,2)
## approximately recode time as year0, year6 and year 12.
#framingham$Time <- rep(c(0,6,12),times = 3108)

## we exclude observations whose TotalChol (342), CigPDay (57), BMI (29 - can be interpolated based on other values within cluster), BPMeds (531),  Glucose (1200) have missing values.
id1 <- unique(framingham$RANDID[which(is.na(framingham$TotalChol),arr.ind = T)])
id2 <- unique(framingham$RANDID[which(is.na(framingham$BMI),arr.ind = T)])

ID <- unique(c(id1,id2))
framingham_noNA <- framingham[!(framingham$RANDID %in% ID),]
length(unique(framingham_noNA$RANDID))

## check the na.omitted dataset.
which(is.na(framingham_noNA$BMI))
## after this we still have 1661 clusteres which is too much. Thus we implemented random sampling from the randomID.

# You do not need to run this line
# write.csv(framingham_noNA,file = "/Users/chanter/Desktop/Correlated Data/framingham_nonafiltered.csv")




##########################################################################
# This step is to use case-control sampling to generates datasets for analysis.
# Because we did not set seed in this step, so you might get different dataset from ours.
# We suggest you to skip this step (random sample part) and use the datasets we offered to do model construction.
###########################################################################

## Filter the dataset to exclude the subjects who have missing values in BMI and totalChol variables.
## That's framingham_nona_filtered
framingham_nona <- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_framingham_nonafiltered.csv"))

idMI.STRK <- unique(framingham_nona$RANDID[which(framingham_nona$PrevSTRK==1 | framingham_nona$PrevMI==1)],arr.ind = T)

id.Dia <- unique(framingham_nona$RANDID[which(framingham_nona$Diabetes==1)],arr.ind = T)
## Extracting the Cases
framingham_diabetes<- framingham_nona[which(framingham_nona$RANDID %in% id.Dia),]
framingham_MISTRK <- framingham_nona[which(framingham_nona$RANDID %in% idMI.STRK),]

## Randomly take samples from the control.
n.diab <- nrow(framingham_diabetes)/3
n.MISTRK <- nrow(framingham_MISTRK)/3

framingham_diabetes.control<- framingham_nona[!(framingham_nona$RANDID %in% id.Dia),]
framingham_MISTRK.control <- framingham_nona[!(framingham_nona$RANDID %in% idMI.STRK),]


idx<- sample(unique(framingham_diabetes.control$RANDID),n.diab)
control.diabe <- framingham_diabetes.control[(framingham_diabetes.control$RANDID %in% idx),]
idx <- sample(unique(framingham_MISTRK.control$RANDID),n.MISTRK)
control.MISTRK <- framingham_MISTRK.control[(framingham_MISTRK.control$RANDID %in% idx),]

##Make the dataset there are diabetes cases and control withouth missing values of BMI and Totalcholestral. 
Diabetes_nona_casecontrol <- as.data.frame(rbind(control.diabe,framingham_diabetes))

## The dataset there are MI or STROKE cases and control without missing values.
MISTRK_nona_casecontrol <- as.data.frame(rbind(control.MISTRK,framingham_MISTRK))
MISTRK_nona_casecontrol$MISTRK <- ifelse(MISTRK_nona_casecontrol$PrevSTRK==1 | MISTRK_nona_casecontrol$PrevMI==1,1,0)

#write.csv(MISTRK_nona_casecontrol,paste0(path,"/MISTRK_nona_casecontrol.csv"))

#write.csv(Diabetes_nona_casecontrol,paste0(path,"/Diabetes_nona_casecontrol.csv"))
