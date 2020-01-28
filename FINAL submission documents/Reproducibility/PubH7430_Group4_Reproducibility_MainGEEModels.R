#####################################################################
## PubH 7430 Project - Group 4
## main codes for GEE models 
## 
## Including: 
## 1. 6 GEE models with logistic link identity function 
##    with AR-1 correlation structure
## 2. 6 GEE models with logistic link identity function 
##    with exchangeable correlation structure for sensitivity analysis
## 3. Primary outcome of interest: MI/Stroke
##    Secondary outcome of interest: Diabetes
## 4. Primary predictor of interest: Age
##  
#####################################################################


#####################################################################
## load packages
require(ggplot2)
library(geepack)
library(doBy)

## Define file path here
path <- "D:/UMN COURSE/3rd_term/PUBH7430/project/"


######################################################################
## Effects of age, smoking status and gender on the 
## incidence of stroke or MI
######################################################################


## import data
mistrk <- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_MISTRK_nona_casecontrol.csv"))
# Model 1 - no interaction with AR1
gee_i = geeglm(formula = MISTRK~Age + BMI + TotalChol + SysBp,data=mistrk, id=RANDID, corstr="ar1",
               family=binomial(link="logit"))

# summarize the model results
summary(gee_i)
exp(coef(gee_i))

est <- esticon(gee_i, diag(5))
exp(est)


# Model 2 - Age*CurSmoke interaction with AR1
mistrk$CurSmoke <- factor(mistrk$CurSmoke)

gee_ii = geeglm(formula = MISTRK~Age*CurSmoke + BMI + TotalChol + SysBp, data=mistrk, id=RANDID, corstr="ar1",
                family=binomial(link="logit"))

# summarize the model results
summary(gee_ii)
exp(coef(gee_ii))
est <- esticon(gee_ii, diag(7))
exp(est)
est
# Model 3 - Age*Sex interaction with AR1
mistrk$Sex <- ifelse(mistrk$Sex == 1, 0, 1)
mistrk$Sex <- factor(mistrk$Sex)

gee_iii = geeglm(formula = MISTRK~Age*Sex + BMI + TotalChol + SysBp, data=mistrk, id=RANDID, corstr="ar1",
                family=binomial(link="logit"))
# summarize the model results
summary(gee_iii)
exp(coef(gee_iii))
exp(esticon(gee_iii, diag(7)))
esticon(gee_iii, diag(7))

#####################################################################
## Effects of age smoking status and gender on the incidence of diabetes
######################################################################

diabetes <- read.csv(paste0(path,"/PubH7430_Group4_Reproducibility_Diabetes_nona_casecontrol.csv"))

#Model 4 - no interaction with AR1
gee_iv = geeglm(formula = Diabetes~Age + BMI + TotalChol + SysBp,data=diabetes, id=RANDID, corstr="ar1",
               family=binomial(link="logit"))
# summarize the model results
summary(gee_iv)
esticon(gee_iv, diag(5))
exp(coef(gee_iv))
exp(esticon(gee_iv, diag(5)))


#Model 5 - Age*CurSmoke interaction with AR1
diabetes$CurSmoke <- factor(diabetes$CurSmoke)

gee_v = geeglm(formula = Diabetes~Age*CurSmoke + BMI + TotalChol + SysBp, data=diabetes, id=RANDID, corstr="ar1",
                family=binomial(link="logit"))
# summarize the model results
summary(gee_v)
exp(coef(gee_v))
exp(esticon(gee_v, diag(7)))
esticon(gee_v, diag(7))

#Model 6 - Age*Sex interaction with AR1
diabetes$Sex <- ifelse(diabetes$Sex == 1, 0, 1)
diabetes$Sex <- factor(diabetes$Sex)

gee_vi = geeglm(formula = Diabetes~Age*Sex + BMI + TotalChol + SysBp, data=diabetes, id=RANDID, corstr="ar1",
                 family=binomial(link="logit"))
# summarize the model results
summary(gee_vi)
exp(coef(gee_vi))
exp(esticon(gee_vi, diag(7)))
esticon(gee_vi, diag(7))



#####################################################################
## Sensitivity analysis: 
## Use exchangeable working correlation structure
## We did not see many differences in results between the GEE models 
## with these two different correlation structures.
######################################################################

# Model 1b - no interaction with exchangeable structure
gee_ib = geeglm(formula = MISTRK~Age + BMI + TotalChol + SysBp,data=mistrk, id=RANDID, corstr="exchangeable",
               family=binomial(link="logit"))
# summarize the model results
summary(gee_ib)
exp(coef(gee_ib))

est <- esticon(gee_ib, diag(5))
exp(est)


# Model 2b - Age*CurSmoke interaction with exchangeable structure
gee_iib = geeglm(formula = MISTRK~Age*CurSmoke + BMI + TotalChol + SysBp, data=mistrk, id=RANDID, corstr="exchangeable",
                family=binomial(link="logit"))
# summarize the model results
summary(gee_iib)
exp(coef(gee_iib))
est <- esticon(gee_iib, diag(7))
exp(est)
est

# Model 3b - Age*Sex interaction with exchangeable structure
gee_iiib = geeglm(formula = MISTRK~Age*Sex + BMI + TotalChol + SysBp, data=mistrk, id=RANDID, corstr="exchangeable",
                 family=binomial(link="logit"))
# summarize the model results
summary(gee_iiib)
exp(coef(gee_iiib))
exp(esticon(gee_iiib, diag(7)))
esticon(gee_iiib, diag(7))

#Model 4b - no interaction with exchangeable structure
gee_ivb = geeglm(formula = Diabetes~Age + BMI + TotalChol + SysBp,data=diabetes, id=RANDID, corstr="exchangeable",
                family=binomial(link="logit"))
# summarize the model results
summary(gee_ivb)
exp(coef(gee_ivb))
exp(esticon(gee_ivb, diag(5)))
esticon(gee_ivb, diag(5))


#Model 5b - Age*CurSmoke interaction with exchangeable structure
gee_vb = geeglm(formula = Diabetes~Age*CurSmoke + BMI + TotalChol + SysBp, data=diabetes, id=RANDID, corstr="exchangeable",
               family=binomial(link="logit"))
# summarize the model results
summary(gee_vb)
exp(coef(gee_vb))
exp(esticon(gee_vb, diag(7)))
esticon(gee_vb, diag(7))

#Model 6b - Age*Gender interaction with exchangeable structure
gee_vib = geeglm(formula = Diabetes~Age*Sex + BMI + TotalChol + SysBp, data=diabetes, id=RANDID, corstr="exchangeable",
                family=binomial(link="logit"))

# summarize the model results
summary(gee_vib)
exp(coef(gee_vib))
exp(esticon(gee_vib, diag(7)))
esticon(gee_vib, diag(7))

