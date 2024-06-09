# Load Libraries 
library(robumeta)
library(metafor)
library(dplyr)
library(devtools)
library(sessioninfo)
library(clubSandwich)
library(nlme)
library(vioplot)
library(lmtest)
library(memisc)
library(car)
library(carData)
library(stargazer)
library(nnet)
library(class)
library(timeDate)
library(caret)
library(rgl)
library(sandwich)
library(survival)
library(foreign)
library(Formula)
library(AER)
library(dummies)
library(BMS)
# Load data
mdata <- read.csv('updated_mrwp_mdata_prefinal.csv',header=T,sep=",")
# Quick check with the data
dim(mdata)
summary(as.factor(mdata$form))
head (mdata);str(mdata)
# Isolate price elasticity estimates 
pem=subset(mdata, y_p_d=="P")
head(pem);str(pem)
names(pem)
dim(pem)
# Isolate income elasticity estimates 
iem=subset(mdata,y_p_d=="Y")
head(iem);str(iem)
# Isolate people elasticity estimates 
#hem=subset(mdata,y_p_d=="D")
#head(hem);str(hem)
#removing na values from the entire dataset
#mdatan.na.omit<-na.omit(mdata)
#mdatan.na.omit
#to make the names shorter#
#mdatan<-na.omit(mdata)
#mdatan
### creating a subset for each elasticity type with na's removed ###
#pemn=subset(mdatan,y_p_d=="P")
#iemn=subset(mdatan,y_p_d=="Y")
#hemn=subset(mdatan,y_p_d=="D")

#str(pemn$study_id)
#str(pemn)
#pemn$ID <- as.factor(pemn$study_id)

# 1) perform robu test

## PE ##
mp1 <- robu (formula=est~1, data =pem,studynum = study_id, 
             var.eff.size =var,modelweights = "CORR", rho = .8,small =FALSE)
print(mp1)

#model weights is the user-specified option for selecting either the hierarchical# 
#(HIER) or correlated (CORR) effects model                                       #        
#pe_intercept2 <- robu (formula=est~1, data =pemn,modelweights = "HIER",  
#studynum = study_id, var.eff.size =se, small =FALSE)                     
#print(pe_intercept2)

###############################################################################
###############################################################################

##Weighted average for top 10 most precise estimate
tdatap <- read.csv('top_ten_estimates_PE.csv',header=T,sep=",")                 #
getwd()
head (tdatap);str(tdatap)                                                  
topp <- robu (formula=est~1, data =tdatap,studynum = study_id,                  #
              var.eff.size =var,modelweights = "CORR", rho = .8,small =FALSE)
print(topp)
summary(tdatap$est)                                                             #   
##Publication bias
topbp <- robu (formula=est~1+se, data =tdatap,studynum = study_id, 
               var.eff.size =var,modelweights = "CORR", rho = .8,small =FALSE)  # 
print(topbp)

##not detected

##Weighted average for top 5 most precise studies

tfivep <- read.csv('top_five_studies_PE.csv',header=T,sep=",")                  # 
getwd()
head (tfivep);str(tfivep)                                                  
topfp <- robu (formula=Av_est~1, data =tfivep,studynum = study_id,              #
               var.eff.size =var,modelweights = "CORR", rho = .8,small =FALSE)
print(topfp)
summary(tfivep$est)  

###############################################################################
################################################################################



## IE ##
mi1 <- robu (formula=est~1, data =iem, studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mi1)
###############################################################################
##Weighted average for top 10 most precise estimate

tdatai <- read.csv('top_ten_estimates_IE.csv',header=T,sep=",")                           #
getwd()
head (tdatai);str(tdatai)                                                  
topi <- robu (formula=est~1, data =tdatai,studynum = study_id,                 #
              var.eff.size =var,modelweights = "CORR", rho = .8,small =FALSE)
print(topi)
summary(tdatai$est)                                                                
##Publication bias
topbi <- robu (formula=est~1+se, data =tdatai,studynum = study_id, 
               var.eff.size =var,modelweights = "CORR", rho = .8,small =FALSE) # 
print(topbi)


###############################################################################

## HHE ##
mh1 <- robu (formula=est~1, data =hem, studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mh1)


## 2)perform a publication bias test (1) using intercept and SE ##

## PE ## 

mp2 <- robu (formula=est~1+se, data =pem,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mp2)


## IE ##
mi2 <- robu (formula=est~1+se, data =iem,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mi2)

## HHE ##
mh2 <- robu (formula=est~1+se, data =hem,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mh2)

##No evidence of bias ##


## 3) Publication bias corrected (1) ##
## using the formula est= intercept+var
## var=se^2

## PE ## 

mp3 <- robu (formula=est~1+I(se^2), data =pem,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mp3)

## IE ##
mi3 <- robu (formula=est~1+I(se^2), data =iem,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mi3)

## HHE ##
mh3 <- robu (formula=est~1+I(se^2), data =hemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mh3)

## 4) publication bias corrected (2):
##compute adequate power by comparing the standard error of each estimate to 
#the absolute value of this fixed-effect weighted average (WLS-FE) divided by 
#2.8.If the standard error is less than this threshold, we can conclude that 
#the estimate is adequately powered to detect an effect size suggested by 
#the weighted average of all estimates in this area of research
# so for PE 0.343 /2.8= 0.1225, if SE<  0.1225 we consider for the analysis
## PE ##
#0.348/2.8
#SEP1=subset(pemn,se<0.1225)
0.343/2.8
SEP1=subset(pem,se< 0.1225)
#View(SEP1)
mp4 <- robu (formula=est~1, data =SEP1,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mp4)


## IE ##
## for income the threshold is   0.17/2.8=0.06071429
0.17/2.8
SEI1=subset(iem,se<0.06071429)
#View(SEI1)
mi4 <- robu (formula=est~1, data =SEI1,studynum = study_id, 
             var.eff.size =var, rho = .8,small =FALSE)
print(mi4)
## Dealing with endogeneity
# using the plm package
library(plm);library(panelr)
head(pem)
#fitting the FE estimator
tp<-pdata.frame(pem,index=c("study_id"))

fe1<-plm(est~1+se,data=tp,model="within")
summary(fe1)
#      Estimate Std. Error   t-value  Pr(>|t|)    
#se  -0.897650   0.084435  -10.631   < 2.2e-16 ***


# r drops the intercept for the within-estimator, but not Stata, I think 

## The weighted FE-using invers_noestmate as a weighting scheme

fe2<-plm(est~1+se,data=tp,weights =invers_noestmate,model="within")
summary(fe2)

## The weighted FE-using invers_var as a weighting scheme

fe3<-plm(est~1+se,data=tp,weights =var,model="within")
summary(fe3)

## Fitting IV regression
head(pem)
ols1<-lm(est~1+se, data = pem)
summary(ols1)
ivreg1<-ivreg(est~se|invrs_sqrt_n,data = pem, weights=invers_noestmate, cluster(study))
summary(ivreg1)

##P-uniform*
#install.packages("puniform")
library(puniform)
library(metafor)
#install.packages("dmetar")
#library(dmetar)
library(haven)
library(tidyverse)
library(meta)
?puni_star

puni_star(yi = pem$est, vi = pem$var, side="left", 
method="ML",alpha = 0.05, control=list( max.iter=1000,tol=0.1,reps=10000, 
int=c(0,2), verbose=TRUE))
# alternative
puni_star(tobs = pem$tstat, ni = pem$samplesize, alpha = 0.05, side =
"left", method = "ML", control=list(stval.tau=0.5, max.iter=10000,tol=0.1,
reps=10000, int=c(0,2), verbose=TRUE))

#alternatively

### P-uniform
resp3 <- puniform(yi = pem$est, vi = pem$var, side = "left",method = "LNP")

print(resp3)

### P-uniform*
resp4 <- puni_star(yi = pem$est, vi = pem$var, side = "right", method = "ML",
                   alpha = .05,boot = FALSE)
print(resp4)

#puni_star(yi = data$discrate_med, vi = data$variance_med, side="right", 
#method="ML",alpha = 0.05, control=list( max.iter=1000,tol=0.1,reps=10000, 
#int=c(0,2), verbose=TRUE))
# alternative
#puni_star(tobs = data$tstat_med, ni = data$nobs_med, alpha = 0.05, side =
#"right", method = "ML", control=list(stval.tau=0.5, max.iter=10000,tol=0.1,
#reps=10000, int=c(0,2), verbose=TRUE))

###############################################################################

#Fitting Three-Level Meta-Analysis Models in R
##Creating est.id
#Price Elasticity
head(pem)
pem$est.id <- seq_along(pem[,1])
head(pem)
tail(pem)
three.p1 <- rma.mv(yi = est, 
                     V = var, 
                     slab = study,
                     data = pem,
                     random = ~ 1 | study/est.id, 
                     test = "t", 
                     method = "REML")
summary(three.p1)

##Income elasticity
head(iem)
iem$est.id <- seq_along(iem[,1])
head(iem)
tail(iem)
view(iem)
three.i1 <- rma.mv(yi = est, 
                   V = var, 
                   slab = study,
                   data = iem,
                   random = ~ 1 | study/est.id, 
                   test = "t", 
                   method = "REML")
summary(three.i1)

##Fitting the RVE using set of variables

## Price E ##
##Base (1)
?relevel
glimpse(pem)
head(pem)
colnames(pem)
levels(pem$endoge)
is.factor(pem$endoge)
#declaring as a factor
pem$endoge <- as.factor(pem$endoge) 
pem$form <- as.factor(pem$form) 
pem$data_type <- as.factor(pem$data_type)
pem$a_s_l <- as.factor(pem$a_s_l)
pem$economy <- as.factor(pem$economy)
pem$tot_in_out <- as.factor(pem$tot_in_out)
pem$income <- as.factor(pem$income)
pem$tariff <- as.factor(pem$tariff)
summary(as.factor(pem$data_collection_collapsed))
pem$data_collection <- as.factor(pem$data_collection_collapsed)
summary(as.factor(pem$scale_study))
pem$scale_study <- as.factor(pem$scale_study)
pem$rain_tem <- as.factor(pem$rain_tem)
summary(as.factor(pem$publication_type))
pem$publication_type <- as.factor(pem$publication_type)


#eg. to set reference levels
#warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
endogp<-relevel(pem$endoge,ref = "yes")
summary(as.factor(pem$form))
formp<-relevel(pem$form,ref ="Stone-Geary")
data_typep<-relevel(pem$data_type,ref = "Panel")
a_s_lp<-relevel(pem$a_s_l,ref = "A")
economyp<-relevel(pem$economy,ref = "lower middle")
tot_in_outp<-relevel(pem$tot_in_out,ref = "T")
incomep<-relevel(pem$income,ref="income")
#summary(as.factor(pem$tariff))
tarifp<-relevel(pem$tariff,ref="increasing")
#summary(as.factor(pem$data_collection))
datafreqp<-relevel(pem$data_collection,ref="annual")
#summary(as.factor(pem$scale_study))
datascalep<-relevel(pem$scale_study,ref="aggregated")
#summary(as.factor(pem$rain_tem))
climatep<-relevel(pem$rain_tem,ref="rt")
publicationp<-relevel(pem$publication_type,ref="published")


##including all variables
Model.P1 <- robu(formula=est~endogp+formp+tot_in_outp+economyp+
                data_typep+a_s_lp+incomep+tarifp+datafreqp+
                  datascalep+climatep ,
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(Model.P1)

##Income as a continous variable

Model.P2 <- robu(formula=est~endogp+formp+tot_in_outp+
                   data_typep+a_s_lp+incomep+tarifp+datafreqp+
                   datascalep+climatep+log(income19) ,
                 data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                   "CORR",rho = .8, small =FALSE)
print(Model.P2)

### trying iterations before the base model


RVEP.T1 <- robu(formula=est~endogp+formp+tot_in_outp+economyp+
                data_typep+a_s_lp+incomep+datascalep+climatep+tarifp,
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEP.T1)
##################################################################
library(metafor)
library(ggplot2)
library(MuMIn)
install.packages("dmetar")
library(dmetar)

?MuMIn

# Example 1: Perform multimodel inference with default settings

colnames(pem)
VS.1 = multimodel.inference(TE = 'est', seTE = 'se', data = pem,
                                                       predictors = c('income_pc', 'endoge', 'form',
                                                                     'income19','data_type', 'a_s_l'))
            

print(VS.1)

RVEP1 <- robu(formula=est~endogp+formp+tot_in_outp+economyp+
                data_typep+a_s_lp+incomep,
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEP1)


################################################################################
#################################################################################

P.In1 <- robu(formula=est~endogp+formp+tot_in_outp+
                data_typep+a_s_lp+incomep+income19+
                scale_study+data_collection+drought+tariff,
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(P.In1)
## Checking the effect of 2019-income on price elasticity
head(pem)
P.In2<- robu(formula=est~log(income19),
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(P.In2)
#no effect

## Checking the effect of 2019-income on income elasticity
Y.In1<- robu(formula=est~log(income19),
             data=iem,studynum = study_id,var.eff.size =var,modelweights = 
               "CORR",rho = .8, small =FALSE)
print(Y.In1)
##no effect

P.In3<- robu(formula=est~log(income19)+tot_in_outp+a_s_lp+endogp+formp+tariff,
             data=pem,studynum = study_id,var.eff.size =var,modelweights = 
               "CORR",rho = .8, small =FALSE)
print(P.In3)

P.In4 <- robu(formula=est~endogp+formp+tot_in_outp+
                data_typep+a_s_lp+incomep+log(income19),
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(P.In4)

Inc.means <- tapply(pem$income19,pem$economy,mean)

## try above and below median income: 1 if above med; 0 otherwise   
median(pem$income19)

pem$incomenew <- ifelse(pem$income19>57530.3, 1, 0)
summary(pem$incomenew)

P.In5 <- robu(formula=est~endogp+formp+tot_in_outp+
                data_typep+a_s_lp+incomep+incomenew,
              data=pem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(P.In5)

################################################################################
################################################################################

##Testing the functional forms: non-SG are equal
library(clubSandwich)
## Testing with the full model--RVEP1          
print(RVEP1)
F1 <- rbind(c(0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0)   #beta3 =beta4
            +c(0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0))  #beta3 =beta5

Wald_test(RVEP1, constraints = F1, vcov="CR2")


#conclude: do not reject the null that the coefficents are equal
#Test     F        df_num       d.f.    p.val
#HTZ    0.000605      1      3.3    0.982 

?Wald_test

#  create new SG variable for the Stone-Geary functional forms
pem$SG <- ifelse(pem$form == "Stone-Geary", 0, 1)
#  Check it has been added
summary(pem$SG)

RVEP1.test1 <-robu(formula=est~endogp+SG+tot_in_outp+economyp+
                     data_typep+a_s_lp+incomep,data=pem,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
print(RVEP1.test1)

## has an effect for SG and everything else as a group

## testing data type: non-panel are equal

## Testing for the full model--RVEP1          
print(RVEP1)
DT <- rbind(c(0,0,0,0,0,0,0,0,0,0,-1,1,0,0,0,0))   #beta11 =beta12

Wald_test(RVEP1, constraints = DT, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test       F   d.f.    p.val
#HTZ   0.179   32.6  0.675


#  create new PD variable for the panel data types
pem$PD <- ifelse(pem$data_type == "Panel", 0, 1)
#  Check it has been added
summary(pem$PD)

RVEP1.test2 <-robu(formula=est~endogp+formp+tot_in_outp+economyp+
                     PD+a_s_lp+incomep,data=pem,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP1.test2)
# the model has an effect for panel data and everything else as a group

## Testing elasticity estimates: non-short-run are equal
## But it makes sense to test non-long-run coeficents are equal

## Testing for the full model--RVEP1          
#print(RVEP1)
#LR <- rbind(c(0,0,0,0,0,0,0,0,0,0,0,-1,1,0,0))   #beta12 =beta13

#Wald_test(RVEP1, constraints = LR, vcov="CR2")
# we cannot reject the null that the coefficents are equal
# Test    F   d.f.    p.val
#HTZ    1.54  28.4   0.225

#  create new LR variable for the Long-run elasticity estimates
#pemn$LR <- ifelse(pemn$a_s_l == "L", 0, 1)
#  Check it has been added
#summary(pemn$LR)

#RVEP1.test3 <-robu(formula=est~endogp+formp+tot_in_outp+economyp+
#                     data_typep+LR+incomep,data=pemn,studynum = 
#                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
#
#print(RVEP1.test3)
##it has no an effect for long-run estimates and everything else as a group
## so we don't make constraint (no shrinking) to this group, makes sense to keep
## as it is.

## Testing countries: non-lower-middle are equal
## Testing for the full model--RVEP1          
print(RVEP1)
LM <- rbind(c(0,0,0,0,0,0,0,0,-1,1,0,0,0,0,0,0))   #beta9 =beta10

Wald_test(RVEP1, constraints = LM, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test F     d.f.    p.val
#HTZ   2.05   32    0.162 

#  create new LM variable for the lower-middle-income countries
pem$LM <- ifelse(pem$economy == "lower middle", 0, 1)

summary(as.factor(pem$economy))

#  Check it has been added
summary(pem$LM)
RVEP1.test4 <-robu(formula=est~endogp+formp+tot_in_outp+LM+
                     data_typep+a_s_lp+incomep,data=pem,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP1.test4)
## the model has an effect for lower-middle-income countries and for the group

## Testing income data: non-income data are equal

## Testing for the full model--RVEP1          
print(RVEP1)
IN <- rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,1))   #beta15 =beta16

Wald_test(RVEP1, constraints = IN, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test   F   d.f.   p.val
#HTZ  1.86  58.9 0.178

#  create new IN variable for the income data--when income is directly used
pem$IN <- ifelse(pem$income == "income", 0, 1)
#  Check it has been added
summary(pem$IN)

RVEP1.test5 <-robu(formula=est~endogp+formp+tot_in_outp+economyp+
                     data_typep+a_s_lp+IN,data=pem,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
print(RVEP1.test5)

##it has an effect for income data and for everything else as a group.

##Pooling the shrinking model
RVEPs <-robu(formula=est~endogp+SG+tot_in_outp+LM+
               PD+a_s_lp+IN,data=pem,studynum = 
               study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEPs)

#                Estimate StdErr t-value dfs  P(|t|>) 95% CI.L 95% CI.U Sig
 #X.Intercept.    -0.3591 0.1209  -2.970 161 0.003433 -0.59790  -0.1203 ***
  #    endogpno    0.1327 0.0616   2.156 161 0.032566  0.01116   0.2543  **
   #       SG     -0.2021 0.0568  -3.560 161 0.000487 -0.31415  -0.0900 ***
#  tot_in_outpI    0.0481 0.1311   0.367 161 0.714058 -0.21081   0.3071    
# tot_in_outpO    -0.3870 0.1625  -2.382 161 0.018384 -0.70787  -0.0662  **
#            LM    0.1711 0.0929   1.843 161 0.067230 -0.01228   0.3545   *
#           PD    -0.1456 0.0647  -2.250 161 0.025807 -0.27340  -0.0178  **
#       a_s_lpL   -0.0930 0.0942  -0.987 161 0.325163 -0.27910   0.0931    
#      a_s_lpS     0.0744 0.0591   1.258 161 0.210160 -0.04236   0.1911    
#           IN     0.0999 0.0497   2.011 161 0.046031  0.00178   0.1981  **

######################
######################

## getting the price elasticity values for each category 
endogp<-relevel(pemn$endoge,ref = "yes")
formp<-relevel(pemn$form,ref ="Stone-Geary")
data_typep<-relevel(pemn$data_type,ref = "Panel")
economyp<-relevel(pemn$economy,ref = "lower middle")
#economyp<-relevel(pemn$economy,ref = "high")
#economyp<-relevel(pemn$economy,ref = "upper middle")
incomep<-relevel(pemn$income,ref="income")

#pemn$LM <- ifelse(pemn$economy == "lower middle", 0, 1)# for lower-middle-income countries
#pemn$UM<-ifelse(pemn$economy=="upper middle", 0,1) # for upper-middle-income countries
#summary(pemn$UM)
#pemn$HI<-ifelse(pemn$economy=="high", 0,1) # for high-income countries 
#summary(pemn$HI)

#  Check it has been added
summary(pemn$LM)

# data type
pemn$PD <- ifelse(pemn$data_type == "Panel", 0, 1)
pemn$TD <- ifelse(pemn$data_type == "Time", 0, 1)
pemn$CSD <- ifelse(pemn$data_type == "Cross", 0, 1)

# Income data
pemn$IN <- ifelse(pemn$income == "income", 0, 1)
pemn$INP <- ifelse(pemn$income == "proxy_income", 0, 1)
pemn$INN <- ifelse(pemn$income == "no", 0, 1)


pemn$LM <- ifelse(pemn$economy == "lower middle", 0, 1)

pemn$HUM <- ifelse(pemn$economy == "lower middle", 1, 0)

summary(as.factor(pemn$economy))

# first change the base as 
## ID V OD
tot_in_outp<-relevel(pemn$tot_in_out,ref = "T")
tot_in_outp<-relevel(pemn$tot_in_out,ref = "O")
tot_in_outp<-relevel(pemn$tot_in_out,ref = "I")

##LR V SR
a_s_lp<-relevel(pemn$a_s_l,ref = "A")
a_s_lp<-relevel(pemn$a_s_l,ref = "L")
a_s_lp<-relevel(pemn$a_s_l,ref = "S")

RVEPb <-robu(formula=est~endogp+SG+tot_in_outp+LM+
               PD+a_s_lp+IN,data=pemn,studynum = 
               study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEPb)

## then grab the intercept value for it to get the elasticity estimate
#Or it could be calculated as -0.8391+0.1711 for lower-middle income countries
# for outdoor short-run demand

##############################################################################

## Bublication bias test

#Publication bias detection using SE (2)
RVEP2 <-robu(formula=est~endogp+SG+tot_in_outp+LM+
               PD+a_s_lp+IN+se,data=pem,studynum = 
               study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP2)
##         Estimate  StdErr   t-value    dfs   P(|t|>)   95% CI.L  95% CI.U  Sig
##se      -0.9910    0.2338  -4.239    164   0.0000374  -1.45266  -0.5294    ***

##Publication bias has been detected

##Correction 1
## Publication bias correction using Stanley and Doucouliagos (2014), using the var

RVEP3 <-robu(formula=est~endogp+SG+tot_in_outp+LM+
               PD+a_s_lp+IN+I(se^2),data=pem,studynum = 
               study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP3)   

##Getting elasticity values for correction 1 model

## ID Vs OD
tot_in_outp<-relevel(pem$tot_in_out,ref = "T")
tot_in_outp<-relevel(pem$tot_in_out,ref = "O")
tot_in_outp<-relevel(pem$tot_in_out,ref = "I")

##LR V SR
a_s_lp<-relevel(pem$a_s_l,ref = "A")
a_s_lp<-relevel(pem$a_s_l,ref = "L")
a_s_lp<-relevel(pem$a_s_l,ref = "S")


pem$LM <-ifelse(pem$economy=="lower middle", 0,1) # for lower-middle-income countries
pem$HUM<-ifelse(pem$economy=="lower middle", 1,0) # for high and upper-middle-income countries
summary(pem$HUM)


RVEP3 <-robu(formula=est~endogp+SG+tot_in_outp+LM+
                PD+a_s_lp+IN+I(se^2),data=pem,studynum = 
                study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
print(RVEP3)  


##Correction 2
## Publication bias correction using Ioannidis et al. (2017)
## Here I use the the coefficent estimate of the intercept from the the intrecpet only 
## model (mp1) to calculate the threshold cut
# so for PE 0.343 /2.8= 0.1225, if SE< 0.1225 we consider for the analysis
## PE ##
#0.343/2.8
#SEP1=subset(pemn,se<0.1225)


0.343/2.8
PEP1=subset(pem,se<0.1232143)


#declaring as a factor

PEP1$endoge <- as.factor(PEP1$endoge) 
PEP1$form <- as.factor(PEP1$form) 
PEP1$data_type <- as.factor(PEP1$data_type)
PEP1$a_s_l <- as.factor(PEP1$a_s_l)
PEP1$economy <- as.factor(PEP1$economy)
PEP1$tot_in_out <- as.factor(PEP1$tot_in_out)
PEP1$income <- as.factor(PEP1$income)
PEP1$tariff <- as.factor(PEP1$tariff)
summary(as.factor(PEP1$data_collection_collapsed))
PEP1$data_collection <- as.factor(PEP1$data_collection_collapsed)
summary(as.factor(PEP1$scale_study))
PEP1$scale_study <- as.factor(PEP1$scale_study)
PEP1$rain_tem <- as.factor(PEP1$rain_tem)
summary(as.factor(PEP1$publication_type))
PEP1$publication_type <- as.factor(PEP1$publication_type)


endogp2<-relevel(PEP1$endoge,ref = "yes")
formp2<-relevel(PEP1$form,ref ="Stone-Geary")
data_typep2<-relevel(PEP1$data_type,ref = "Panel")
a_s_lp2<-relevel(PEP1$a_s_l,ref = "A")
economyp2<-relevel(PEP1$economy,ref = "lower middle")
tot_in_outp2<-relevel(PEP1$tot_in_out,ref = "T")
incomep2<-relevel(PEP1$income,ref="income")

RVEP4 <- robu(formula=est~endogp2+formp2+tot_in_outp2+economyp2+
                data_typep2+a_s_lp2+incomep2,
              data=PEP1,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEP4)



##Testing the functional forms: non-SG are equal

## Testing with the full model--RVEP1          
print(RVEP4)
F2 <- rbind(c(0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0)   #beta3 =beta4
            +c(0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0))  #beta3 =beta5

Wald_test(RVEP4, constraints = F2, vcov="CR2")
#conclude: do not reject the null that the coefficents are equal
# Test   F    d.f.    p.val
#HTZ     3.01  2.16    0.215

#  create new SG variable for the Stone-Geary functional forms
PEP1$SG <- ifelse(PEP1$form == "Stone-Geary", 0, 1)
#  Check it has been added
summary(PEP1$SG)

RVEP4.test1 <-robu(formula=est~endogp2+SG+tot_in_outp2+economyp2+
                     data_typep2+a_s_lp2+incomep2,data=PEP1,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
print(RVEP4.test1)

## has no an effect for SG and everything else as a group

## testing data type: non-panel are equal

## Testing for the full model--RVEP4          
print(RVEP4)
DT2 <- rbind(c(0,0,0,0,0,0,0,0,0,0,-1,1,0,0,0,0))   #beta11 =beta12

Wald_test(RVEP4, constraints = DT2, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test    F   d.f.   p.val
#HTZ   2.62 21.1  0.12


#  create new PD variable for the panel data types
PEP1$PD <- ifelse(PEP1$data_type == "Panel", 0, 1)
#  Check it has been added
summary(PEP1$PD)

RVEP4.test2 <-robu(formula=est~endogp2+formp2+tot_in_outp2+economyp2+
                     PD+a_s_lp2+incomep2,data=PEP1,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP4.test2)
# the model has an effect for panel data and everything else as a group

## Testing elasticity estimates: non-short-run are equal
## But it makes sense to test non-long-run coeficents are equal

## Testing for the full model--RVEP4          
#print(RVEP4)
#LR2 <- rbind(c(0,0,0,0,0,0,0,0,0,0,0,-1,1,0,0))   #beta12 =beta13

#Wald_test(RVEP4, constraints = LR2, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test     F    d.f.   p.val
#HTZ    0.133  29.4   0.718

#  create new LR variable for the Long-run elasticity estimates
#PEP1$LR <- ifelse(PEP1$a_s_l == "L", 0, 1)
#  Check it has been added
#summary(PEP1$LR)

#RVEP4.test3 <-robu(formula=est~endogp2+formp2+tot_in_outp2+economyp2+
#                     data_typep2+LR+incomep2,data=PEP1,studynum = 
#                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
#
#print(RVEP4.test3)
##it has no an effect for long-run estimates and everything else as a group
## so we don't make constraint (no shrinking) to this group, makes sense to keep
## as it is.

## Testing countries: non-lower-middle are equal
## Testing for the full model--RVEP4          
print(RVEP4)
LM2 <- rbind(c(0,0,0,0,0,0,0,0,-1,1,0,0,0,0,0,0))   #beta9 =beta10

Wald_test(RVEP4, constraints = LM2, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test    F  d.f.   p.val
#HTZ  5.5   23.8   0.0762

#  create new LM variable for the lower-middle-income countries
PEP1$LM <- ifelse(PEP1$economy == "lower middle", 0, 1)

summary(as.factor(PEP1$economy))

#  Check it has been added
summary(PEP1$LM)
RVEP4.test4 <-robu(formula=est~endogp2+formp2+tot_in_outp2+LM+
                     data_typep2+a_s_lp2+incomep2,data=PEP1,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP4.test4)
## the model has an effect for lower-middle-income countries and for the group

## Testing income data: non-income data are equal

## Testing for the full model--RVEP4          
print(RVEP4)
IN2 <- rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,1))   #beta14 =beta15

Wald_test(RVEP4, constraints = IN2, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test    F  d.f.    p.val
#HTZ   1.61  47.5   0.211

#  create new IN variable for the income data--when income is directly used
PEP1$IN <- ifelse(PEP1$income == "income", 0, 1)
#  Check it has been added
summary(PEP1$IN)

RVEP4.test5 <-robu(formula=est~endogp2+formp2+tot_in_outp2+economyp2+
                     data_typep2+a_s_lp2+IN,data=PEP1,studynum = 
                     study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEP4.test5)
##it has no an effect for income data and for everything else as a group.

##Pooling the shrinking model
RVEPs2 <-robu(formula=est~endogp2+SG+tot_in_outp2+LM+
                PD+a_s_lp2+IN,data=PEP1,studynum = 
                study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)

print(RVEPs2)


#               Estimate StdErr t-value   dfs P(|t|>) 95% CI.L 95% CI.U Sig
#X.Intercept.   -0.3832 0.1179  -3.251 135 0.00145  -0.6164  -0.1501 ***
#endogp2no       0.1160 0.0550   2.109 135 0.03683   0.0072   0.2248  **
#      SG       -0.1332 0.0405  -3.285 135 0.00130  -0.2133  -0.0530 ***
#tot_in_outp2I   0.0417 0.1067   0.391 135 0.69634  -0.1693   0.2527      
#tot_in_outp2O  -0.1085 0.1560  -0.695 135 0.48795  -0.4171   0.2001   
#           LM   0.2163 0.0966   2.240 135 0.02670   0.0254   0.4073  **
#           PD  -0.0865 0.0550  -1.572 135 0.11834  -0.1953   0.0223   
#     a_s_lp2L  -0.0620 0.1140  -0.544 135 0.58739  -0.2875   0.1635    
#     a_s_lp2S   0.0103 0.0612   0.168 135 0.86672  -0.1107   0.1313      
#           IN   0.0798 0.0493   1.620 135 0.10765  -0.0176   0.1772        

##Getting the elasticity values

endogp2<-relevel(PEP1$endoge,ref = "yes")
PEP1$SG <- ifelse(PEP1$form == "Stone-Geary", 0, 1)
PEP1$PD <- ifelse(PEP1$data_type == "Panel", 0, 1)
PEP1$IN <- ifelse(PEP1$income == "income", 0, 1)


# ID V OD
tot_in_outp2<-relevel(PEP1$tot_in_out,ref = "I")
tot_in_outp2<-relevel(PEP1$tot_in_out,ref = "O")
tot_in_outp2<-relevel(PEP1$tot_in_out,ref = "T")

# SR V LR
a_s_lp2<-relevel(PEP1$a_s_l,ref = "S")
a_s_lp2<-relevel(PEP1$a_s_l,ref = "L")
a_s_lp2<-relevel(PEP1$a_s_l,ref = "A")

# Country income group

PEP1$LM1 <- ifelse(PEP1$economy == "lower middle", 0, 1)
PEP1$HUM1 <- ifelse(PEP1$economy == "lower middle", 1, 0)
#PEP1$HI1 <- ifelse(PEP1$economy == "high", 0, 1)

RVEPs3 <-robu(formula=est~endogp2+SG+tot_in_outp2+LM1+
                PD+a_s_lp2+IN,data=PEP1,studynum = 
                study_id,var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
print(RVEPs3)

##ploting the more precise and less precise estimates
dev.off()
#layout(matrix(1:3,ncol=1,nrow = 3) , heights = c(1,1.1,1))
#par(mar = c(4, 4, 1, 0) + 0.5)
par(mfrow=c(2,1))


with(pemn,plot(est,se,pch=20,cex=1.5,col='darkgreen',xlab='Price Elasticity',ylab=
                 'Standard Error',xlim = c(-4,4),ylim = c(0,3))) 
with(PEP1,points(est,se,pch=18,cex=1.5,col='blueviolet')) 
legend('topleft',c('Excluded','Included'),pch=c(20,18),
       pt.cex=1.5,inset=0.04, col=c('darkgreen','blueviolet')) 

## getting the summary statistics
#for the more precise model
str(PEP1)
summary(PEP1)
mean(PEP1$est)
median(PEP1$est)
sd(PEP1$est)
var(PEP1$est)

mean(PEP1$se)
median(PEP1$se)
sd(PEP1$se)
var(PEP1$se)

# for the less precise model
str(pemn)
summary(pemn)
mean(pemn$est)
median(pemn$est)
sd(pemn$est)
var(pemn$est)

mean(pemn$se)
median(pemn$se)
sd(pemn$se)
var(pemn$se)

############### END OF PRICE ELASTICITY MODEL ESTIMATION #####################

###############################################################################
###############################################################################
###############################################################################

#mdata <- read.csv('mrwp_mdata.csv',header=T,sep=",")

#head (mdata);str(mdata)

iemn=subset(mdata,y_p_d=="Y")
head(iemn);str(iemn)
dim(iemn)

#removing na values from the entire dataset
#mdatan.na.omit<-na.omit(mdata)
#mdatan.na.omit
#to make the names shorter#
#mdatan<-na.omit(mdata)
#mdatan
### creating a subset for each elasticity type with na's removed ###
#iemn=subset(mdatan,y_p_d=="Y")

#iem <- read.csv('IE_data.csv',header=T,sep=",")
dim(iemn)
#iemn<-na.omit(iem)
dim(iemn)

##Income

iemn$endoge <- as.factor(iemn$endoge) 
iemn$form <- as.factor(iemn$form) 
iemn$data_type <- as.factor(iemn$data_type)
iemn$a_s_l <- as.factor(iemn$a_s_l)
iemn$economy <- as.factor(iemn$economy)
iemn$tot_in_out <- as.factor(iemn$tot_in_out)
iemn$income <- as.factor(iemn$income)
iemn$tariff <- as.factor(iemn$tariff)
summary(as.factor(iemn$data_collection_collapsed))
iemn$data_collection <- as.factor(iemn$data_collection_collapsed)
summary(as.factor(iemn$scale_study))
iemn$scale_study <- as.factor(iemn$scale_study)
iemn$rain_tem <- as.factor(iemn$rain_tem)
summary(as.factor(iemn$publication_type))
iemn$publication_type <- as.factor(iemn$publication_type)


##Re-leveling factors
endogi<-relevel(iemn$endoge,ref = "yes")
formi<-relevel(iemn$form,ref ="Stone-Geary")
data_typei<-relevel(iemn$data_type,ref = "Panel")
a_s_li<-relevel(iemn$a_s_l,ref = "A")
economyi<-relevel(iemn$economy,ref = "lower middle")
tot_in_outi<-relevel(iemn$tot_in_out,ref = "T")
incomei<-relevel(iemn$income,ref="income")
climatei<-relevel(iemn$rain_tem,ref = "n")
scalei<-relevel(iemn$scale_study,ref = "aggregated")
data_collectioni<-relevel(iemn$data_collection,ref = "annual")
publicationi<-relevel(iemn$publication_type,ref = "published")


RVE.T1 <- robu(formula=est~formi+data_collectioni+climatei+
                publicationi+scalei+tot_in_outi+a_s_li+economyi+endogi,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVE.T1)

RVEI1 <- robu(formula=est~formi+data_collectioni+climatei+
                publicationi,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEI1)
###############################################################################
###############################################################################
IE.I1 <- robu(formula=est~formi+data_collectioni+climatei+
                publicationi+log(income_pc)+tariff,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(IE.I1)

# try above and below median income; 1=if income above med; 0 otherwise
median(iemn$income19)

iemn$incomedum <- ifelse(iemn$income19>53381.4, 1, 0)
summary(iemn$incomedum)
IE.I2 <- robu(formula=est~formi+data_collectioni+climatei+
                publicationi+incomedum,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(IE.I2)

################################################################################
################################################################################

#Price elasticity model
#RVEi1 <- robu(formula=est~endogi+formi+tot_in_outi+economyi+
#             data_typei+a_s_li+incomei,
#           data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
#            "CORR",rho = .8, small =FALSE)
#print(RVEi1)
# testing functional forms: all the non-SG are eqaul
## Testing with the full model--RVEI1          
print(RVEI1)
Fi1 <- rbind(c(0,-1,1,0,0,0,0,0,0,0,0,0)   #beta2 =beta3
             +c(0,0,-1,1,0,0,0,0,0,0,0,0))  #beta3 =beta4

Wald_test(RVEI1, constraints = Fi1, vcov="CR2")
#conclude: do not reject the null that the coefficents are equal
#Test    F  d.f.    p.val
#HTZ  0.377  1.89   0.605   

#  create new SG variable for the Stone-Geary functional forms
iemn$SG <- ifelse(iemn$form == "Stone-Geary", 0, 1)
#  Check it has been added
summary(iemn$SG)

RVEI1.test1 <- robu(formula=est~SG+data_collectioni+climatei+
                      publicationi,
                    data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)
print(RVEI1.test1)

##it has an effect for SG forms and everything else as a group

## data frequency, testing all non-annual are equal
print(RVEI1)
dfi1 <- rbind(c(0,0,0,0,0,-1,1,0,0,0,0,0)   #beta6 =beta7
              +c(0,0,0,0,0,0,-1,1,0,0,0,0))  #beta7 =beta8

Wald_test(RVEI1, constraints = dfi1, vcov="CR2")

#conclude: do not reject the null that the coefficients are equal
#Test    F  d.f.  p.val
#HTZ 0.801  21.6  0.381

#  create new annual variable for the annual data frequency
iemn$AD <- ifelse(iemn$data_collection == "annual", 0, 1)
#  Check it has been added
summary(iemn$AD)

RVEI1.test2 <- robu(formula=est~formi+AD+climatei+
                      publicationi,
                    data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)

print(RVEI1.test2)

##it has an effect for annual data and everything else as a group

## climate variable, testing all non-tr-climate data variables are equal
print(RVEI1)
cvi1 <- rbind(c(0,0,0,0,0,0,0,0,-1,1,0,0)   #beta9 =beta10
              +c(0,0,0,0,0,0,0,0,0,-1,1,0))  #beta10 =beta11

Wald_test(RVEI1, constraints = cvi1, vcov="CR2")
##conclude: do not reject the null that the coefficients are equal
#Test  F      d.f.   p.val
#HTZ   0.306   9.93  0.592 

iemn$CD <- ifelse(iemn$rain_tem == "n", 1, 0)
#  Check it has been added
summary(iemn$CD)

RVEI1.test3 <- robu(formula=est~formi+data_collectioni+CD+
                      publicationi,
                    data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)

print(RVEI1.test3)

##it has an effect for no climate data included and everything else as a group

## checking the country income group
iemn$LM <- ifelse(iemn$economy == "lower middle", 0, 1)# for lower-middle-income countries
summary(iemn$LM)
iemn$UM<-ifelse(iemn$economy=="upper middle", 0,1) # for upper-middle-income countries
summary(iemn$UM)
iemn$HI<-ifelse(iemn$economy=="high", 0,1) # for high-income countries 
summary(iemn$HI)

## Checking other factors
iemn$L <- ifelse(iemn$form == "Linear", 0, 1)
iemn$DD <- ifelse(iemn$data_collection == "annual", 0,1)
publicationi<-relevel(iemn$publication_type,ref = "published")

##pooling the shrinking selection

RVEIP <- robu(formula=est~SG+AD+CD+
                publicationi+LM,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEIP)


#                    Estimate StdErr t-value dfs  P(|t|>) 95% CI.L 95% CI.U Sig
#       .Intercept.   0.2130 0.0728   2.926 119 0.004110   0.0689   0.3572 ***
#                SG   0.1219 0.0316   3.864 119 0.000182   0.0594   0.1844 ***
#                AD  -0.0855 0.0336  -2.542 119 0.012295  -0.1520  -0.0189  **
#                CD  -0.0756 0.0323  -2.338 119 0.021039  -0.1396  -0.0116  **
#   publicationigrey -0.0912 0.0366  -2.491 119 0.014126  -0.1638  -0.0187  **
#               LM   -0.0423 0.0541  -0.782 119 0.435590  -0.1494   0.0648   

# Excluding country-income group

RVEIP <- robu(formula=est~SG+AD+CD+
                publicationi,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEIP)

#                 Estimate StdErr t-value dfs   P(|t|>) 95% CI.L 95% CI.U Sig
#    X.Intercept.    0.1619 0.0394    4.11 120 0.0000724   0.0840  0.23993 ***
#               SG   0.1218 0.0312    3.90 120 0.0001602   0.0599  0.18367 ***
#               AD  -0.0763 0.0327   -2.33 120 0.0213922  -0.1411 -0.01151  **
#               CD  -0.0620 0.0302   -2.05 120 0.0420745  -0.1217 -0.00226  **
# publicationigrey  -0.0881 0.0353   -2.50 120 0.0139456  -0.1580 -0.01820  **

##Bias Test using SE

RVEI2 <- robu(formula=est~SG+AD+CD+
                publicationi+se,
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEI2)
## so publication bias has been detected
#      Estimate    StdErr   t-value   dfs        P(|t|>)      95% CI.L  95% CI.U Sig
# se   1.4679     0.1975    7.43    119     0.0000000000177   1.0769    1.85896 ***


##Bias correction (1) using the variance
iemn$HI <- ifelse(iemn$economy == "high", 0, 1)# for high-income countries
iemn$UM <- ifelse(iemn$economy == "upper middle", 0, 1)# for upper-middle-income countries
iemn$LM <- ifelse(iemn$economy == "lower middle", 0, 1)# for lower-middle-income countries

RVEI3 <- robu(formula=est~SG+AD+CD+
                publicationi+I(se^2),
              data=iemn,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEI3)

#                    Estimate StdErr t-value dfs  P(|t|>) 95% CI.L 95% CI.U Sig
#      X.Intercept.  0.1433 0.0368    3.89 119 0.000163   0.0704  0.21622 ***
#               SG   0.1059 0.0312    3.39 119 0.000940   0.0441  0.16779 ***
#               AD  -0.0632 0.0290   -2.17 119 0.031631  -0.1207 -0.00565  **
#               CD  -0.0510 0.0287   -1.78 119 0.078056  -0.1078  0.00581   *
#  publicationigrey -0.0822 0.0331   -2.48 119 0.014411  -0.1477 -0.01665  **
#           I.se.2.  1.8163 0.5528    3.29 119 0.001337   0.7217  2.91084 ***

####Bias correction (2) using the Ioaniddis et al.(2017) technique
## for income the threshold is  0.17/2.8=0.06071429

0.17/2.8
PEI1=subset(iemn,se<0.06071429)

endogi2<-relevel(PEI1$endoge,ref = "yes")
formi2<-relevel(PEI1$form,ref ="Stone-Geary")
data_typei2<-relevel(PEI1$data_type,ref = "Panel")
data_collectioni2<-relevel(PEI1$data_collection,ref = "annual")
climatei2<-relevel(PEI1$rain_tem,ref = "n")
a_s_li2<-relevel(PEI1$a_s_l,ref = "A")
economyi2<-relevel(PEI1$economy,ref = "lower middle")
scale_studyi2<-relevel(PEI1$scale_study,ref ="aggregated")
tot_in_outi2<-relevel(PEI1$tot_in_out,ref = "T")
incomei2<-relevel(PEI1$income,ref="income")
publication2<-relevel(PEI1$publication_type,ref = "published")



RVEI4 <- robu(formula=est~formi2+data_collectioni2+climatei2+
                publication2,data =PEI1,
              studynum = study_id,
              var.eff.size =var,modelweights = "CORR",rho = .8, small =FALSE)
print(RVEI4) 

## Making constraints 

print(RVEI4)

Fi2 <- rbind(c(0,-1,1,0,0,0,0,0,0,0,0,0)   #beta2 =beta3
             +c(0,0,-1,1,0,0,0,0,0,0,0,0))  #beta3 =beta4

Wald_test(RVEI4, constraints = Fi2, vcov="CR2")
#conclude: do not reject the null that the coefficents are equal
#Test    F    d.f.   p.val
#HTZ   1.79   1.04  0.403   

#  create new SG variable for the Stone-Geary functional forms
PEI1$SG <- ifelse(PEI1$form == "Stone-Geary", 0, 1)
#  Check it has been added
summary(PEI1$SG)

RVEI4.test1 <- robu(formula=est~SG+data_collectioni2+climatei2+
                      publication2,
                    data=PEI1,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)
print(RVEI4.test1)

##not significant for SG forms and everything else as a group

## data frequency, testing all non-annual are equal
print(RVEI4)
dfi2 <- rbind(c(0,0,0,0,0,-1,1,0,0,0,0,0)   #beta6 =beta7
              +c(0,0,0,0,0,0,-1,1,0,0,0,0))  #beta7 =beta8

Wald_test(RVEI4, constraints = dfi2, vcov="CR2")

#conclude: do not reject the null that the coefficents are equal
#Test     F   d.f.  p.val
#HTZ   0.24   16.2  0.63

#  create new annual variable for the annual data frequency
PEI1$AD <- ifelse(PEI1$data_collection == "annual", 0, 1)
#  Check it has been added
summary(PEI1$AD)

RVEI4.test2 <- robu(formula=est~formi2+AD+climatei2+
                      publication2,
                    data=PEI1,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)

print(RVEI4.test2)

##not statistically significant for annual data and everything else as a group

## climate variable, testing all non-tr-climate data variables are equal
print(RVEI4)
cvi2 <- rbind(c(0,0,0,0,0,0,0,0,-1,1,0,0)   #beta9 =beta10
              +c(0,0,0,0,0,0,0,0,0,-1,1,0))  #beta10 =beta11

Wald_test(RVEI4, constraints = cvi2, vcov="CR2")
##conclude: do not reject the null that the coefficents are equal
#Test     F    d.f.  p.val
#HTZ    0.719  7.76  0.422

PEI1$CD <- ifelse(PEI1$rain_tem == "n", 1, 0)
#  Check it has been added
summary(PEI1$CD)

RVEI4.test3 <- robu(formula=est~formi2+data_collectioni2+CD+
                      publication2,
                    data=PEI1,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)

print(RVEI4.test3)

# checking the country income group
PEI1$LM <- ifelse(PEI1$economy == "lower middle", 0, 1)# for lower-middle-income countries
summary(PEI1$LM)
PEI1$UM<-ifelse(PEI1$economy=="upper middle", 0,1) # for upper-middle-income countries
summary(PEI1$UM)
PEI1$HI<-ifelse(PEI1$economy=="high", 0,1) # for high-income countries 
summary(PEI1$HI)

##Pooling the shrinking model
RVEIPs <- robu(formula=est~SG+AD+CD+
                 publication2,
               data=PEI1,studynum = study_id,var.eff.size =var,modelweights = 
                 "CORR",rho = .8, small =FALSE)
print(RVEIPs)




# getting the summary statistics
#for the more precise model
str(PEI1)
summary(PEI1)
mean(PEI1$est)
median(PEI1$est)
sd(PEI1$est)
var(PEI1$est)

mean(PEI1$se)
median(PEI1$se)
sd(PEI1$se)
var(PEI1$se)

# for the less precise model
str(iemn)
summary(iemn)
mean(iemn$est)
median(iemn$est)
sd(iemn$est)
var(iemn$est)

mean(iemn$se)
median(iemn$se)
sd(iemn$se)
var(iemn$se)




############### END OF INCOME ELASTICITY MODEL ESTIMATION #####################

###############################################################################
###############################################################################
###############################################################################

##HHS Elasticity
mdata <- read.csv('HE_data.csv',header=T,sep=",")
head(mdata)
dim(mdata)

hem=subset(mdata,y_p_d=="D")
head(hem);str(hem)


##HHSE

endogh<-relevel(hem$endoge,ref = "yes")
formh<-relevel(hem$form,ref ="Double log")
data_typeh<-relevel(hem$data_type,ref = "Panel")
a_s_lh<-relevel(hem$a_s_l,ref = "A")
economyh<-relevel(hem$economy,ref = "lower middle")
tot_in_outh<-relevel(hem$tot_in_out,ref = "T")
incomeh<-relevel(hem$income,ref="no")
climateh<-relevel(hem$rain_tem,ref = "n")
data_collectionh<-relevel(hem$data_collection,ref = "annual")
publicationh<-relevel(hem$publication_type,ref = "published")


RVEH1 <- robu(formula=est~climateh+tot_in_outh+scale+incomeh,
              data=hem,studynum = study_id,var.eff.size =var,modelweights = 
                "CORR",rho = .8, small =FALSE)
print(RVEH1)


#                        Estimate StdErr t-value dfs    P(|t|>) 95% CI.L 95% CI.U Sig
#           X.Intercept.   0.565 0.1862    3.04  63 0.00348544   0.1932  0.93730 ***
#             climatehr    0.221 0.0854    2.59  63 0.01184573   0.0507  0.39184  **
#           climatehrt     0.188 0.0779    2.41  63 0.01891176   0.0320  0.34316  **
#             climateht    0.462 0.0929    4.98  63 0.00000524   0.2769  0.64803 ***
#          tot_in_outhI    0.286 0.0659    4.35  63 0.00005113   0.1548  0.41809 ***
#          tot_in_outhO   -0.183 0.0998   -1.84  63 0.07064739  -0.3829  0.01591   *
#            scalemacro   -0.148 0.0821   -1.81  63 0.07523779  -0.3124  0.01554   *
#         incomehincome   -0.401 0.1893   -2.12  63 0.03817667  -0.7790 -0.02254  **
#   incomehproxy_income   -0.388 0.1896   -2.05  63 0.04502520  -0.7667 -0.00887  **

# the PE model
#RVEh1 <- robu(formula=est~endogh+formh+tot_in_outh+economyh+
#               data_typeh+a_s_lh+incomeh,
#              data=hem,studynum = study_id,var.eff.size =var,modelweights = 
#              "CORR",rho = .8, small =FALSE)
#print(RVEh1)

##the IE model
#RVEh2 <- robu(formula=est~formh+data_collectionh+climateh+
#                publicationh,
#              data=hem,studynum = study_id,var.eff.size =var,modelweights = 
#                "CORR",rho = .8, small =FALSE)
#print(RVEh2)


##Testing the climate variable: non-tr data are equal

## Testing with the full model--RVEH1  

print(RVEH1)
CD <- rbind(c(0,-1,1,0,0,0,0,0,0)   #beta2 =beta 3
            +c(0,0,-1,1,0,0,0,0,0))  #beta3 =beta4

Wald_test(RVEH1, constraints = CD, vcov="CR2")

#conclude: found marginal evidence for do not rejecting the null that the coefficents are equal
#Test    F d.f.  p.val
#HTZ  4.65 5.77 0.0761

#  create new CD variable for the no climate data included in the model
hem$CD <- ifelse(hem$rain_tem == "n", 1, 0) ##no climate data is 1 the rest is 0
#  Check it has been added
summary(hem$CD)


RVEH1.test1 <- robu(formula=est~CD+tot_in_outh+scale+incomeh,
                    data=hem,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)
print(RVEH1.test1)

#the model has an effect for no-climate data and everything else as a group

## Testing income data: non-income data are equal

print(RVEH1)
IDh <-rbind(c(0,0,0,0,0,0,0,-1,1))   #beta8 =beta 9
Wald_test(RVEH1, constraints = IDh, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test      F  d.f.  p.val
#HTZ   0.0402 11.5 0.845

#  create new income  data variable when actual income data is used
hem$IDh <- ifelse(hem$income == "no", 1, 0)

summary(as.factor(hem$income))
#  Check it has been added
summary(hem$IDh)



RVEH1.test2 <- robu(formula=est~CD+tot_in_outh+scale+IDh,
                    data=hem,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small =FALSE)
print(RVEH1.test2)
##it has no effect, so don't collapse the group

hem$LMh<-ifelse(hem$economy=="lower middle", 0,1) # for lower-middle-income countries
hem$UMh<-ifelse(hem$economy=="upper middle", 0,1) # for upper-middle-income countries
summary(hem$UMh)
hem$HIh<-ifelse(hem$economy=="high", 0,1) # for high-income countries 
summary(hem$HIh)

## however, no country-income group has an effect on the hhs elasticity 
## checking other climate variable

#hem$CD <- ifelse(hem$rain_tem == "n", 0, 1)
##pooling the shrinking selection
## having lower-middle-income in the model make the intercept sign (p<0.1)

RVEHP<-robu(formula=est~CD+tot_in_outh+scale+IDh,
            data=hem,studynum = study_id,var.eff.size =var,modelweights = 
              "CORR",rho = .8, small =FALSE)
print(RVEHP)


#               Estimate StdErr t-value dfs       P(|t|>) 95% CI.L 95% CI.U Sig
# X.Intercept.      0.385 0.0556    6.93  66 0.00000000217   0.2740   0.4958 ***
#             CD   -0.221 0.0675   -3.27  66 0.00172153630  -0.3556  -0.0859 ***
#   tot_in_outhI    0.259 0.0618    4.19  66 0.00008444138   0.1355   0.3821 ***
#   tot_in_outhO   -0.214 0.0891   -2.41  66 0.01885351125  -0.3922  -0.0366  **
#     scalemacro   -0.130 0.0828   -1.57  66 0.12065801286  -0.2955   0.0351    
#          IDh      0.372 0.1806    2.06  66 0.04329591602   0.0115   0.7328  **
  

## Getting indoor-outdoor-total estimates

tot_in_outho<-relevel(hem$tot_in_out,ref = "O") # for outdoor
tot_in_outhi<-relevel(hem$tot_in_out,ref = "I") # for indoor
tot_in_outht<-relevel(hem$tot_in_out,ref = "T") # for total demand

RVEHP1<-robu(formula=est~CD+tot_in_outht+scale+IDh,
            data=hem,studynum = study_id,var.eff.size =var,modelweights = 
              "CORR",rho = .8, small =FALSE)
print(RVEHP1)

##Publication bias detection using SE

RVEH2<-robu(formula=est~CD+tot_in_outh+scale+IDh+se,
            data=hem,studynum = study_id,var.eff.size =var,modelweights = 
              "CORR",rho = .8, small =FALSE)
print(RVEH2)

##No bias detected 
#        Estimate  StdErr  t-value   dfs     P(|t|>)     95% CI.L   95% CI.U Sig
# se     0.186    0.5039   0.369    65    0.71313659026  -0.82036   1.1925

#################### End of HHS Elasticity Model Estimation ###############
