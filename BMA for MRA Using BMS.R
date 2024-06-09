
#* Heterogeneity - Bayesian model averaging in R

library(brms)
library(puniform)
library(metafor)
library(dmetar)
library(haven)
library(tidyverse)
library(meta)
library(BMS)
library(corrplot)
library(rstan)
mdata <- read.csv('mrwp_mdata.csv',header=T,sep=",")
dim(mdata)
head (mdata);str(mdata)

pem=subset(mdata, y_p_d=="P")
head(pem);str(pem)
names(pem)

iem=subset(mdata,y_p_d=="Y")
head(iem);str(iem)

###BRM

#Bayesian Regression Models using 'Stan'
priors <- c(prior(normal(-1,1), class = Intercept),
            prior(cauchy(-0.5,0.5), class = sd))

p.brm1 <- brm(est|se(se) ~ 1 + (1|study),
              data = pem,
              prior = priors,
              iter = 4000)
summary(p.brm1)
#Population-Level Effects: 
#            Estimate Est.Error l-95% CI  u-95% CI   Rhat  Bulk_ESS  Tail_ESS
#Intercept    -0.29      0.02    -0.32    -0.25     1.18       18      65
?bms

#Bayesian Model Sampling and Averaging using brms
#estimating a standard MC3 chain with 1000 burn-ins and 2000 iterations and uniform model priors


bmap= read.csv("BMA_PE_data_test.csv")
is.data.frame(bmap)
#bma=bmap[,c(1:8,10:14)]
View(bmap)
#bma=bmap[,c(3:9,11,13:15,17:19,21:23,25,28,30:31,35:37,44)]
# the model drops the rest of covariates 
#bma=bmap[,c(3:4,10,11,13,15,16,17,19,20,21,23,24,25,27,28,
#           30,31,33,34,39,45)]

bma=bmap[,c(3:4,11,12,14,15:17,19,21,23,24,25,27,28,
           30,31,33,34,39,45)]
View(bma)
#bma=na.omit(bmap)

elasticity1 = bms(bma, burn=1e6,iter=2e6, g="UIP", mprior="dilut", nmodel=5000, mcmc="bd", user.int=FALSE)
PE2 = bms(bma, burn=20000,iter=40000, g="UIP", mprior="uniform", nmodel=5000, mcmc="bd", user.int=FALSE)
PE3 = bms(bma, burn=20000,iter=40000, g="BRIC", mprior="random", nmodel=5000, mcmc="bd", user.int=FALSE)
PE4 = bms(bma, burn=20000,iter=40000, g="hyper=BRIC", mprior="random", nmodel=5000, mcmc="bd", user.int=FALSE)

elasticity2 = bms(bma, burn=1e6,iter=2e6, g="BRIC", mprior="random", nmodel=5000, mcmc="bd", user.int=FALSE)
coef(elasticity1, order.by.pip = F, exact=T, include.constant=T)
coef(elasticity2, order.by.pip = F, exact=T, include.constant=T)
image(elasticity1, yprop2pip=FALSE, order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, cex.axis = 0.7)
image(elasticity2, yprop2pip=FALSE, order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, cex.axis = 0.7)
summary(elasticity1)
summary(elasticity2)
plot(elasticity1)
plot(elasticity2)

print(elasticity1$topmod[1])
col<- colorRampPalette(c("red", "white", "blue"))
M <- round(cor(bma, use="complete.obs", method="pearson"), 2)
corrplot (M, method="color", col=col(200), type="upper", addCoef.col = "black", number.cex=0.5, tl.pos = c("lt"), tl.col="black", tl.srt=45, sig.level = 0.01, insig = "blank")

## Replicating Table 5 in the paper after collapsing some groups

bmape2= read.csv("BMA_PE_data2.csv")
#bma=bmape2[,c(1:8,10:14)]
View(bmape2)
#bma=bmape2[,c(3:9,11,13:15,17:19,21:23,25,28,30:31,35:37,44)]
# the model drops the rest of covariates 
#bma=bmape2[,c(3:4,10,11,13,15,16,17,19,20,21,23,24,25,27,28,
#           30,31,33,34,39,45)]

bma2=bmape2[,c(4,5,7,8,10:12,14:17)]
View(bma2)
#bma=na.omit(bmape2)

PE1 = bms(bma2, burn=1e6,iter=2e6, g="UIP", mprior="dilut", nmodel=5000, mcmc="bd", user.int=FALSE)
#PE2 = bms(bma2, burn=20000,iter=40000, g="UIP", mprior="uniform", nmodel=5000, mcmc="bd", user.int=FALSE)
#PE3 = bms(bma2, burn=20000,iter=40000, g="BRIC", mprior="random", nmodel=5000, mcmc="bd", user.int=FALSE)
#PE4 = bms(bma2, burn=20000,iter=40000, g="hyper=BRIC", mprior="random", nmodel=5000, mcmc="bd", user.int=FALSE)

PE2 = bms(bma2, burn=1e6,iter=2e6, g="BRIC", mprior="random", nmodel=5000, mcmc="bd", user.int=FALSE)
coef(PE1, order.by.pip = F, exact=T, include.constant=T)
coef(PE2, order.by.pip = F, exact=T, include.constant=T)
image(PE1, yprop2pip=FALSE, order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, cex.axis = 0.7)
image(PE2, yprop2pip=FALSE, order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, cex.axis = 0.7)
summary(PE1)
summary(PE2)
plot(PE1)
plot(PE2)

print(PE1$topmod[1])
col<- colorRampPalette(c("red", "white", "blue"))
M <- round(cor(bma2, use="complete.obs", method="pearson"), 2)
corrplot (M, method="color", col=col(200), type="upper", addCoef.col = "black", number.cex=0.5, tl.pos = c("lt"), tl.col="black", tl.srt=45, sig.level = 0.01, insig = "blank")



