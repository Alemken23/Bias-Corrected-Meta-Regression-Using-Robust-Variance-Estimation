
## Visuals for RVE analysis--graphics of various data

mdata <- read.csv('mrwp_mdata.csv',header=T,sep=",")
head (mdata);str(mdata)
# attach(mdata)
pem=subset(mdata, y_p_d=="P")
head(pem);str(pem)
names(pem)
# attach(pem)
iem=subset(mdata,y_p_d=="Y")
head(iem);str(iem)
hem=subset(mdata,y_p_d=="D")
head(hem);str(hem)
# removing na values from the entire dataset
mdatan.na.omit<-na.omit(mdata)
mdatan.na.omit
# to make the names shorter#
mdatan<-na.omit(mdata)
mdatan
### creating a subset for each elasticity type with na's removed ###
pemn=subset(mdatan,y_p_d=="P")
iemn=subset(mdatan,y_p_d=="Y")
hemn=subset(mdatan,y_p_d=="D")

# 1) perform robu test

## PE ##
mp1 <- robu (formula=est~1, data =pemn,studynum = study_id, 
             var.eff.size =var,modelweights = "CORR", rho = .8,small = TRUE)
print(mp1)

## sensetivity analysis
R<-sensitivity(mp1)

res_3<-robu(formula = yi ~ 1, var.eff.size=vi, studynum = studyid,
            modelweights = "CORR", rho = 0.8, small=TRUE, data=pemn)
#model weights is the user-specified option for selecting either the hierarchical# 
#(HIER) or correlated (CORR) effects model                                       #        
#pe_intercept2 <- robu (formula=est~1, data =pemn,modelweights = "HIER",  
#studynum = study_id, var.eff.size =se, small = TRUE)                     
#print(pe_intercept2)

## IE ##
mi1 <- robu (formula=est~1, data =iemn, studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mi1)
## sensetivity analysis
R<-sensitivity(mi1)

## HHE ##
mh1 <- robu (formula=est~1, data =hemn, studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mh1)
## sensetivity analysis
R<-sensitivity(mh1)

## 2)perform a publication bias test (1) using intercept and SE ##

## PE ## 

mp2 <- robu (formula=est~1+se, data =pemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mp2)



## IE ##
mi2 <- robu (formula=est~1+se, data =iemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mi2)

## HHE ##
mh2 <- robu (formula=est~1+se, data =hemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mh2)

## No evidence of bias ##


## 3) Publication bias corrected (1) ##
## using the formula est= intercept+var
## var=se^2

## PE ## 

mp3 <- robu (formula=est~1+I(se^2), data =pemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mp3)

## IE ##
mi3 <- robu (formula=est~1+I(se^2), data =iemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mi3)

## HHE ##
mh3 <- robu (formula=est~1+I(se^2), data =hemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mh3)

## 4) publication bias corrected (2):
##compute adequate power by comparing the standard error of each estimate to 
#the absolute value of this fixed-effect weighted average (WLS-FE) divided by 
#2.8.If the standard error is less than this threshold, we can conclude that 
#the estimate is adequately powered to detect an effect size suggested by 
#the weighted average of all estimates in this area of research
# so for PE 0.348 /2.8= 0.1242857, if SE<  0.1242857 we consider for the analysis
## PE ##
#0.348/2.8
#SEP1=subset(pemn,se<0.1242857)
0.348 /2.8
SEP1=subset(pemn,se< 0.1242857)
View(SEP1)
mp4 <- robu (formula=est~1, data =SEP1,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mp4)


## IE ##
## for income the threshold is  0.171/2.8=0.06107143
0.171/2.8
SEI1=subset(iemn,se<0.06107143)
View(SEI1)
mi4 <- robu (formula=est~1, data =SEI1,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mi4)

## HHE ##
## for HHSE the threshold is  0.277/2.8=0.09964286
0.279/2.8
SEH1=subset(hemn,se<0.09964286)
View(SEH1)
hi4 <- robu (formula=est~1  , data =SEH1,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(hi4)


###########
### Funnel Plot for Price and Income Elasticities  ###
#  I have plotted study averages here.
#  The formula for the rma is not dealing with the
#  the repeats

#  There is more than one way to do the funnel plots
#  Here i have cleaned out the background.  
#  A clean plot is a good plot.
#  More generally, avoid defaults.  You want the plots to
#  look different.
#  The presentation aspect i have gone for is 
#  setting the x and y scale to be the same for each plot
#  that makes some aspects of the visual comparisons easier
#  I have also added a vertical line at zero.  This is an
#  interesting refernce point to use as part of discussions
#  You will need to check that this is the look you want. 

#  I also use 1,1,1 as pch. If yo want to vary pch maybe: 1, 0, 5
#  as a set of open symbols, is an option.  I dont mind.

library(metafor)
dev.off()
par(mfrow = c(2, 2))
mean.p.data <- read.csv("PE_Forestplot.csv")
head(mean.p.data)

mra.p <-rma(est_ave, sei = se_ave, data = mean.p.data)

funnel(mra.p, ylim =c(0,1.8), 
       xlim =c(-4,4), 
       steps =4,digits=1, 
       at = seq(from=-4,to=4,by=2),
       las=1, 
       pch=19,
       cex=.8,
       col = "black",back="white",
       xlab = "Price Elasticity")

abline(v=0, lty=2)
legend('topright',legend= c('Sample mean','Zero line'),lty = c(1,2),bty="n",cex=1)

mean.y.data <- read.csv("IE_Forestplot.csv")
head(mean.y.data)

mra.y <-rma(est_ave,se_ave, data = mean.y.data)

funnel(mra.y, ylim =c(0,1.8), 
       xlim =c(-4,4), 
       steps =4,digits=1, 
       at = seq(from=-4,to=4,by=2),
       las=1, 
       pch=16,
       cex=.8,
       col = "black",back="white",
       xlab = "Income Elasticity")

abline(v=0, lty=2)
legend('topleft',legend= c('Sample mean','Zero line'),lty = c(1,2),bty="n",cex=1)
#legend('topright',legend= c('Studies'),pch=c(18),bty="n", cex=1,inset=0.08)

#legend("topright", legend =c("sample mean"), lty = c(1), bty="n" ,cex=.8)
#legend("topleft", legend =c("zero line" ), lty = c(2), bty="n", cex=.8 )


mean.d.data <- read.csv("mean_hhe.csv")
head(mean.d.data)

mra.d <-rma(ave_est,ave_se, data = mean.d.data)

funnel(mra.d, ylim =c(0,1.8), 
       xlim =c(-4,4), 
       steps =4,digits=1, 
       at = seq(from=-4,to=4,by=2),
       las=1, 
       pch=15,
       cex=.8,
       col = "black",back="white",
       xlab = "People Elasticity")

abline(v=0, lty=2)
legend('topleft',legend= c('Sample mean','Zero line'),lty = c(1,2),bty="n",cex=1)


# Compare model estimate

#  use the averages
summary(mra.p)
# 
# estimate      se      zval    pval    ci.lb    ci.ub     
# -0.2977  0.0240  -12.4210  <.0001  -0.3447  -0.2508  ***

#  for reference full meta estimate
# Estimate StdErr t-value dfs P(|t|>) 95% CI.L 95% CI.U Sig
# 1 X.Intercept.   -0.348 0.0254   -13.7 161       0   -0.398   -0.298 ***

# Broadly consistent in terms of SE with averages and detail
#  this makes sense as weighting is at the study level

#  Your existing implementation thinks each observation
#  is an independent data point
mra.p1 <-rma(est, se, data = pemn)
summary(mra.p1)

#  Note the SE value.  It has approximately halved the level
#  of uncertainty.  It is treating all observations as
#  unique pieces of information

# Model Results:
#   estimate      se      zval    pval    ci.lb    ci.ub     
# -0.2836  0.0116  -24.5510  <.0001  -0.3063  -0.2610  ***


summary(mra.y)
summary(mra.d)

###############################################################################
##############################################################################

png(file = "FunnelPlotpriceandincome.png", width = 350, height = 160, res = 300, units = "mm")

par(mfrow=c(1,2))
resp1<-rma(est_ave,se_ave, data = mean.p.data)
funnel(resp1, yaxis ="sei", steps = 5,digits = 1, back="white", shade="white",
       hlines="white", pch=20, pch.fill=20,col = "dodgerblue4", 
       xlab = "Price Elasticity", main="a) Price Elasticity", ylim = c(0.01,1.8),
       xlim = c(-4,4), las=1,cex.axis=1.1, cex.lab=1.1, cex.main=1.2)


abline(v=0, lty=1)
legend('topright',legend= c('Sample mean','Zero line'),lty = c(2,1),bty="n",cex=1.1)
legend('topleft',legend= c('Studies'),pch=c(20),col = c("dodgerblue4"),bty="n", cex=1.1,inset=.00)


##Income
resi2<-rma( est_ave, se_ave, data = mean.y.data)
funnel(resi2, yaxis ="sei",steps = 5,digits=1, back="white", shade="white",
       hlines="white",pch=20, pch.fill=20,col = "blue4",main= "b) Income Elasticity",
       xlab = "Income Elasticity",ylim = c(0,1.8), xlim = c(-4,4),
       las=1,cex.axis=1.1, cex.lab=1.1, cex.main=1.2)
abline(v=0, lty=1)
legend('topleft',legend= c('Studies'),pch=c(20),col = c("blue4"),bty="n", cex=1.1,inset=.00)
legend('topright',legend= c('Sample mean','Zero line'),lty = c(2,1),bty="n",cex=1.1)

dev.off()
##############################################################################
##############################################################################


#Histogram plots

##Price

PE_data= read.csv("price_updated_data.csv")


## Price, distribution plot of total estimates
hist(PE_data$est,main = "a) Price Elasticity", xlab = "Elasticity Estimates",
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,900), xlim = c(-3,3),breaks = 7,
     cex.main=1.4, border = "black", col = "grey", las=1)

##triming the data

library(glue)
library(lava)

summary(PE_data$est)

dev.off()
par(mfrow=c(1,2))
## Price, distribution plot of total estimates

## This is fine but probably a bit distorted by the outliers. we can trim those...  you can also pool them but here I think it OK to trim..


hist(PE_data$est,main = "a) Price Elasticity", xlab = "Elasticity Estimates",
     
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,320), xlim = c(-4,4),breaks = seq(-4.6,4,.2),
     
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)

abline(v=mean(PE_data$est), lty=1,lwd=1.9) # can change to be whatever

abline(v=median(PE_data$est), lty=2,lwd=1.9)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )


#  exclude the bottom and top 1%
dev.off()

png(file = "HistPlotpriceandincome.png", width = 350, height = 200, res = 300, units = "mm")


par(mfrow=c(1,2))
quantile(PE_data$est, c(.01,.99))

#?trim

trim.datap <- subset(PE_data$est, PE_data$est > quantile(PE_data$est, c(.01)))   

trim.datap <- subset(PE_data$est, PE_data$est< quantile(PE_data$est, c(.99)))   



# use to inform setting the range: use sensible values based on these

max(trim.datap)

min(trim.datap)



hist(trim.datap,main = "a) Price Elasticity", xlab = "Elasticity Estimates",
     
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,200), xlim = c(-2,1),breaks = seq(-5.,1,.095),
     
     cex.main=1.5, border = "black", col = "grey", las=1,freq=T)



abline(v=mean(trim.datap), lty=1,lwd=1) # can change to be whatever

abline(v=median(trim.datap), lty=2,lwd=1)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2),cex = 1.2, bty="n" )


##################


## Income
# hist_IE=read.csv("hist_IE_data.csv")
# 
# hist.i<-hist(hist_IE$publication_year,ylim=c(1,40),main = "b) Income Elasticity",
#              xlab = "Publication Year",ylab = "Number of Studies", cex.axis=1.2,
#              cex.lab=1.2, cex.main=1.5,
#              border = "black", col="grey",las=1)
# text(hist.i$mids,hist.i$counts,labels=hist.i$counts,adj=c(0.5, -0.5),cex=1.2)

##Income, distribution plot of total estimates

IE_data=read.csv("income_updated_data.csv")

# hist(IE_data$est,main = "b) Income Elasticity", xlab = "Elasticity Estimates",
#      cex.axis=1.2, cex.lab=1.2, xlim = c(-1,4),ylim = c(0,450), breaks = 7,
#      cex.main=1.4, border = "black", col = "grey", las=1,freq=T)
# 
# 
# ## Triming
# 
# ## Income, distribution plot of total estimates
# 
# ## This is fine but prolly a bit distorted by the outliers. we can trim those...  you can also pool them but here I think it OK to trim..
# 
# 
# 
# hist(IE_data$est,main = "a) Income Elasticity", xlab = "Elasticity Estimates",
#      
#      cex.axis=1.2, cex.lab=1.2, ylim = c(1,250), xlim = c(-1,4),breaks = seq(-4.6,4,.2),
#      
#      cex.main=1.4, border = "black", col = "grey", las=1,freq=T)
# abline(v=mean(IE_data$est), lty=1,lwd=1.9) # can change to be whatever
# 
# abline(v=median(IE_data$est), lty=2,lwd=1.9)  # can change to be whatever
# 
# legend("center", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )


#  exclude the bottom and top 1%
#dev.off()
#par(mfrow=c(1,2))
quantile(IE_data$est, c(.01,.99))



trim.datai <- subset(IE_data$est, IE_data$est > quantile(IE_data$est, c(.01))  )   

trim.datai <- subset(IE_data$est, IE_data$est < quantile(IE_data$est, c(.99))  )   



# use to inform setting the range: use sensible values based on these

max(trim.datai)

min(trim.datai)



hist(trim.datai,main = "b) Income Elasticity", xlab = "Elasticity Estimates",
     
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,200), xlim = c(-0.5,2),breaks = seq(-1.,2,.095),
     
     cex.main=1.5, border = "black", col = "grey", las=1,freq=T)



abline(v=mean(trim.datai), lty=1,lwd=1.9) # can change to be whatever

abline(v=median(trim.datai), lty=2,lwd=1.9)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2),cex = 1.2,  bty="n")

dev.off()
###############################################################################
###############################################################################
###############################################################################

### plotting the number of studies over time ###
# price elasticity 
dev.off()

png(file = "BarPlotpriceandincome.png", width = 360, height = 200, res = 300, units = "mm")

par(mfrow=c(1,2))
PE.c = read.csv("cum_PE_updated_data.csv")
names(PE.c)
head(PE.c)
#barplot(PE.c$cum,names.arg=PE.c$year,xlab="Publication Year",
#        ylab="Number of Studies",col="grey",
#        main="Growth in Studies",border="blue",ylim = c(0,200))

barplot(PE.c$cum,names.arg = PE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,200),
        main = "a) Price Elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=0.9,cex.lab=1.0, cex.main=1.2, cex.names = 0.8,
        las=1, space = 2)

## Income elasticity

IE.c = read.csv("cum_IE_updated_data.csv")
names(IE.c)
head(IE.c)
#barplot(IE.c$cum,names.arg=IE.c$year,xlab="Publication Year",
#       ylab="Number of Studies",col="grey",
#      main="",border="blue",ylim = c(0,180))

#barplot(cum~year, data = IE.c)

barplot(IE.c$cum,names.arg = IE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,200),
        main = "b) Income Elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=0.9,cex.lab=1.0, cex.main=1.2,cex.names = 0.8,
        las=1, space = 2)
dev.off()


