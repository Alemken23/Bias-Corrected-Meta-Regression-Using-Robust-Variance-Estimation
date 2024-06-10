

## Weighted mean values for all elasticities--OLS based estimation

edata <- read.csv('mrwp_mdata.csv',header=T,sep=",")

##subsetting price, income and people elasts
pe=subset(edata, y_p_d=="P")
#write.csv(pe,file = "PE_data.csv")
head(pe);str(pe)
names(pe)
ie=subset(edata,y_p_d=="Y")
#write.csv(ie,file = "IE_data.csv")
head(ie);str(ie)
he=subset(edata,y_p_d=="D")
head(he);str(he)
#write.csv(he,file = "HHSE_data.csv")
## OLS ##
## the unweighted average for price elasticity estimates
#list()
#remove(list = ls())


PE_data= read.csv("PE_data.csv")
olsp1<-lm(est~1, data =PE_data)
summary(olsp1)

#            Estimate   Std. Error  t value  Pr(>|t|)    
#(Intercept) -0.36474    0.01448   -25.2     <2e-16 ***

##including the SE
olsp2<-lm(est~1+se, data =PE_data)
summary(olsp2)

#              Estimate  Std. Error  t value  Pr(>|t|)    
#(Intercept)  -0.24397    0.01614   -15.12    <2e-16 ***
# se          -0.93209    0.06985   -13.34    <2e-16 ***

## Now the weighted OLS (WLS)
## inverse elasticity estimate as a weighting

###  Price Elasticity ###

wlsp1<-lm(est~1,weights=invers_noestmate,PE_data)
summary(wlsp1)

##Regions
summary(as.factor(PE_data$region))

#Asia
Asia.p=subset(PE_data,region=="ASIA")

wls.Asia<-lm(est~1,weights=invers_noestmate,data=Asia.p)
summary(wls.Asia)

length(unique(Asia.p[,3]))
#Aus and NZ

Aunz.p=subset(PE_data,region=="AUNZ")

wls.Aunz<-lm(est~1,weights=invers_noestmate,data=Aunz.p)
summary(wls.Aunz)

length(unique(Aunz.p[,3]))

# Central America

CA.p=subset(PE_data,region=="CA")

wls.CA<-lm(est~1,weights=invers_noestmate,data=CA.p)
summary(wls.CA)
length(unique(CA.p[,3]))


#EU

EU.p=subset(PE_data,region=="EU")

wls.EU<-lm(est~1,weights=invers_noestmate,data=EU.p)
summary(wls.EU)
length(unique(EU.p[,3]))


#Lating America

LA.p=subset(PE_data,region=="LA")

wls.LA<-lm(est~1,weights=invers_noestmate,data=LA.p)
summary(wls.LA)
length(unique(LA.p[,3]))

# MENA

MENA.p=subset(PE_data,region=="MENA")

wls.MENA<-lm(est~1,weights=invers_noestmate,data=MENA.p)
summary(wls.MENA)

length(unique(MENA.p[,3]))

# N America

NA.p=subset(PE_data,region=="NAMERICA")

wls.NA<-lm(est~1,weights=invers_noestmate,data=NA.p)
summary(wls.NA)
length(unique(NA.p[,3]))

#Non-EU
NEU.p=subset(PE_data,region=="NONEU")

wls.NEU<-lm(est~1,weights=invers_noestmate,data=NEU.p)
summary(wls.NEU)
length(unique(NEU.p[,3]))

#SSA

SSA.p=subset(PE_data,region=="SSA")

wls.SSA<-lm(est~1,weights=invers_noestmate,data=SSA.p)
summary(wls.SSA)
length(unique(SSA.p[,3]))

## Elasticity type

# Average elasticity 

Avelast.p=subset(PE_data,a_s_l=="A")

wls.avelast<-lm(est~1,weights=invers_noestmate,data=Avelast.p)
summary(wls.avelast)

length(unique(Avelast.p[,3]))

# Long-run elasticity

lrelast.p=subset(PE_data,a_s_l=="L")

wls.lrelast<-lm(est~1,weights=invers_noestmate,data=lrelast.p)
summary(wls.lrelast)

length(unique(lrelast.p[,3]))

# Short-run elasticity

srelast.p=subset(PE_data,a_s_l=="S")

wls.srelast<-lm(est~1,weights=invers_noestmate,data=srelast.p)
summary(wls.srelast)
length(unique(srelast.p[,3]))


# Indoor and Outdoor

# Indoor
idelast.p=subset(PE_data,tot_in_out=="I")

wls.idelast<-lm(est~1,weights=invers_noestmate,data=idelast.p)
summary(wls.idelast)
length(unique(idelast.p[,3]))

#Outdoor

odelast.p=subset(PE_data,tot_in_out=="O")

wls.odelast<-lm(est~1,weights=invers_noestmate,data=odelast.p)
summary(wls.odelast)
length(unique(odelast.p[,3]))

# Total demand
tdelast.p=subset(PE_data,tot_in_out=="T")

wls.tdelast<-lm(est~1,weights=invers_noestmate,data=tdelast.p)
summary(wls.tdelast)
length(unique(tdelast.p[,3]))

## Endogeneity 
# yes controlled

yeselast.p=subset(PE_data,endoge=="yes")

wls.yeselast<-lm(est~1,weights=invers_noestmate,data=yeselast.p)
summary(wls.yeselast)

length(unique(yeselast.p[,3]))

# not controlled

noelast.p=subset(PE_data,endoge=="no")

wls.noelast<-lm(est~1,weights=invers_noestmate,data=noelast.p)
summary(wls.noelast)

length(unique(noelast.p[,3]))

## data aggregation
summary(as.factor(PE_data$scale_study))
# Aggregated data
aggr.p=subset(PE_data,scale_study=="aggregated")

wls.aggr<-lm(est~1,weights=invers_noestmate,data=aggr.p)
summary(wls.aggr)
length(unique(aggr.p[,3]))

# household data
disaggr.p=subset(PE_data,level_study=="household level")

wls.disaggr<-lm(est~1,weights=invers_noestmate,data=disaggr.p)
summary(wls.disaggr)
length(unique(disaggr.p[,3]))

## data series
summary(as.factor(PE_data$data_type))
# Panel data
pan.p=subset(PE_data,data_type=="Panel")

wls.pan<-lm(est~1,weights=invers_noestmate,data=pan.p)
summary(wls.pan)
length(unique(pan.p[,3]))

# Cross-sectional data
cros.p=subset(PE_data,data_type=="Cross")

wls.cros<-lm(est~1,weights=invers_noestmate,data=cros.p)
summary(wls.cros)
length(unique(cros.p[,3]))

# Time-series data
time.p=subset(PE_data,data_type=="Time")

wls.time<-lm(est~1,weights=invers_noestmate,data=time.p)
summary(wls.time)
length(unique(time.p[,3]))


############################

## Income Elasticity ##
## OLS ##
## the unweighted average for Income elasticity estimates
IE_data=read.csv("IE_data.csv")
head(IE_data)
dim(IE_data)

length(unique(IE_data[,3]))


olsi1<-lm(est~1, data =IE_data)
summary(olsi1)

#            Estimate   Std. Error  t value  Pr(>|t|)    
#(Intercept)  0.27307    0.01667   16.39   <2e-16 ***

##including the SE
olsi2<-lm(est~1+se, data =IE_data)
summary(olsi2)

#              Estimate  Std. Error  t value  Pr(>|t|)    
#(Intercept)  0.08995    0.01590     5.658    2.55e-08 ***
# se          1.38557    0.07211     19.215   < 2e-16 ***

## Now the weighted OLS (WLS)
## inverse elasticity estimate as a weighting


wlsi1<-lm(est~1,weights=invers_noestmate,data=IE_data)
summary(wlsi1)

##Regions
summary(as.factor(IE_data$region))

#Asia
Asia.i=subset(IE_data,region=="ASIA")

wlsi.Asia<-lm(est~1,weights=invers_noestmate,data=Asia.i)
summary(wlsi.Asia)

length(unique(Asia.i[,3]))
#Aus and NZ

Aunz.i=subset(IE_data,region=="AUNZ")

wlsi.Aunz<-lm(est~1,weights=invers_noestmate,data=Aunz.i)
summary(wlsi.Aunz)

length(unique(Aunz.i[,3]))

# Central America

CA.i=subset(IE_data,region=="CA")

wlsi.CA<-lm(est~1,weights=invers_noestmate,data=CA.i)
summary(wlsi.CA)

length(unique(CA.i[,3]))

#EU
EU.i=subset(IE_data,region=="EU")

wlsi.EU<-lm(est~1,weights=invers_noestmate,data=EU.i)
summary(wlsi.EU)
length(unique(EU.i[,3]))


#Lating America

LA.i=subset(IE_data,region=="LA")

wlsi.LA<-lm(est~1,weights=invers_noestmate,data=LA.i)
summary(wlsi.LA)
length(unique(LA.i[,3]))

# MENA

MENA.i=subset(IE_data,region=="MENA")

wlsi.MENA<-lm(est~1,weights=invers_noestmate,data=MENA.i)
summary(wlsi.MENA)

length(unique(MENA.i[,3]))

# N America

NA.i=subset(IE_data,region=="NAMERICA")

wlsi.NA<-lm(est~1,weights=invers_noestmate,data=NA.i)
summary(wlsi.NA)
length(unique(NA.i[,3]))

#Non-EU
NEU.i=subset(IE_data,region=="NONEU")

wlsi.NEU<-lm(est~1,weights=invers_noestmate,data=NEU.i)
summary(wlsi.NEU)

#SSA

SSA.i=subset(IE_data,region=="SSA")

wlsi.SSA<-lm(est~1,weights=invers_noestmate,data=SSA.i)
summary(wlsi.SSA)
length(unique(SSA.i[,3]))

## Elasticity type

# Average elasticity 


Avelast.i=subset(IE_data,a_s_l=="A")

wlsi.avelast<-lm(est~1,weights=invers_noestmate,data=Avelast.i)
summary(wlsi.avelast)

length(unique(Avelast.i[,3]))

# Long-run elasticity

lrelast.i=subset(IE_data,a_s_l=="L")

wlsi.lrelast<-lm(est~1,weights=invers_noestmate,data=lrelast.i)
summary(wlsi.lrelast)

length(unique(lrelast.i[,3]))

# Short-run elasticity

srelast.i=subset(IE_data,a_s_l=="S")

wlsi.srelast<-lm(est~1,weights=invers_noestmate,data=srelast.i)
summary(wlsi.srelast)

length(unique(lrelast.i[,3]))
# Indoor and Outdoor

# Indoor
idelast.i=subset(IE_data,tot_in_out=="I")

wlsi.idelast<-lm(est~1,weights=invers_noestmate,data=idelast.i)
summary(wlsi.idelast)

length(unique(idelast.i[,3]))

#Outdoor

odelast.i=subset(IE_data,tot_in_out=="O")

wlsi.odelast<-lm(est~1,weights=invers_noestmate,data=odelast.i)
summary(wlsi.odelast)

length(unique(odelast.i[,3]))

# Total demand
tdelast.i=subset(IE_data,tot_in_out=="T")

wlsi.tdelast<-lm(est~1,weights=invers_noestmate,data=tdelast.i)
summary(wlsi.tdelast)

length(unique(tdelast.i[,3]))

## Endogeneity 
# yes controlled

yeselast.i=subset(IE_data,endoge=="yes")

wlsi.yeselast<-lm(est~1,weights=invers_noestmate,data=yeselast.i)
summary(wlsi.yeselast)
length(unique(yeselast.i[,3]))

# not controlled

noelast.i=subset(IE_data,endoge=="no")

wlsi.noelast<-lm(est~1,weights=invers_noestmate,data=noelast.i)
summary(wlsi.noelast)
length(unique(noelast.i[,3]))

## data aggregation
summary(as.factor(IE_data$scale_study))

# Aggregated data
aggr.i=subset(IE_data,scale_study=="aggregated")

wlsi.aggr<-lm(est~1,weights=invers_noestmate,data=aggr.i)
summary(wlsi.aggr)
length(unique(aggr.i[,3]))

# household data
disaggr.i=subset(IE_data,level_study=="household level")

wlsi.disaggr<-lm(est~1,weights=invers_noestmate,data=disaggr.i)
summary(wlsi.disaggr)

length(unique(disaggr.i[,3]))


## data series
summary(as.factor(IE_data$data_type))
# Panel data
pan.i=subset(IE_data,data_type=="Panel")

wlsi.pan<-lm(est~1,weights=invers_noestmate,data=pan.i)
summary(wlsi.pan)
length(unique(pan.i[,3]))

# Cross-sectional data
cros.i=subset(IE_data,data_type=="Cross")

wlsi.cros<-lm(est~1,weights=invers_noestmate,data=cros.i)
summary(wlsi.cros)
length(unique(cros.i[,3]))

# Time-series data
time.i=subset(IE_data,data_type=="Time")

wlsi.time<-lm(est~1,weights=invers_noestmate,data=time.i)
summary(wlsi.time)
length(unique(time.i[,3]))

###############################################################################
###############################################################################
###############################################################################

## People Elasticity ##

## OLS ##
## the unweighted average for people elasticity estimates
he_data= read.csv("HE_data_updated.csv")
head(he_data)
dim(he_data)
str(he_data)

summary(he_data$est)
mean(he_data$est)
sd(he_data$est)

std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderr(he_data$est)


olsh1<-lm(est~1, data =he_data)
summary(olsh1)

#            Estimate   Std. Error  t value  Pr(>|t|)    
#(Intercept)  0.41397    0.02705    15.3   <2e-16 ***

##including the_data SE
olsh2<-lm(est~1+se, data =he_data)
summary(olsh2)

#              Estimate  Std. Error  t value  Pr(>|t|)    
#(Intercept)  0.28060    0.03199   8.772  < 2e-16 ***
# se          0.86229    0.12797   6.738 9.15e-11 ***

## Now the weighted OLS (WLS)
## inverse elasticity estimate as a weighting

length(unique(he_data[,3]))
wlsh1<-lm(est~1,weights=invers_noestmate,data=he_data)
summary(wlsh1)

##Regions
summary(as.factor(he_data$region))
summary(he_data$region)
tapply(he_data$est,he_data$region,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$region,stderr)
stderrtable



#Asia
Asia.h=subset(he_data,region=="ASIA")

wlsh.Asia<-lm(est~1,weights=invers_noestmate,data=Asia.h)
summary(wlsh.Asia)
length(unique(Asia.h[,3]))
#Aus and NZ

Aunz.h=subset(he_data,region=="AUNZ")

wlsh.Aunz<-lm(est~1,weights=invers_noestmate,data=Aunz.h)
summary(wlsh.Aunz)
length(unique(Aunz.h[,3]))

# Central America

CA.h=subset(he_data,region=="CA")

wlsh.CA<-lm(est~1,weights=invers_noestmate,data=CA.h)
summary(wlsh.CA)
length(unique(CA.h[,3]))

#EU

EU.h=subset(he_data,region=="EU")

wlsh.EU<-lm(est~1,weights=invers_noestmate,data=EU.h)
summary(wlsh.EU)
length(unique(EU.h[,3]))

#Lating America

LA.h=subset(he_data,region=="LA")

wlsh.LA<-lm(est~1,weights=invers_noestmate,data=LA.h)
summary(wlsh.LA)
length(unique(LA.h[,3]))


# MENA

MENA.h=subset(he_data,region=="MENA")

wlsh.MENA<-lm(est~1,weights=invers_noestmate,data=MENA.h)
summary(wlsh.MENA)
length(unique(MENA.h[,3]))

# N America

NA.h=subset(he_data,region=="NAMERICA")

wlsh.NA<-lm(est~1,weights=invers_noestmate,data=NA.h)
summary(wlsh.NA)
length(unique(NA.h[,3]))

#Non-EU
NEU.h=subset(he_data,region=="NONEU")

wlsh.NEU<-lm(est~1,weights=invers_noestmate,data=NEU.h)
summary(wlsh.NEU)
length(unique(NEU.h[,3]))

#SSA

SSA.h=subset(he_data,region=="SSA")

wlsh.SSA<-lm(est~1,weights=invers_noestmate,data=SSA.h)
summary(wlsh.SSA)
length(unique(SSA.h[,3]))

## Elasticity type

# Average elasticity 
summary(he_data$a_s_l)
tapply(he_data$est,he_data$a_s_l,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$a_s_l,stderr)
stderrtable

Avelast.h=subset(he_data,a_s_l=="A")
wlsh.avelast<-lm(est~1,weights=invers_noestmate,data=Avelast.h)
summary(wlsh.avelast)
length(unique(Avelast.h[,3]))

# Long-run elasticity

lrelast.h=subset(he_data,a_s_l=="L")

wlsh.lrelast<-lm(est~1,weights=invers_noestmate,data=lrelast.h)
summary(wlsh.lrelast)
length(unique(lrelast.h[,3]))

# Short-run elasticity

srelast.h=subset(he_data,a_s_l=="S")

wlsh.srelast<-lm(est~1,weights=invers_noestmate,data=srelast.h)
summary(wlsh.srelast)
length(unique(srelast.h[,3]))

# Indoor and Outdoor

# Indoor
summary(he_data$tot_in_out)
tapply(he_data$est,he_data$tot_in_out,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$tot_in_out,stderr)
stderrtable


idelast.h=subset(he_data,tot_in_out=="I")

wlsh.idelast<-lm(est~1,weights=invers_noestmate,data=idelast.h)
summary(wlsh.idelast)
length(unique(idelast.h[,3]))

#Outdoor

odelast.h=subset(he_data,tot_in_out=="O")

wlsh.odelast<-lm(est~1,weights=invers_noestmate,data=odelast.h)
summary(wlsh.odelast)
length(unique(odelast.h[,3]))

# Total demand
tdelast.h=subset(he_data,tot_in_out=="T")

wlsh.tdelast<-lm(est~1,weights=invers_noestmate,data=tdelast.h)
summary(wlsh.tdelast)
length(unique(tdelast.h[,3]))

## Endogeneity 
# yes controlled
head(he_data)
tapply(he_data$est,he_data$endoge,mean)
summary(as.factor(he_data$endoge))  

tapply(he_data$est,he_data$endoge,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
  sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$endoge,stderr)
stderrtable

yeselast.h=subset(he_data,endoge=="yes")

wlsh.yeselast<-lm(est~1,weights=invers_noestmate,data=yeselast.h)
summary(wlsh.yeselast)

length(unique(yeselast.h[,3]))

# not controlled

noelast.h=subset(he_data,endoge=="no")

wlsh.noelast<-lm(est~1,weights=invers_noestmate,data=noelast.h)
summary(wlsh.noelast)
length(unique(noelast.h[,3]))


## data series
summary(as.factor(he_data$data_type))

tapply(he_data$est,he_data$data_type,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
  sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$data_type,stderr)
stderrtable

# Cross-sectional data
cros.h=subset(he_data,data_type=="Cross")

wlsh.cros<-lm(est~1,weights=invers_noestmate,data=cros.h)
summary(wlsh.cros)
length(unique(cros.h[,3]))
# Panel data
pan.h=subset(he_data,data_type=="Panel")

wlsh.pan<-lm(est~1,weights=invers_noestmate,data=pan.h)
summary(wlsh.pan)
length(unique(pan.h[,3]))


# Time-series data
time.h=subset(he_data,data_type=="Time")

wlsh.time<-lm(est~1,weights=invers_noestmate,data=time.h)
summary(wlsh.time)
length(unique(time.h[,3]))


## data aggregation ##

hen<-na.omit(he_data)
summary(as.factor(he_data$scale))
summary(as.factor(he_data$ data_level))


he_data$data_level [he_data$data_level %in% c("Apartment level",
                                                "Household level")]<- "Household level"

he_data$data_level [he_data$data_level %in% c("Census tract level","Suburb level",
                                              "Community level")]<- "Suburb level"

he_data$data_level [he_data$data_level %in% c("Municipality level",
                                              "Utility level")]<- "Municipality level"

he_data$data_level [he_data$data_level %in% c("Regional level","District level",
                                              "Governorate level")]<- "Regional level"

PWDAD21$Land.Use.Description [PWDAD21$Land.Use.Description %in% c("HOUSE", "HOUSES",
                                                                  "HOUSE, COTT",
                                                                  "PART HOUSE",
                                                                  "HOUSE, FLAT")]<- "House"
summary(as.factor(he_data$ data_level))
tapply(he_data$est,he_data$data_level,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$data_level,stderr)
stderrtable

#  Suburb level 
sub.h=subset(he_data,data_level=="Suburb level")

wlsh.sub<-lm(est~1,weights=invers_noestmate,data=sub.h)
summary(wlsh.sub)
length(unique(sub.h[,3]))


# Household level
hhld.h=subset(he_data,data_level=="Household level")

wlsh.hhld<-lm(est~1,weights=invers_noestmate,data=hhld.h)
summary(wlsh.hhld)
length(unique(hhld.h[,3]))

# Municipality level
mun.h=subset(he_data,data_level=="Municipality level")

wlsh.mun<-lm(est~1,weights=invers_noestmate,data=mun.h)
summary(wlsh.mun)
length(unique(mun.h[,3]))

# Regional  level
reg.h=subset(he_data,data_level=="Regional level")

wlsh.reg<-lm(est~1,weights=invers_noestmate,data=reg.h)
summary(wlsh.reg)
length(unique(reg.h[,3]))



# Aggregated data
aggr.h=subset(he_data,scale=="macro")

wlsh.aggr<-lm(est~1,weights=invers_noestmate,data=aggr.h)
summary(wlsh.aggr)
length(unique(aggr.h[,3]))
# household data
disaggr.h=subset(he_data,studyconductedu=="Household level")

wlsh.disaggr<-lm(est~1,weights=invers_noestmate,data=disaggr.h)
summary(wlsh.disaggr)
length(unique(disaggr.h[,3]))

### Tariff structure ###
#Increasing
head(he_data)
summary(he_data$tariffadj)
tapply(he_data$est,he_data$tariffadj,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$tariffadj,stderr)
stderrtable


incr.h=subset(he_data,tariffadj=="increasing")

wlsh.incr<-lm(est~1,weights=invers_noestmate,data=incr.h)
summary(wlsh.incr)
length(unique(incr.h[,3]))

#Decreasing 

decr.h=subset(he_data,tariffadj=="decreasing")
wlsh.decr<-lm(est~1,weights=invers_noestmate,data=decr.h)
summary(wlsh.decr)
length(unique(decr.h[,3]))

#Uniform
uni.h=subset(he_data,tariffadj=="uniform")
wlsh.uni<-lm(est~1,weights=invers_noestmate,data=uni.h)
summary(wlsh.uni)
length(unique(uni.h[,3]))

#Other tariff structures

other.h=subset(he_data,tariffadj=="others")
wlsh.othi<-lm(est~1,weights=invers_noestmate,data=other.h)
summary(wlsh.othi)
length(unique(other.h[,3]))

##Climate data
head(he_data)

tapply(he_data$est,he_data$rain_tem,mean)
summary(as.factor(he_data$rain_tem))  

tapply(he_data$est,he_data$rain_tem,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
  sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$rain_tem,stderr)
stderrtable

#No climate
noclimate.h=subset(he_data,rain_tem=="n")

wlsh.noclimate<-lm(est~1,weights=invers_noestmate,data=noclimate.h)
summary(wlsh.noclimate)

length(unique(noclimate.h[,3]))

#Only Rainfall 
rain.h=subset(he_data,rain_tem=="r")

wlsh.rain<-lm(est~1,weights=invers_noestmate,data=rain.h)
summary(wlsh.rain)

length(unique(rain.h[,3]))

#Only Temperature 
temp.h=subset(he_data,rain_tem=="t")

wlsh.temp<-lm(est~1,weights=invers_noestmate,data=temp.h)
summary(wlsh.temp)

length(unique(temp.h[,3]))

#
#Both Rain and Temperature 
Both.h=subset(he_data,rain_tem=="rt")

wlsh.both<-lm(est~1,weights=invers_noestmate,data=Both.h)
summary(wlsh.both)

length(unique(Both.h[,3]))

###################

##Income data
head(he_data)

tapply(he_data$est,he_data$income,mean)
summary(as.factor(he_data$income))  

tapply(he_data$est,he_data$income,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
  sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(1,2,3,4))
stderrtable <- tapply(he_data$est,he_data$income,stderr)
stderrtable

##Income directly used

income.h=subset(he_data,income=="income")

wlsh.income<-lm(est~1,weights=invers_noestmate,data=income.h)
summary(wlsh.income)

length(unique(income.h[,3]))

##Income proxy used

proxy.h=subset(he_data,income=="proxy_income")

wlsh.proxy<-lm(est~1,weights=invers_noestmate,data=proxy.h)
summary(wlsh.proxy)

length(unique(proxy.h[,3]))

##Income data not used at all

noincome.h=subset(he_data,income=="no")

wlsh.noincome<-lm(est~1,weights=invers_noestmate,data=noincome.h)
summary(wlsh.noincome)

length(unique(noincome.h[,3]))

## Log GDP Per Capita
## inverse elasticity estimate as a weighting

length(unique(he_data[,3]))
wls.income<-lm(log(income_pc)~1,weights=invers_noestmate,data=he_data)
summary(wls.income)


## Log Household Size
HH1=subset(he_data,hhs>0)

length(unique(HH1[,3]))
wls.hhs<-lm(log(hhs)~1,weights=invers_noestmate,data=HH1)
summary(wls.hhs)


#removing na values from the entire dataset
mdatan.na.omit<-na.omit(mdata)
mdatan.na.omit
#to make the names shorter#
mdatan<-na.omit(mdata)
mdatan


###############################################################################

### Unweighted Mean For the Tariff Structure ###

tapply(he_data$est,he_data$tariffadj,mean)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(-1,-2,-3,-4))
stderrtable <- tapply(he_data$est,he_data$tariffadj,stderr)
stderrtable

summary(as.factor(he_data$tariffadj))

### Unweighted Mean For GDP Per Capita ###
mean(he_data$income_pc)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(-1,-2,-3,-4))
stderr(he_data$income_pc)
sd(he_data$income_pc)
summary(as.numeric(he_data$income_pc))

### Unweighted Mean For Household Size ###
hhs.n<- na.omit(he_data$hhs) 
mean(hhs.n)
std <- function(x) sd(x)/sqrt(length(x))
stderr <- function(x) {
        sqrt(var(x)/length(x))
}
std <- function(x) sd(x)/sqrt(length(x))
stderr(c(-1,-2,-3,-4))
stderr(hhs.n)
sd(hhs.n)
is.na(he_data$hhs)
summary(as.numeric(he_data$hhs))

#################### Plotting Histograms ######################################

# Using a better ploting strategy
dev.off()
par(mfrow=c(1,2))
head(PE_data)
##Price
PE_data=read.csv("PE_data.csv")
studies.p=length(unique(PE_data[,3]))
studies.p
year.p=length(unique(PE_data[,4]))
year.p

hist_PE= read.csv("hist_PE_data.csv")
head(hist_PE)
hist.p <- hist(hist_PE$publication_year,ylim=c(0,40),
          main = "a) Price Elasticity",
          xlab = "Publication Year",ylab = "Number of Studies",cex.axis=1.2,
          cex.lab=1.2, cex.main=1.5,
          border = "black", col="grey",las=1)
text(hist.p$mids,hist.p$counts,labels=hist.p$counts, adj=c(0.5, -0.5),cex =1.2 )
hist(hist_PE$study_id, add = TRUE)

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

## This is fine but prolly a bit distorted by the outliers. we can trim those...  you can also pool them but here I think it OK to trim..



hist(PE_data$est,main = "a) Price Elasticity", xlab = "Elasticity Estimates",
     
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,320), xlim = c(-4,4),breaks = seq(-4.6,4,.2),
     
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)

abline(v=mean(PE_data$est), lty=1,lwd=1.9) # can change to be whatever

abline(v=median(PE_data$est), lty=2,lwd=1.9)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )


#  exclude the bottom and top 1%
dev.off()
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

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )

#############

##################


##Income
hist_IE=read.csv("hist_IE_data.csv")

hist.i<-hist(hist_IE$publication_year,ylim=c(1,40),main = "b) Income Elasticity",
        xlab = "Publication Year",ylab = "Number of Studies", cex.axis=1.2,
        cex.lab=1.2, cex.main=1.5,
        border = "black", col="grey",las=1)
text(hist.i$mids,hist.i$counts,labels=hist.i$counts,adj=c(0.5, -0.5),cex=1.2)

##Income, distribution plot of total estimates
IE_data=read.csv("IE_data.csv")
hist(IE_data$est,main = "b) Income Elasticity", xlab = "Elasticity Estimates",
     cex.axis=1.2, cex.lab=1.2, xlim = c(-1,4),ylim = c(0,450), breaks = 7,
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)


## Triming

## Income, distribution plot of total estimates

## This is fine but prolly a bit distorted by the outliers. we can trim those...  you can also pool them but here I think it OK to trim..



hist(IE_data$est,main = "a) Income Elasticity", xlab = "Elasticity Estimates",
     
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,250), xlim = c(-1,4),breaks = seq(-4.6,4,.2),
     
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)
abline(v=mean(IE_data$est), lty=1,lwd=1.9) # can change to be whatever

abline(v=median(IE_data$est), lty=2,lwd=1.9)  # can change to be whatever

legend("center", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )


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

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )


##HHS 
h<-hist(hem$publication_year,ylim=c(1,110),main = "People Elasticity",
        xlab = "Publication Year",ylab = "Number of Estimate",cex.axis=1.4,
        cex.lab=1.5,cex.main=1.8,
        border = "black", col="grey",las=1)
text(h$mids,h$counts,labels=h$counts,adj=c(0.5, -0.5),cex=1.5)

##People, distribution plot of total estimates
HE_data=read.csv("HE_data.csv")
hist(HE_data$est,main = "b) People Elasticity", xlab = "Elasticity Estimates",
     cex.axis=1.2, cex.lab=1.2, xlim = c(-1,4),ylim = c(0,450), breaks = 7,
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)


## Triming

## People, distribution plot of total estimates

## This is fine but prolly a bit distorted by the outlHErs. we can trim those...  you can also pool them but here I think it OK to trim..

dev.off()
par(mfrow=c(1,2))

hist(HE_data$est,main = "a) People elasticity values", xlab = "Elasticity Estimates",
     
     cex.axis=1.2, cex.lab=1.2, ylim = c(1,80), xlim = c(-1,2),breaks = seq(-3.6,5,.2),
     
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)
abline(v=mean(HE_data$est), lty=1,lwd=1.9) # can change to be whatever

abline(v=median(HE_data$est), lty=2,lwd=1.9)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )


#  exclude the bottom and top 1%
#dev.off()
#par(mfrow=c(1,2))
quantile(HE_data$est, c(.01,.99))



trim.datah <- subset(HE_data$est, HE_data$est > quantile(HE_data$est, c(.01))  )   

trim.datah <- subset(HE_data$est, HE_data$est < quantile(HE_data$est, c(.99))  )   



# use to inform setting the range: use sensible values based on these

max(trim.datah)

min(trim.datah)



hist(trim.datah,main = "a) People elasticity values", xlab = "Elasticity Estimates",
     
     cex.axis=1.3, cex.lab=1.4, ylim = c(1,35), xlim = c(-.5,2),breaks = seq(-3.,2,.095),
     
     cex.main=1.5, border = "black", col = "grey", las=1,freq=T)



abline(v=mean(trim.datah), lty=1,lwd=1.9) # can change to be whatever

abline(v=median(trim.datah), lty=2,lwd=1.9)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )

########################################

## Plotting using number of study 
ehist <- read.csv('hist.csv',header=T,sep=",")
dev.off()
head(ehist)
par(mfrow=c(1,2)) 
##Price
s <- hist(ehist$publication_year,ylim=c(1,40),main = "",
          xlab = "Publication Year",ylab = "Number of Studies ",
          border = "royalblue4", col="salmon4")
text(s$mids,s$counts,labels=s$counts, adj=c(0.5, -0.5))

###############################################################################
###############################################################################
### plotting the number of studies over time ###
# price elasticiy 
dev.off()
par(mfrow=c(1,2))
PE.c = read.csv("cum_PE.csv")
names(PE.c)
#barplot(PE.c$cum,names.arg=PE.c$year,xlab="Publication Year",
#        ylab="Number of Studies",col="grey",
#        main="Growth in Studies",border="blue",ylim = c(0,200))

barplot(PE.c$cum,names.arg = PE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,200),
        main = "a) Price Elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=1.2,cex.lab=1.2, cex.main=1.3,
        las=1, space = 2)


#barplot(cum~year,ylim=c(0,180), data = PE.c)

## Income elasticity

IE.c = read.csv("cum_IE.csv")
names(IE.c)
#barplot(IE.c$cum,names.arg=IE.c$year,xlab="Publication Year",
#       ylab="Number of Studies",col="grey",
#      main="",border="blue",ylim = c(0,180))

#barplot(cum~year, data = IE.c)

barplot(IE.c$cum,names.arg = IE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,200),
         main = "b) Income Elasticity",
             xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=1.2,cex.lab=1.2, cex.main=1.3,
             las=1, space = 2)


## People elasticity
dev.off()
HE.c = read.csv("cum_HE.csv")
names(HE.c)
#barplot(IE.c$cum,names.arg=IE.c$year,xlab="Publication Year",
#       ylab="Number of Studies",col="grey",
#      main="",border="blue",ylim = c(0,180))

#barplot(cum~year, data = IE.c)

barplot(HE.c$cum,names.arg = HE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,80),
        main = "b) Growth in studies reporting people elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=1.3,cex.lab=1.4, cex.main=1.5,
        las=1, space = 2)

?barplot

library(ggplot2)
ggplot(data = IE.c, aes(x =IE.c$year))+
        geom_bar(fill = "coral", lpha = 0.5) +
        theme_classic()

## another option
barplot(HE.c$cum,names.arg = HE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,80),
        main = "b) Growth in studies reporting people elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=1.2,cex.lab=1.2, cex.main=1.3,
        las=2.5, space = 2)


###Global HHS trend
library(tidyverse)
hhs = read.csv("HHS_Global.csv")
names(hhs)
str(hhs)
hhs$Year<-as.Date(hhs$Year,"%d/%m/%Y")
hhsr<-na.omit(hhs)
summary(hhsr)

#barplot(IE.c$cum,names.arg=IE.c$year,xlab="Publication Year",
#       ylab="Number of Studies",col="grey",
#      main="",border="blue",ylim = c(0,180))

#barplot(cum~year, data = IE.c)

barplot(hhsr$Ave.HHS,names.arg = hhsr$Year,beside = TRUE, axes = TRUE,
       # ylim=c(0,80),#
        main = "b) Growth in studies reporting people elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=1.3,cex.lab=1.4, cex.main=1.5,
        las=1, space = 2)



date=hhs$Date
year= as.Date(date,format="%m/%d/%y")


plot(hhs$Ave.HHS, hhs$Year)

###############################################################################

## Summary statistics

ghhs= read.csv("HHS_Global.csv",header=T)
head(ghhs)
sapply(ghhs, mode)
sapply(ghhs,class)

ghhs$Ave.HHS<-as.numeric(as.character(ghhs$Ave.HHS))
sapply(ghhs,class)
nghhs<-na.omit(ghhs)
head(nghhs)
summary(nghhs$Ave.HHS)
#transform(nghhs,Ave.HHS = as.numeric(Ave.HHS))
mean(nghhs$Ave.HHS)
summary(as.numeric(nghhs$Ave.HHS))
sd(nghhs$Ave.HHS)
dim(ghhs)
dim(nghhs)
summary(as.numeric(ghhs$Ave.HHS))

#1970
d.70= subset(nghhs, Year=="1970")
tapply(d.70$Ave.HHS,d.70$Region, mean)

#1980
d.80= subset(nghhs, Year=="1980")
tapply(d.80$Ave.HHS,d.80$Region, mean)

#1990
d.90= subset(nghhs, Year=="1990")
tapply(d.90$Ave.HHS,d.90$Region, mean)

#2000
d.200= subset(nghhs, Year=="2000")
tapply(d.200$Ave.HHS,d.200$Region, mean)

#2010
d.010= subset(nghhs, Year=="2010")
tapply(d.010$Ave.HHS,d.010$Region, mean)

#2015
d.015= subset(nghhs, Year=="2015")
tapply(d.015$Ave.HHS,d.015$Region, mean)


#THE END ##############################################
#@Alemken J.############################################
