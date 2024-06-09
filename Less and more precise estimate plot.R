
## Ploting more precise and less precise estimates

mdata <- read.csv('mrwp_mdata.csv',header=T,sep=",")
head (mdata);str(mdata)
attach(mdata)
pem=subset(mdata, y_p_d=="P")
head(pem);str(pem)
names(pem)
attach(pem)
iem=subset(mdata,y_p_d=="Y")
head(iem);str(iem)
hem=subset(mdata,y_p_d=="D")
head(hem);str(hem)
any(is.na(pem$samplesize))
sum(is.na(pem$estimator))
######################
# removing na values from the entire dataset
View(mdata)
##### quicker way to do it ######
mdatan.na.omit<-na.omit(mdata)
mdatan.na.omit
#to make the names shorter#
mdatan<-na.omit(mdata)
mdatan
### creating a subset for each elasticity type with na's removed ###
pemn=subset(mdatan,y_p_d=="P")
iemn=subset(mdatan,y_p_d=="Y")
hemn=subset(mdatan,y_p_d=="D")

dev.off()


mp3 <- robu (formula=est~1+I(se^2), data =pemn,studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mp3)

0.346 /2.8
SEP1=subset(pemn,se<0.1203571)
View(SEP1)
mp4 <- robu (formula=est~1, data =SEP1,studynum = study_id, 
             var.eff.size =I(se^2), rho = .8,small = TRUE)
print(mp4)

## plotting using elasticity estimates ##

dev.off()

#layout(matrix(c(1,2,3),nrow=3),heights=c(1,1,0.9))
#par(mai=c(0,0.7,0.3,0.7))

layout(matrix(1:3,ncol=1,nrow = 3) ,heights = c(1,1,1))
par(mar = c(4, 4, 0.7, 0.6) + 0.5)
y=pemn$est
x=1:length(y)
plot(y,pch=4,xlab = '',ylab='Price Elasticity',ylim=c(-4,4),col='darkgreen')


lines(y,col='darkgreen')
se=pemn$se
segments(x, y-se,x, y+se,col='darkgreen')
epsilon = 0.01
segments(x-epsilon,y-se,x+epsilon,y-se,col='darkgreen')
segments(x-epsilon,y+se,x+epsilon,y+se,col='darkgreen')
mtext('Less and more precise estimates',side=3,line=0.1,cex=1,font=6,adj=0.4)
#xlabb='0                                                             0.098                                                              1.5'


y=SEP1$est
est=y
x=1:length(y)
lines(y,col='magenta')
se1=SEP1$se
segments(x, y-se1,x, y+se1,col='magenta')
epsilon = 0.01
segments(x-epsilon,y-se1,x+epsilon,y-se1,col='magenta')
segments(x-epsilon,y+se1,x+epsilon,y+se1,col='magenta')
points(y,pch=21,bg='white',col='magenta')

#text(1,4,'a) Unfiltered and Filtered SE',pos=4)
legend('top',c('The whole sample','Subsample (more precise estimates)'),
       lty=1,pch=c(4,21),bty='n',bg='white',
       col=c('darkgreen','magenta'))

## Income ##
0.171/2.8
SEI1=subset(iemn,se<0.06107143)

#par(mai=c(0,0.7,0.3,0.7))
par(mar = c(4, 4, 0.7, 0.6) + 0.5)
y=iemn$est
x=1:length(y)
plot(y,pch=16,xlab="",ylab='Income Elasticity',ylim=c(-2,4),col='blue')
lines(y,col='blue')
se=iemn$se
segments(x, y-se,x, y+se,col='blue')
epsilon = 0.01
segments(x-epsilon,y-se,x+epsilon,y-se,col='blue')
segments(x-epsilon,y+se,x+epsilon,y+se,col='blue')
# mtext('Less V more precise estimate',side=3,line=0.1,cex=1.3,font=6,adj=0.1)
#xlabb='0                                                             0.098                                                              1.5'


y=SEI1$est
est=y
x=1:length(y)
lines(y,col='red')
se1=SEI1$se
segments(x, y-se1,x, y+se1,col='red')
epsilon = 0.05
segments(x-epsilon,y-se1,x+epsilon,y-se1,col='red')
segments(x-epsilon,y+se1,x+epsilon,y+se1,col='red')
points(y,pch=8,bg='white',col='red')

#text(1,4,'a) Unfiltered and Filtered SE',pos=4)
legend('top',c('The whole sample','Subsample (more precise estimates)'),
       lty=1,pch=c(16,8),bty='n',bg='white',
       col=c('blue','red'))

## HHS##
0.277/2.8
SEH1=subset(hemn,se<0.09892857)

#par(mai=c(0,0.7,0.3,0.7))
par(mar = c(4, 4, 0.7, 0.6) + 0.5)
y=hemn$est
x=1:length(y)
plot(y,pch=18,xlab = "",ylab='HHS Elasticity',ylim=c(-2,3),col='darkred')
lines(y,col='darkred')
se=hemn$se
segments(x, y-se,x, y+se,col='darkred')
epsilon = 0.02
segments(x-epsilon,y-se,x+epsilon,y-se,col='darkred')
segments(x-epsilon,y+se,x+epsilon,y+se,col='darkred')
#mtext('Less V more precise estimate',side=3,line=0.1,cex=1.3,font=6,adj=0.1)
xlabb='0                                                             0.098                                                              1.5'


y=SEH1$est
est=y
x=1:length(y)
lines(y,col='green')
se1=SEH1$se
segments(x, y-se1,x, y+se1,col='green')
epsilon = 0.05
segments(x-epsilon,y-se1,x+epsilon,y-se1,col='green')
segments(x-epsilon,y+se1,x+epsilon,y+se1,col='green')
points(y,pch=17,bg='white',col='green')

# text(1,4,'a) Unfiltered and Filtered SE',pos=4)
legend('topright',c('The whole sample','Subsample (more precise estimates)'),
       lty=1,pch=c(18,17),bty='n',bg='white',
       col=c('darkred','green'))

#axis(4,labels=seq(0,30,by=5),at=seq(0,30,by=5)*7)

#axis(4,labels=seq(0,30,by=5),at=seq(0,30,by=5)*7)

dev.off()

## Plotting using SE ##

layout(matrix(1:3,ncol=1,nrow = 3) ,heights = c(1,1,1))
par(mar = c(4, 4, 0.7, 0.6) + 0.5)
y=pemn$se
x=1:length(y)
plot(y,pch=20,xlab='',ylab='SE (Price Elasticity)',ylim=c(0,1.6),col='darkgreen')
lines(y,col='darkgreen')
#points(y,pch=4,bg='white',col='darkgreen')
#est=pemn$est
#segments(x, y-est,x, y+est,col='darkgreen')
#epsilon = 0.05
#segments(x-epsilon,y-se,x+epsilon,y-se,col='darkgreen')
#segments(x-epsilon,y+se,x+epsilon,y+se,col='darkgreen')
mtext('Less and more precise estimates',side=3,line=0.1,cex=1,font=6,adj=0.4)

y=SEP1$se
x=1:length(y)

lines(y,col='magenta')
se1=SEP1$se
segments(x, y-se1,x, y+se1,col='magenta')
epsilon = 0.01
segments(x-epsilon,y-se1,x+epsilon,y-se1,col='magenta')
segments(x-epsilon,y+se1,x+epsilon,y+se1,col='magenta')
points(y,pch=18,bg='white',col='magenta')
legend('top',c('The whole sample','Subsample (more precise estimates)'),
       lty=1,pch=c(20,18),bty='n',bg='white',
       col=c('darkgreen','magenta'))

## income ##
par(mar = c(4, 4, 0.7, 0.6) + 0.5)
y=iemn$se
x=1:length(y)
plot(y,pch=16,xlab='',ylab='SE (Income Elasticity)',ylim=c(0,2),col='darkblue')
lines(y,col='darkblue')

y=SEI1$se
x=1:length(y)

lines(y,col='red')
se1=SEI1$se
segments(x, y-se1,x, y+se1,col='red')
epsilon = 0.05
segments(x-epsilon,y-se1,x+epsilon,y-se1,col='red')
segments(x-epsilon,y+se1,x+epsilon,y+se1,col='red')
points(y,pch=8,bg='white',col='red')
legend('top',c('The whole sample','Subsample (more precise estimates)'),
       lty=1,pch=c(16,8),bty='n',bg='white',
       col=c('darkblue','red'))

##HHS elasticity ##
par(mar = c(4, 4, 0.7, 0.6) + 0.5)
y=hemn$se
x=1:length(y)
plot(y,pch=18,xlab='',ylab='SE (HHS Elasticity)',ylim=c(0,2),col='darkred')
lines(y,col='darkred')

y=SEH1$se
x=1:length(y)

lines(y,col='green')
se1=SEH1$se
segments(x, y-se1,x, y+se1,col='green')
epsilon = 0.05
segments(x-epsilon,y-se1,x+epsilon,y-se1,col='green')
segments(x-epsilon,y+se1,x+epsilon,y+se1,col='green')
points(y,pch=17,bg='white',col='green')
legend('top',c('The whole sample','Subsample (more precise estimates)'),
       lty=1,pch=c(18,17),bty='n',bg='white',
       col=c('darkred','green'))
#####################################################
## another way to do it ##

mdata <- read.csv('mrwp_mdata.csv',header=T,sep=",")

dev.off()

par(mfrow=c(2,1))
## Price ##
0.348/2.8
SEP1=subset(pemn,se<0.1242857)

layout(matrix(1:3,ncol=1,nrow = 3) , heights = c(1,1.1,1))
par(mar = c(4, 4, 1, 0) + 0.5)

with(pemn,plot(est,se,pch=20,cex=1.5,col='darkgreen',xlab='Price Elasticity',ylab=
    'Standard Error',xlim = c(-4,4),ylim = c(0,3))) 
with(SEP1,points(est,se,pch=18,cex=1.5,col='blueviolet')) 
legend('topleft',c('Less Precise','More Precise'),pch=c(20,18),
       pt.cex=1.5,inset=0.04, col=c('darkgreen','blueviolet')) 

## Income ##
par(mar = c(4, 4, 1, 0) + 0.5)
0.171/2.8
SEI1=subset(iemn,se<0.06107143)

with(iemn,plot(est,se,pch=15,cex=.9,col='darkblue',xlab='Income Elasticity',ylab=
    'Standard Error',xlim = c(-1, 4), ylim = c(0,2))) 
with(SEI1,points(est,se,pch=17,cex=1.4,col='orangered2')) 
legend('topleft',c('Less Precise','More Precise'),pch=c(15,17),
       pt.cex=1.2,inset=0.04, col=c('darkblue','orangered2'))

## HHS Elasticity ##
0.277/2.8
SEH1=subset(hemn,se<0.09892857)
with(hemn,plot(est,se,pch=19,cex=1,col='darkred',xlab='HHS Elasticity',ylab='Standard Error')) 
with(SEH1,points(est,se,pch=18,cex=1.5,col='mediumseagreen')) 
legend('topright',c('Less Precise','More Precise'),pch=c(19,18),
       pt.cex=1.5,inset=0.04, col=c('darkred','mediumseagreen'))

dev.off()
### Density Plot
par(mfrow=c(2,1)) 
#lines(xnorm1,ynorm1,col="red",lty=2)
density(pemn$est)
plot(density(pemn$est),pch=19,lwd=1,col='blue',lty=1,ylim=c(0,2.5),xlim=c(-4,4),ylab = "Density", xlab = "Price Elasticity",main = "")
lines(density(SEP1$est),pch=18,lwd=2,col='red',lty=2 )

legend("topright", legend = c("Exclude", "Include"),
       col = c("blue", "red"), lty = 1:2, cex = 1,inset=0.02)


#lines(xnorm1,ynorm1,col="red",lty=2)
density(iemn$est)
plot(density(iemn$est),pch=19,lwd=1,col='blue',lty=1,ylim=c(0,5),xlim=c(-2,4),ylab = "Density", xlab = "Income Elasticity",main = "")
lines(density(SEI1$est),pch=18,lwd=2,col='red',lty=2 )

legend("topright", legend = c("Exclude", "Include"),
       col = c("blue", "red"), lty = 1:2, cex = 0.9,inset=0.02)

density(hemn$est)
plot(density(hemn$est),pch=19,lwd=1,col='blue',lty=1,ylim=c(0,5),xlim=c(-2,4),ylab = "Density", xlab = "HHS Elasticity",main = "")
lines(density(SEI1$est),pch=18,lwd=2,col='red',lty=2 )
legend("topright", legend = c("Exclude", "Include"),
       col = c("blue", "red"), lty = 1:2, cex = 1,inset=0.02)

dev.off()


