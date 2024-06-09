install.packages("PublicationBias")
install.packages("robumeta")
library(PublicationBias)
library(robumeta)
library(metafor)


###### EXAMPLE WITH SKEWED TRUE EFFECT SIZES ####### 

eta = 10

# simulate data with skewed true effects
k = 200
V = 0

set.seed(seed+2)
sei = runif( n = k,
             min = 1,
             max = 2 )

set.seed(seed+3)
thi = rexp( n = k, 
            rate = 1 )
hist(thi)
mean(thi)

set.seed(seed+4)
yi = rnorm( n = k,
            mean = thi,
            sd = sei )
hist(yi)


d = data.frame( yi, sei, vi = sei^2 )
d$pval = 2 * ( 1 - pnorm( abs( yi / sei ) ) )

# induce publication bias
# 1-tailed publication bias
signif = d$pval < 0.05 & d$yi > 0
publish = rep( 1, nrow(d) )
set.seed(seed+5)
publish[ signif == FALSE ] = rbinom( n = sum(signif == FALSE), size = 1, prob = 1/eta)
d = d[ publish == TRUE, ]
########################################################################


## Now using real data ##
##### Make Significance Funnel #####
dev.off()
wcd=read.csv("mrwp_mdata.csv")
head(wcd)
pe=subset(wcd,y_p_d=="P")
wcd=read.csv("mrwp_mdata.csv")
PE=subset(wcd,y_p_d=="P")
head(PE)
PE$pval = 2 * ( 1 - pnorm( abs( PE$est/PE$se ) ) )

#STANDARD RANDOM-EFFECTS
meta.RE = rma.uni( yi = PE$est, 
                   vi = PE$var,
                   method = "REML",
                   knha = TRUE) 
print(meta.RE)

# naive
meta.naive = robu( formula = est ~ 1,
                   effect = est,
                   var.eff.size = var,
                   data = PE,
                   studynum =1:nrow(PE), 
                   small = TRUE)

print(meta.naive)

## Hedges et al. or Tipton (2015)
mh1 <- robu (formula=est~1, data =PE, studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mh1)

##Clustered 

meta.RI.rob = robu(est ~ 1, 
                      data = PE, 
                      cluster=1:nrow(PE),
                      var.eff.size = var,
                      small = TRUE) 

print(meta.RI.rob) 
## doesn't look good

# here the point estimate is negative, so nonaffirmatives could have positive estimates
PE$nonaffirm  = PE$pval > 0.05 | PE$est > 0

# # keep only 1 nonaffirmative study
# ( N = which( dat$nonaffirm == 1) )
# dat = dat[ -c(1,5,8,12), ]
# table(dat$nonaffirm)



#####Fixed-Effects Clustered Specification #####
sval.1 = svalue( yi = PE$est,
               vi = PE$var,
               q = 0,
               favor.positive = FALSE,
               model = "fixed",
               return.worst.meta = TRUE )

print(sval.1)

##### Robust Clustered Specification #####

sval.2 = svalue( yi = PE$est,
               vi = PE$var,
               q = 0,
               favor.positive = FALSE,
               model = "robust",
               return.worst.meta = TRUE )
print(sval.2)

# worst-case
worst = PE[ PE$est < 0 | PE$pval > 0.05, ]

meta.worst = robu( formula = worst$est ~ 1,
                   effect = worst$est,
                   var.eff.size = worst$var,
                   data = worst,
                   studynum = 1:nrow(worst), 
                   small = TRUE )

print(meta.worst)


##### Worst-Case Meta-Analysis ##### 
disf = PE[ PE$est < 0 | PE$pval > 0.05, ]

# put each study in its own cluster
my.cluster = 1:nrow(disf)

meta.worst.re.rob = robu( formula = disf$est ~ 1,
                          effect = disf$est,
                          var.eff.size = disf$var,
                          data = disf,
                          studynum = my.cluster, 
                          small = TRUE )

print(meta.worst.re.rob)

funnel2 = significance_funnel( yi = PE$est,
                               vi = PE$var,
                               favor.positive = FALSE,
                               est.N = meta.worst$b.r,  # on analysis scale, not transformed to original scale
                               est.all = meta.naive$b.r,
                               alpha = 0.05,
                               plot.pooled = TRUE )



##### Make Standard Funnel #####

library(metafor)

m = rma.uni( yi = PE$est,
             vi = PE$se,
             method = "REML" )
print(m)

metafor::funnel(m, 
                xlim = c(-3.5, 7.5),
                at = seq(-2.5, 4, 2.5),
                ylim = c(0,2),
                level=c(90,95),
                shade=c("white", "gray55", "gray75"),
                legend=TRUE,
                refline = 0,
                xlab = "Point estimate",
                ylab = "Estimated standard error",
                steps = 5)
dev.off()
############################################################################

### Now Estimate publication bias-corrected meta-analysis ###
#1, Standard FE specification
corrected_meta( yi = PE$est,
                vi = PE$var,
                eta = 1,
                model = "fixed",
                favor.positive = FALSE)

# assume a known selection ratio of 5
# i.e., affirmative results are 5x more likely to be published
#  than nonaffirmative

corrected_meta( yi = PE$est,
                vi = PE$var,
                eta = 5,
                model = "fixed",
                favor.positive = FALSE)

# 2. Standard RE specification

corrected_meta(yi = PE$est, 
               vi = PE$var,
               eta = 5,
               model = "random",
               favor.positive = FALSE) 
## this looks not working



# 3. Robust clustered specification ??
# same selection ratio, but now account for heterogeneity
# and clustering via robust specification
corrected_meta( yi = PE$est,
                vi = PE$var,
                eta = 5,
                favor.positive = FALSE,
                clustervar = PE$study_id,
                model = "robust")

# 4. Robust independent specification ??

corrected_meta( yi = PE$est,
                vi = PE$var,
                eta = 5,
                favor.positive = FALSE,
                clustervar = PE$est,
                model = "robust")
###############################################################################


##### Make sensitivity plot as in Mathur & VanderWeele (2020) #####
# range of parameters to try (more dense at the very small ones)

eta.list = as.list( c( 200, 150, 100, 50, 40, 30, 20, rev( seq(1,15,1) ) ) )
res.list = lapply( eta.list, function(x) {
    cat("\n Working on eta = ", x)
    return( corrected_meta( yi = PE$est,
                            vi = PE$var,
                            eta = x,
                            model = "robust",
                            favor.positive = FALSE,
                            clustervar = PE$study_id) )
}
)
print(res.list)

# put results for each eta in a dataframe
res.df = as.data.frame( do.call( "rbind", res.list ))

write.csv(res.df, file="p.elast.est.csv")

library(ggplot2)
ggplot( data = res.df, aes( x = eta, y = est)) +
    geom_ribbon( data = res.df, aes( x = eta, ymin = 
    lo, ymax = hi ), fill = "grey") +
    geom_line( lwd = 1.2 ) +
    xlab( bquote( eta ) ) +
    ylab( bquote( hat(mu)[eta] ) ) +
    theme_classic(base_size = 12)

###Plot one-tailed p-values###
PE$est = -PE$est

pval_plot(yi=PE$est, vi=PE$var, alpha.select = 0.05)

### Make significance funnel plot ###

meta.R =  robu( est ~ 1,
                  studynum = 1:nrow(PE),
                  data = PE,
                  var.eff.size = var,
                  small = TRUE)
print(meta.R)

# optional: calculate worst-case estimate (for the gray diamond)
#  by analyzing only the nonaffirmative studies
PE$pval = 2 * ( 1 - pnorm( abs( PE$est/sqrt(PE$var))))  # two-tailed p-value
PE$affirm = (PE$est > 0) & (PE$pval < 0.05)  # is study affirmative?
meta.worst =  robu( est ~ 1,
                    studynum = 1:nrow(PE[PE$affirm == FALSE,]),
                    data = PE[PE$affirm == FALSE,],
                    var.eff.size = var,
                    small = TRUE)
print(meta.worst)

##### Make Significance Funnel with Alpha = 0.50 and Default Pooled Estimates #####
# change alpha to 0.50 just for illustration
# now the pooled estimates are from the fixed-effect specification because they are
#  not provided by the user
significance_funnel(yi = PE$est,
                     vi = PE$var,
                     favor.positive = FALSE,
                     alpha.select = 0.5,
                     plot.pooled = TRUE )
#########################################################################


### Severity of publication bias needed to "explain away" results ###

##### Fixed-Effects Specification #####
# S-values and worst-case meta-analysis under fixed-effects specification
svals.FE.1 = svalue( yi = PE$est,
                     vi = PE$var,
                     q = 0,
                     favor.positive = FALSE, # default is F
                     model = "fixed" )
print(svals.FE.1)

# not clear how to set the q value

##### Robust Clustered Specification #####
svals.Robu.1= svalue( yi = PE$est,
              vi = PE$var,
             q = 0,
            favor.positive =FALSE, # default is F
            model = "robust")
print(svals.Robu.1)

###########################################################################
###Income Elsticity #######

dev.off()
wcd=read.csv("mrwp_mdata.csv")
head(wcd)
IE=subset(wcd,y_p_d=="Y")
wcd=read.csv("mrwp_mdata.csv")
IE=subset(wcd,y_p_d=="Y")
head(IE)
IE$pval = 2 * ( 1 - pnorm( abs( IE$est/IE$se ) ) )

#STANDARD RANDOM-EFFECTS
meta.RE = rma.uni( yi = IE$est, 
                   vi = IE$var,
                   method = "REML",
                   knha = TRUE) 
print(meta.RE)

# naive
meta.naive = robu( formula = est ~ 1,
                   effect = est,
                   var.eff.size = var,
                   data = IE,
                   studynum =1:nrow(IE), 
                   small = TRUE)

print(meta.naive)

## Hedges et al. or Tipton (2015)
mh1 <- robu (formula=est~1, data =IE, studynum = study_id, 
             var.eff.size =var, rho = .8,small = TRUE)
print(mh1)

##Clustered 

meta.RI.rob = robu(est ~ 1, 
                   data = IE, 
                   cluster=1:nrow(IE),
                   var.eff.size = var,
                   small = TRUE) 

print(meta.RI.rob) 
## doesn't look good
# here the point estimate is negative, so nonaffirmatives could have positive estimates
PE$nonaffirm  = PE$pval > 0.05 | PE$est > 0
# here the point estimate is positive, so nonaffirmatives could have negative estimates
IE$nonaffirm  = IE$pval > 0.05 | IE$est > 0

# # keep only 1 nonaffirmative study
# ( N = which( dat$nonaffirm == 1) )
# dat = dat[ -c(1,5,8,12), ]
# table(dat$nonaffirm)



#####Fixed-Effects Clustered SIEcification #####
sval.1 = svalue( yi = IE$est,
                 vi = IE$var,
                 q = 0,
                 favor.positive = T,
                 model = "fixed",
                 return.worst.meta = TRUE )

print(sval.1)

##### Robust Clustered Specification #####

sval.2 = svalue( yi = IE$est,
                 vi = IE$var,
                 q = 5,
                 favor.positive = TRUE,
                 model = "robust",
                 return.worst.meta = TRUE)
print(sval.2)

# worst-case
worst = IE[ IE$est < 0 | IE$pval > 0.05, ]

meta.worst = robu( formula = worst$est ~ 1,
                   effect = worst$est,
                   var.eff.size = worst$var,
                   data = worst,
                   studynum = 1:nrow(worst), 
                   small = TRUE )

print(meta.worst)


##### Worst-Case Meta-Analysis ##### 
disf = IE[ IE$est < 0 | IE$pval > 0.05, ]

# put each study in its own cluster
my.cluster = 1:nrow(disf)

meta.worst.re.rob = robu( formula = disf$est ~ 1,
                          effect = disf$est,
                          var.eff.size = disf$var,
                          data = disf,
                          studynum = my.cluster, 
                          small = TRUE )

print(meta.worst.re.rob)

funnel2 = significance_funnel( yi = IE$est,
                               vi = IE$var,
                               favor.positive = FALSE,
                               est.N = meta.worst$b.r,  # on analysis scale, not transformed to original scale
                               est.all = meta.naive$b.r,
                               alpha = 0.05,
                               plot.pooled = TRUE )



##### Make Standard Funnel #####

library(metafor)

m = rma.uni( yi = IE$est,
             vi = IE$se,
             method = "REML" )
print(m)

metafor::funnel(m, 
                xlim = c(-3.5, 7.5),
                at = seq(-2.5, 4, 2.5),
                ylim = c(0,2),
                level=c(90,95),
                shade=c("white", "gray55", "gray75"),
                legend=TRUE,
                refline = 0,
                xlab = "Point estimate",
                ylab = "Estimated standard error",
                steps = 5)
dev.off()
############################################################################

### Now Estimate publication bias-corrected meta-analysis ###
#1, Standard FE sIEcification
corrected_meta( yi = IE$est,
                vi = IE$var,
                eta = 1,
                model = "fixed",
                favor.positive = FALSE)

# assume a known selection ratio of 5
# i.e., affirmative results are 5x more likely to be published
#  than nonaffirmative

corrected_meta( yi = IE$est,
                vi = IE$var,
                eta = 5,
                model = "fixed",
                favor.positive = FALSE)

# 2. Standard RE sIEcification

corrected_meta(yi = IE$est, 
               vi = IE$var,
               eta = 5,
               model = "random",
               favor.positive = FALSE) 
## this looks not working



# 3. Robust clustered sIEcification ??
# same selection ratio, but now account for heterogeneity
# and clustering via robust sIEcification
corrected_meta( yi = IE$est,
                vi = IE$var,
                eta = 5,
                favor.positive = FALSE,
                clustervar = IE$study_id,
                model = "robust")

# 4. Robust indeIEndent sIEcification ??

corrected_meta( yi = IE$est,
                vi = IE$var,
                eta = 5,
                favor.positive = FALSE,
                clustervar = IE$est,
                model = "robust")
###############################################################################


##### Make sensitivity plot as in Mathur & VanderWeele (2020) #####
# range of parameters to try (more dense at the very small ones)

eta.list = as.list( c( 200, 150, 100, 50, 40, 30, 20, rev( seq(1,15,1) ) ) )
res.list = lapply( eta.list, function(x) {
    cat("\n Working on eta = ", x)
    return( corrected_meta( yi = IE$est,
                            vi = IE$var,
                            eta = x,
                            model = "robust",
                            favor.positive = FALSE,
                            clustervar = IE$study_id) )
}
)
print(res.list)

# put results for each eta in a dataframe
res.df = as.data.frame( do.call( "rbind", res.list ))

write.csv(res.df, file="p.elast.est.csv")

library(ggplot2)
ggplot( data = res.df, aes( x = eta, y = est)) +
    geom_ribbon( data = res.df, aes( x = eta, ymin = 
                                         lo, ymax = hi ), fill = "grey") +
    geom_line( lwd = 1.2 ) +
    xlab( bquote( eta ) ) +
    ylab( bquote( hat(mu)[eta] ) ) +
    theme_classic(base_size = 12)

###Plot one-tailed p-values###
IE$est = -IE$est

pval_plot(yi=IE$est, vi=IE$var, alpha.select = 0.05)

### Make significance funnel plot ###

meta.R =  robu( est ~ 1,
                studynum = 1:nrow(IE),
                data = IE,
                var.eff.size = var,
                small = TRUE)
print(meta.R)

# optional: calculate worst-case estimate (for the gray diamond)
#  by analyzing only the nonaffirmative studies
IE$pval = 2 * ( 1 - pnorm( abs( IE$est/sqrt(IE$var))))  # two-tailed p-value
IE$affirm = (IE$est > 0) & (IE$pval < 0.05)  # is study affirmative?
meta.worst =  robu( est ~ 1,
                    studynum = 1:nrow(IE[IE$affirm == FALSE,]),
                    data = IE[IE$affirm == FALSE,],
                    var.eff.size = var,
                    small = TRUE)
print(meta.worst)

##### Make Significance Funnel with Alpha = 0.50 and Default Pooled Estimates #####
# change alpha to 0.50 just for illustration
# now the pooled estimates are from the fixed-effect sIEcification because they are
#  not provided by the user
significance_funnel(yi = IE$est,
                    vi = IE$var,
                    favor.positive = FALSE,
                    alpha.select = 0.5,
                    plot.pooled = TRUE )
#########################################################################


### Severity of publication bias needed to "explain away" results ###

##### Fixed-Effects SIEcification #####
# S-values and worst-case meta-analysis under fixed-effects sIEcification
svals.FE.1 = svalue( yi = IE$est,
                     vi = IE$var,
                     q = 0,
                     favor.positive = FALSE, # default is F
                     model = "fixed" )
print(svals.FE.1)

# not clear how to set the q value

##### Robust Clustered SIEcification #####
svals.Robu.1= svalue( yi = IE$est,
                      vi = IE$var,
                      q = 0,
                      favor.positive =FALSE, # default is F
                      model = "robust")
print(svals.Robu.1)


### THE END ###
