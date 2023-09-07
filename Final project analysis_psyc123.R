# Project setup ---------------------------------------------------
##Load Packages
suppressMessages(library(ggplot2))
suppressMessages(library(psych))
suppressMessages(library(bayesrules))
suppressMessages(library(tidyverse))
suppressMessages(library(rstan))
suppressMessages(library(coda))
suppressMessages(library(bayesplot))
suppressMessages(library(bayestestR))
suppressMessages(library(rstanarm))
suppressMessages(library(logspline))
suppressMessages(library(interactions))
suppressMessages(library(ppcor))

##Set up environment for Bayesian modeling
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
##Set working directory
setwd('C:/Users/ktcv3/My Drive/Research/databases/')
##Load data. Data will be loaded as object called "rmset"
load('RMAdataforbayesean.RData')

## Descriptives ==========================
##Look at descriptives of continuous variables
descriptive.stats<-sapply(rmset[,c('Age', 'HosWom', 'emp', 'shvic', 'teen_tot_fem', 'teen_tot_male', 'pt', 'frint')],
                     function(x) {c(n=length(x), mean=mean(x),
                                    sd=sd(x), se=sd(x)/sqrt(length(x)),
                                    min=min(x), max=max(x))})
##Present descriptives in table
round(t(descriptive.stats), 3)
##Check frequencies
##Look at gender frequencies
gen.counts<-cbind(table(rmset$resp.sex)) ##Regular frequencies
gen.relfreq<-cbind(table(rmset$resp.sex)/nrow(rmset)) ##Relative frequencies
gen.freq<-cbind(gen.counts, gen.relfreq) ##Combine into table
gender.labels<-c('Female', 'Male') ##Create gender labels
column.labs<-c('Frequency', 'Relative Frequency') ##Create Column labels
colnames(gen.freq)<-column.labs ##Apply column labels to table
gen.freq
##Race
##Start by summing each of the race variables
afam.sum.2<-sum(rmset$race_0=='African-American', na.rm = TRUE)
asian.sum.2<-sum(rmset$race_1=='Asian', na.rm=TRUE)
cauc.sum.2<-sum(rmset$race_2=='Caucasian', na.rm=TRUE)
hisp.sum.2<-sum(rmset$race_3=='Hispanic', na.rm=TRUE)
na.sum.2<-sum(rmset$race_4=='1', na.rm=TRUE)
oth.sum.2<-sum(rmset$race_5=='Other', na.rm=TRUE)
##Get the relative frequencies
afam.rf.2<-afam.sum.2/nrow(rmset)
asian.rf.2<-asian.sum.2/nrow(rmset)
cauc.rf.2<-cauc.sum.2/nrow(rmset)
hisp.rf.2<-hisp.sum.2/nrow(rmset)
na.rf.2<-na.sum.2/nrow(rmset)
oth.rf.2<-oth.sum.2/nrow(rmset)
#collect into a table
afam.data.2<-cbind(afam.sum.2, afam.rf.2)
asian.data.2<-cbind(asian.sum.2, asian.rf.2)
cauc.data.2<-cbind(cauc.sum.2, cauc.rf.2)
hisp.data.2<-cbind(hisp.sum.2, hisp.rf.2)
na.data.2<-cbind(na.sum.2, na.rf.2)
oth.data.2<-cbind(oth.sum.2, oth.rf.2)
race.labs<-c('African American', 'Asian', 'Caucasian', 'Hispanic', 'Native American', 'Other')
races<-rbind(race.labs)
race.col.labs<-c('Race','Frequency', 'Relative Freqency')
race.data.2<-rbind(afam.data.2, asian.data.2, cauc.data.2, hisp.data.2, na.data.2, oth.data.2)
race.table.2<-cbind(race.labs, race.data.2)
colnames(race.table.2)<-race.col.labs
##row.names(race.data)<-race.labs
race.table.2
##Graph race data
race.data.df.2<-(as.data.frame.matrix(race.table.2))
race.graph.2<-ggplot(race.data.df.2, aes(x=Race, y=as.numeric(Frequency)))+
  geom_bar(color = "#b1b3b4", fill = "#03a9fc", stat="identity")+
  scale_y_continuous(breaks=c(0,50,100,150,200, 250, 300, 350, 400, 450, 500))+
  ylab("Frequency")+
  ggtitle("Frequency of Races")+
  theme(plot.title=element_text(hjust=0.5))
race.graph.2


##Get Information about school of origin -- This is based on an email with Ray
##Create function to count occurances
getcount<-function(data, keyword)
{
  wcount<-str_count(rmset$Filename, keyword)
  return(data.frame(data,wcount))
}
##Run function for variations for fem and mal
femcount<-getcount(rmset$Filename, 'fem')
malcount<-getcount(rmset$Filename, 'mal')
Femcount<-getcount(rmset$Filename, 'Fem')
Malcount<-getcount(rmset$Filename, 'Mal')
##Sum occurances
umfem<-sum(femcount$wcount==1)
ummal<-sum(malcount$wcount==1)
umFem<-sum(Femcount$wcount==1)
umMal<-sum(Malcount$wcount==1)
##Get total from UMass Dartmouth
(totalum<-(umfem+ummal+umFem+umMal))
##Get total Brandeis
(totalbrandeis<-length(rmset$Filename)-totalum)
##Make variable based on school
##Create list of UMass identifiers
umasslist<-c('fem', 'mal', 'Fem', 'Mal')
##rmset$school<-ifelse(grepl(umasslist, rmset$filename), 1, 0)
rmset$fem<-ifelse(grepl("fem", rmset$Filename), 1, 0)
rmset$Fem<-ifelse(grepl("Fem", rmset$Filename), 1, 0)
rmset$mal<-ifelse(grepl("mal", rmset$Filename), 1, 0)
rmset$Mal<-ifelse(grepl("Mal", rmset$Filename), 1, 0)

rmset$school<-rowSums(cbind(rmset$fem, rmset$Fem, rmset$mal, rmset$Mal))
sum(rmset$school)

##Create factor for school
rmset<-within(rmset, {school.fact<-factor(school, levels=c(0, 1), 
                                          labels=c('Brandeis', 'UMass'))})

##Look at school frequences
sch.counts<-cbind(table(rmset$school.fact)) ##Regular frequencies
sch.relfreq<-cbind(table(rmset$school.fact)/nrow(rmset)) ##Relative frequencies
sch.freq<-cbind(sch.counts, sch.relfreq) ##Combine into table
Sch.labels<-c('Brandeis', 'UMass') ##Create gender labels
column.labs<-c('Frequency', 'Relative Frequency') ##Create Column labels
colnames(sch.freq)<-column.labs ##Apply column labels to table
sch.freq

## Plots ==========


##Look at boxplots of variables and predictors
ggplot(rmset)+geom_boxplot(aes(y=HosWom))+ggtitle("Distribution of rape myth acceptance")
ggplot(rmset)+geom_boxplot(aes(y=teen_tot_fem))+ggtitle("Distribution of friendshps with girls")
ggplot(rmset)+geom_boxplot(aes(y=teen_tot_male))+ggtitle("Distribution of friendshps with boys")
ggplot(rmset)+geom_boxplot(aes(y=shvic))+ggtitle("Distribution of sexual harassment victimization")
ggplot(rmset)+geom_boxplot(aes(y=emp))+ggtitle("Distribution of empathy")
ggplot(rmset)+geom_boxplot(aes(y=pt))+ggtitle("Distribution of lack of perspective taking")
ggplot(rmset)+geom_boxplot(aes(y=frint))+ggtitle("Distribution of friendship intimacy")
##Create scatterplots of predictors and outcome
##layout(matrix(c(1, 3, 2, 4), 2, 2))
plot(rmset$teen_tot_fem, rmset$HosWom, 
     main = 'Relation between freindships with girls and rape myth acceptance', 
     col = 'blue', xlab = 'Friendships with girls', ylab = 'Rape myth acceptance')
plot(rmset$teen_tot_male, rmset$HosWom, 
     main = 'Relation between freindships with boys and rape myth acceptance', 
     col = 'blue', xlab = 'Friendships with boys', ylab = 'Rape myth acceptance')
plot(rmset$shvic, rmset$HosWom,
     main='Relation between victimization and rape myth acceptance', col='blue',
     xlab='Sexual harassment vicitmization', ylab = 'Rape myth acceptance')
plot(rmset$emp, rmset$HosWom, main='Relation between empathy and rape myth acceptance',
     col='blue', xlab = 'Empathy', ylab = 'Rape myth acceptance')
plot(rmset$frint, rmset$HosWom, main='Relation between friendship intimacy and rape myth acceptance',
     col='blue', xlab = 'Friendship intimacy', ylab = 'Rape myth acceptance')

## Variable transformations ===============

##Testing transformations
plot((rmset$teen_tot_fem)^2, rmset$HosWom, 
     main = 'Relation between freindships with girls and rape myth acceptance', 
     col = 'blue', xlab = 'Friendships with girls', ylab = 'Rape myth acceptance')
plot((rmset$teen_tot_male)^2, rmset$HosWom, 
     main = 'Relation between freindships with boys and rape myth acceptance', 
     col = 'blue', xlab = 'Friendships with boys', ylab = 'Rape myth acceptance')
plot(exp(-rmset$shvic), rmset$HosWom,
     main='Relation between victimization and rape myth acceptance', col='blue',
     xlab='Sexual harassment vicitmization', ylab = 'Rape myth acceptance')
plot((rmset$emp^(2)), rmset$HosWom, main='Relation between empathy and rape myth acceptance',
     col='blue', xlab = 'Empathy', ylab = 'Rape myth acceptance')
plot((rmset$frint)^2, rmset$HosWom, main='Relation between friendship intimacy and rape myth acceptance',
     col='blue', xlab = 'Friendship intimacy', ylab = 'Rape myth acceptance')

##Create transformed variables
##Total girl friends
rmset$teen_tot_fem.sq<-(rmset$teen_tot_fem)^2
##Total boy friends
rmset$teen_tot_male.sq<-(rmset$teen_tot_male)^2
##Victimization
rmset$shvic.nexp<-exp(-rmset$shvic)
##Friendship intimacy
rmset$frint.sq<-(rmset$frint)^2
##Empathy
rmset$emp.sq<-rmset$emp^2

##Create Matrix of scatterplots
par(mfrow=c(2, 3))
plot((rmset$teen_tot_fem)^2, rmset$HosWom, 
     main = 'Relation between friendships with girls and rape myth \nacceptance', 
     col = 'blue', xlab = 'Friendships with girls (squared)', ylab = 'Rape myth acceptance')
plot((rmset$teen_tot_male)^2, rmset$HosWom, 
     main = 'Relation between friendships with boys and rape myth \nacceptance', 
     col = 'blue', xlab = 'Friendships with boys (squared)', ylab = 'Rape myth acceptance')
plot(exp(-rmset$shvic), rmset$HosWom,
     main='Relation between victimization and rape myth \nacceptance', col='blue',
     xlab='Sexual harassment vicitmization (negative exponent)', ylab = 'Rape myth acceptance')
plot((rmset$emp^(2)), rmset$HosWom, main='Relation between empathy and rape myth \nacceptance',
     col='blue', xlab = 'Empathy (squared)', ylab = 'Rape myth acceptance')
plot((rmset$frint)^2, rmset$HosWom, main='Relation between friendship intimacy and rape myth \nacceptance',
     col='blue', xlab = 'Friendship intimacy (squared)', ylab = 'Rape myth acceptance')
##Descriptives again
##Look at descriptives of continuous variables
descriptives.2<-sapply(rmset[,c('Age', 'HosWom', 'emp.sq', 'shvic.nexp', 'teen_tot_fem.sq', 'teen_tot_male.sq', 'frint.sq')],
                       function(x) {c(n=length(x), mean=mean(x),
                                      sd=sd(x), se=sd(x)/sqrt(length(x)),
                                      min=min(x), max=max(x))})
##Present descriptives in table
round(t(descriptives.2), 3)
##Dummy code gender
rmset$dsex<-ifelse(rmset$resp.sex=='male', 0, 1)
rmset[sample(nrow(rmset), 10), c('resp.sex', 'dsex')]
##Look at means by sex
rmprevar<-data.frame(rmset$Age, rmset$HosWom, rmset$teen_tot_fem, rmset$teen_tot_male, rmset$shvic, rmset$emp, rmset$pt, rmset$frint)
describeBy(rmprevar, list(rmset$resp.sex))
##Look at means by school
describeBy(rmprevar, list(rmset$school.fact))
##Collect variables for a correlation matrix
corvars<-c("dsex", "HosWom", "teen_tot_fem.sq", "teen_tot_male.sq", "shvic.nexp", "emp.sq", "frint.sq")
(rmsetcor<-rmset[corvars])
##Create correlation and partial correlation matrices
(rm.cor<-cor(rmset[corvars], method = "pearson", use = "complete.obs"))
(rmpcor<-pcor(rmsetcor))
(pairs.panels(rmsetcor))

##Create models

# Model 1 -----------------------------------------------------

##Model 1 includes control variables only
##Using default priors
rma.fit1<-stan_glm(HosWom~dsex+shvic.nexp+school, data = rmset, family = gaussian,
                   chains = 4, iter = 50000, seed = 112122, refresh = 0)
##Show priors
prior_summary(rma.fit1)$prior_intercept
prior_summary(rma.fit1)$prior
prior_summary(rma.fit1)$prior_aux
##Generate estimates
print(rma.fit1, digits = 4)
##Run diagnostics
##Get neff ratio
neff_ratio(rma.fit1, pars = c('dsex', 'shvic.nexp', 'school', '(Intercept)', 'sigma'))
##Get r hat
rhat(rma.fit1, pars = c('dsex', 'shvic.nexp', 'school', '(Intercept)', 'sigma'))
color_scheme_set("viridisC")
##Create traceplots
mcmc_trace(rma.fit1, pars = c('dsex', 'shvic.nexp', 'school', '(Intercept)', 'sigma'))
##Create density overlay plots
mcmc_dens_overlay(rma.fit1, pars = c('dsex', 'shvic.nexp', 'school', '(Intercept)', 'sigma'))+ ylab('Density')
##Create autocorrelation plots
mcmc_acf(rma.fit1, pars = c('dsex', 'shvic.nexp', 'school', '(Intercept)', 'sigma'), lags = 15)

##Posterior summary
rma1.sum<-summary(rma.fit1, prob = c(.025, .05, .95, .975), digits = 4)
rma1.sum[,1:9]
descriptives<-function(x) {
  c(mean = mean(x), median = median(x), map = map_estimate(x), sd = sd(x), 
    ETI95 = c(ci(x)$CI_low, ci(x)$CI_high), HDI95 = c(ci(x, method = 'HDI')$CI_low, ci(x, method = 'HDI')$CI_high))
}
apply(rma.df1<-as.data.frame(rma.fit1), 2, descriptives)

##Show posterior distribution
bayesplot_grid(
  plot(rma.fit1, 'areas', prob = .95, pars = 'dsex'),
  plot(rma.fit1, 'areas', prob = .95, pars = 'school'),
  plot(rma.fit1, 'areas', prob = .95, pars = 'shvic.nexp'),
  titles = c('Posterior Distribution for Gender', 'Posterior distribution for School', 
             'Posterior Distribution for victimizaiton \n(negative exponentially transformed)'),
  grid_args = list(ncols = 2)
)
##Get Bayes R-Squared
rma1.bayes.rsq<-bayes_R2(rma.fit1)
rma1.rsq.sum<-c(median = median(rma1.bayes.rsq), mean = mean(rma1.bayes.rsq), 
  sd = sd(rma1.bayes.rsq), quantile(rma1.bayes.rsq, prob = c(.05, .95)))
round(rma1.rsq.sum, 3)


# Model 2 -----------------------------------

##Model 2 adds friendships
##Using default priors
rma.fit2<-stan_glm(HosWom~dsex+shvic.nexp+school+teen_tot_male.sq+teen_tot_fem.sq+frint.sq, 
                   data = rmset, family = gaussian, chains = 4, iter = 50000, 
                   seed = 112122, refresh = 0)
##Show priors
prior_summary(rma.fit2)$prior_intercept
prior_summary(rma.fit2)$prior
prior_summary(rma.fit2)$prior_aux
##Generate estimates
print(rma.fit2, digits = 4)
##Run diagnostics
##Get neff ratio
neff_ratio(rma.fit2, pars = c('dsex', 'shvic.nexp', 'school', 'teen_tot_male.sq', 'teen_tot_fem.sq', 
                              'frint.sq', '(Intercept)', 'sigma'))
##Get r hat
rhat(rma.fit2, pars = c('dsex', 'shvic.nexp', 'school', 'teen_tot_male.sq', 'teen_tot_fem.sq', 
                        'frint.sq', '(Intercept)', 'sigma')) 
color_scheme_set("viridisC")
##Create traceplots
mcmc_trace(rma.fit2, pars = c('dsex', 'shvic.nexp', 'school', 'teen_tot_male.sq', 'teen_tot_fem.sq', 
                              'frint.sq', '(Intercept)', 'sigma'))
##Create density overlay plots
mcmc_dens_overlay(rma.fit2, pars = c('dsex', 'shvic.nexp', 'school', 'teen_tot_male.sq', 'teen_tot_fem.sq', 
                                     'frint.sq', '(Intercept)', 'sigma'))+ ylab('Density')
##Create autocorrelation plots
mcmc_acf(rma.fit2, pars = c('dsex', 'shvic.nexp', 'school', 'teen_tot_male.sq', 'teen_tot_fem.sq', 
                            'frint.sq', '(Intercept)', 'sigma'), lags = 15)

##Posterior summary
rma2.sum<-summary(rma.fit2, prob = c(.025, .05, .95, .975), digits = 4)
rma2.sum[,1:9]
apply(rma.df2<-as.data.frame(rma.fit2), 2, descriptives)

##Show posterior distribution
bayesplot_grid(
  plot(rma.fit2, 'areas', prob = .95, pars = 'dsex'),
  plot(rma.fit2, 'areas', prob = .95, pars = 'shvic.nexp'),
  plot(rma.fit2, 'areas', prob = .95, pars = 'school'),
  plot(rma.fit2, 'areas', prob = .95, pars = 'frint.sq'),
  plot(rma.fit2, 'areas', prob = .95, pars = 'teen_tot_male.sq'),
  plot(rma.fit2, 'areas', prob = .95, pars = 'teen_tot_fem.sq'),
  titles = c('Posterior Distribution for Gender', 
             'Posterior Distribution for victimizaiton \n(negative exponentially transformed)', 
             'Posterior distribution for School', 
             'Posterior Distribution for Friendship Intimacy \n(squared)', 
             'Posterior Distribution for Male Friendships \n(squared)', 
             'Posterior distribution for Female Friendships \n(squared)'),
  grid_args = list(ncols = 2)
)
##Get Bayes R-Squared
rma2.bayes.rsq<-bayes_R2(rma.fit2)
rma2.rsq.sum<-c(median = median(rma2.bayes.rsq), mean = mean(rma2.bayes.rsq), 
                sd = sd(rma2.bayes.rsq), quantile(rma2.bayes.rsq, prob = c(.05, .95)))
round(rma2.rsq.sum, 3)

# Model 3 -----------------------------------

##Model 3 adds empathy
##Using default priors
rma.fit3<-stan_glm(HosWom~dsex+shvic.nexp+school+frint.sq+emp.sq, 
                   data = rmset, family = gaussian, chains = 4, iter = 50000, 
                   seed = 112122, refresh = 0)
##Show priors
prior_summary(rma.fit3)$prior_intercept
prior_summary(rma.fit3)$prior
prior_summary(rma.fit3)$prior_aux
##Generate estimates
print(rma.fit3, digits = 4)
##Run diagnostics
##Get neff ratio
neff_ratio(rma.fit3, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', '(Intercept)', 'sigma'))
##Get r hat
rhat(rma.fit3, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', '(Intercept)', 'sigma')) 
color_scheme_set("viridisC")
##Create traceplots
mcmc_trace(rma.fit3, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', '(Intercept)', 'sigma'))
##Create density overlay plots
mcmc_dens_overlay(rma.fit3, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', '(Intercept)', 'sigma')) + 
  ylab('Density')
##Create autocorrelation plots
mcmc_acf(rma.fit3, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', '(Intercept)', 'sigma'), lags = 15)

##Posterior summary
rma3.sum<-summary(rma.fit3, prob = c(.025, .05, .95, .975), digits = 4)
rma3.sum[,1:9]
apply(rma.df3<-as.data.frame(rma.fit3), 2, descriptives)

##Show posterior distribution
bayesplot_grid(
  plot(rma.fit3, 'areas', prob = .95, pars = 'dsex'),
  plot(rma.fit3, 'areas', prob = .95, pars = 'shvic.nexp'),
  plot(rma.fit3, 'areas', prob = .95, pars = 'school'),
  plot(rma.fit3, 'areas', prob = .95, pars = 'frint.sq'),
  plot(rma.fit3, 'areas', prob = .95, pars = 'emp.sq'),
  titles = c('Posterior Distribution for Gender', 
             'Posterior Distribution for victimizaiton \n(negative exponentially transformed)', 
             'Posterior distribution for School', 
             'Posterior Distribution for Friendship Intimacy \n(squared)', 'Posterior Distribution for Empathy'),
  grid_args = list(ncols = 2)
)
##Get Bayes R-Squared
rma3.bayes.rsq<-bayes_R2(rma.fit3)
rma3.rsq.sum<-c(median = median(rma3.bayes.rsq), mean = mean(rma3.bayes.rsq), 
                sd = sd(rma3.bayes.rsq), quantile(rma3.bayes.rsq, prob = c(.05, .95)))
round(rma3.rsq.sum, 3)

# Model 4 -----------------------------------

##Model 4 adds interaction between friendship intimacy and empathy
##Using default priors
rma.fit4<-stan_glm(HosWom~dsex+shvic.nexp+school+emp.sq*frint.sq, 
                   data = rmset, family = gaussian, chains = 4, iter = 50000, 
                   seed = 112122, refresh = 0)
##Show priors
prior_summary(rma.fit4)$prior_intercept
prior_summary(rma.fit4)$prior
prior_summary(rma.fit4)$prior_aux
##Generate estimates
print(rma.fit4, digits = 4)
##Run diagnostics
##Get neff ratio
neff_ratio(rma.fit4, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', 'emp.sq:frint.sq', '(Intercept)', 'sigma'))
##Get r hat
rhat(rma.fit4, pars = c('dsex', 'school', 'shvic.nexp', 'frint.sq', 'emp.sq', 'emp.sq:frint.sq', '(Intercept)', 'sigma')) 
color_scheme_set("viridisC")
##Create traceplots
mcmc_trace(rma.fit4, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', 'emp.sq:frint.sq', '(Intercept)', 'sigma'))
##Create density overlay plots
mcmc_dens_overlay(rma.fit4, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', 'emp.sq:frint.sq', '(Intercept)', 'sigma')) + 
  ylab('Density')
##Create autocorrelation plots
mcmc_acf(rma.fit4, pars = c('dsex', 'shvic.nexp', 'school', 'frint.sq', 'emp.sq', 'emp.sq:frint.sq', '(Intercept)', 'sigma'), lags = 15)

##Posterior summary
rma4.sum<-summary(rma.fit4, prob = c(.025, .05, .95, .975), digits = 4)
rma4.sum[,1:9]
apply(rma.df4<-as.data.frame(rma.fit4), 2, descriptives)

##Show posterior distribution
bayesplot_grid(
  plot(rma.fit4, 'areas', prob = .95, pars = 'dsex'),
  plot(rma.fit4, 'areas', prob = .95, pars = 'shvic.nexp'),
  plot(rma.fit4, 'areas', prob = .95, pars = 'school'),
  plot(rma.fit4, 'areas', prob = .95, pars = 'frint.sq'),
  plot(rma.fit4, 'areas', prob = .95, pars = 'emp.sq'),
  plot(rma.fit4, 'areas', prob = .95, pars = 'emp.sq:frint.sq'),
  titles = c('Posterior Distribution for Gender', 
             'Posterior Distribution for victimizaiton \n(negative exponentially transformed)', 
             'Posterior distribution for School', 
             'Posterior Distribution for Friendship Intimacy \n(squared)', 'Posterior Distribution for Empathy', 
             'Posterior distributon for the Interaction \nbetween Friendship Intimacy and Empathy'),
  grid_args = list(ncols = 2)
)
##Get Bayes R-Squared
rma4.bayes.rsq<-bayes_R2(rma.fit4)
rma4.rsq.sum<-c(median = median(rma4.bayes.rsq), mean = mean(rma4.bayes.rsq), 
                sd = sd(rma4.bayes.rsq), quantile(rma4.bayes.rsq, prob = c(.05, .95)))
round(rma4.rsq.sum, 4)

# Compare the models --------------------------------
##Compare results
list(m1 = rma.fit1$coefficients, m2 = rma.fit2$coefficients, m3 = rma.fit3$coefficients, m4 = rma.fit4$coefficients)
##Combine rsq
rbind(m1 = round(rma1.rsq.sum, 4), m2 = round(rma2.rsq.sum, 4), m3 = round(rma3.rsq.sum, 4), m4 = round(rma4.rsq.sum, 4))


##Show posterior predictive check for each model
bayesplot_grid(
  pp_check(rma.fit1, seed = 112222),
  pp_check(rma.fit2, seed = 112222),
  pp_check(rma.fit3, seed = 112222),
  pp_check(rma.fit4, seed = 112222),
  titles = c('m1', 'm2', 'm3', 'm4')
)
##Posterior prediction accuracy
set.seed(112222)
pred.m3<-posterior_predict(rma.fit3, newdata = rmset)
pred.m4<-posterior_predict(rma.fit4, newdata = rmset)
ppc_intervals(rmset$HosWom, yrep = pred.m3, x = rmset$frint.sq, prob = .5, prob_outer = .95) + 
  labs(x = 'Squared friendship intimacy', y = 'm3: RMA')
ppc_intervals(rmset$HosWom, yrep = pred.m4, x = rmset$frint.sq, prob = .5, prob_outer = .95) + 
  labs(x = 'Squared friendship intimacy', y = 'm4: RMA')
ppc_intervals(rmset$HosWom, yrep = pred.m3, x = rmset$emp.sq, prob = .5, prob_outer = .95) + 
  labs(x = 'Squared empathy', y = 'm3: RMA')
ppc_intervals(rmset$HosWom, yrep = pred.m4, x = rmset$emp.sq, prob = .5, prob_outer = .95) + 
  labs(x = 'Squared empathy', y = 'm4: RMA')

##Create quick median split
rmset$emphilo<-ifelse(rmset$emp.sq > median(rmset$emp.sq), 1, 0)
ppc_intervals_grouped(rmset$HosWom, yrep = pred.m4, x = rmset$frint.sq, 
                      group = rmset$emphilo, prob = .5, prob_outer = .95,
                      facet_args = list(scales = "fixed"))+
  labs(x = 'Squared friendship intimacy', y = 'm4: RMA')

##Try with interactions package too
interact_plot(rma.fit4, modx = emp.sq, pred = frint.sq, main.title = "Model 4: The Association between friendship intimacy and RMA by levels of empathy", x.label = "Friendship Intimacy (squared)", y.label = "RMA")

##Run cross validation check
m1.cv<-prediction_summary_cv(model = rma.fit1, data = rmset, k = 10)
m2.cv<-prediction_summary_cv(model = rma.fit2, data = rmset, k = 10)
m3.cv<-prediction_summary_cv(model = rma.fit3, data = rmset, k = 10)
m4.cv<-prediction_summary_cv(model = rma.fit4, data = rmset, k = 10)
rbind(m1 = m1.cv$cv, m2 = m2.cv$cv, m3 = m3.cv$cv, m4 = m4.cv$cv)

##ELPD comparison
set.seed(112222)
loo1<-loo(rma.fit1)
loo2<-loo(rma.fit2)
loo3<-loo(rma.fit3)
loo4<-loo(rma.fit4)
##Compare ELPD estimate
c(m1.elpd<-loo1$estimates[1], m2.elpd<-loo2$estimates[1], m3.elpd<-loo3$estimates[1], m4.elpd<-loo4$estimates[1])
##Model comparison
loo_compare(loo1, loo2, loo3, loo4)

#Examining results for selected model----------------

##Create function fo probability of direction
prob.dir<-function(x){
  ifelse(sign(median(x)>0),
         sum(x>0)/length(x),
         sum(x<0)/length(x))
}
##Display probability of direction along the median
rbind(median = apply(as.data.frame(rma.fit3), 2, median), 
      HDI.025 = apply(rma.df3<-as.data.frame(rma.fit3), 2, descriptives)[7,], 
      HDI.975 = apply(rma.df3<-as.data.frame(rma.fit3), 2, descriptives)[8,],
      pd = apply(as.data.frame(rma.fit3), 2, prob.dir))
##Examine ROPE
rope(rma.fit3, parameters = c('frint.sq', 'emp'), range = c(-.01, .01), ci = c(.5, .9, .95))
rope(rma.fit3, parameters = c('frint.sq', 'emp'), range = c(-.1, .1), ci = c(.5, .9, .95))
rope(rma.fit3, parameters = c('frint.sq', 'emp'), range = c(-.05, .05), ci = c(.5, .9, .95))

##Get bayes factor
##rma.fit3.prior<-update(rma.fit3, prior_PD = TRUE)
##bayesfactor(rma.fit3, prior = rma.fit3.prior)

##Null=0
bayesfactor_parameters(rma.fit3, null = 0)
##Negative direction of effect
bayesfactor_parameters(rma.fit3, null = c(0, Inf), parameters = c('frint.sq', 'emp'))
##Get sample of slopes to graph
set.seed(112222)
row.id<-sample(1:nrow(rma.df3), 40)
slope.sample<-data.frame(row.id, rma.df3[row.id,])
c.title<-theme(plot.title = element_text(hjust = 0.5))
##Graph association between Friendship intimacy and RMA
ggplot(rmset, aes(x=frint.sq, y=HosWom))+ geom_point()+
  geom_abline(slope = median(rma.df3$frint.sq), intercept = median(rma.df3$`(Intercept)`), col = 'blue', size = 2)+
  geom_abline(data = slope.sample, aes(intercept = X.Intercept., slope = frint.sq, group = row.id), alpha = .5)+
  theme_classic()+
  ylab('RMA')+
  xlab('Friendship Intimacy (squared)')+
  ggtitle('Predicted RMA Based on Friendship Intimacy')+
  c.title
##Graph association between empathy and RMA
ggplot(rmset, aes(x=emp.sq, y=HosWom))+ geom_point()+
  geom_abline(slope = median(rma.df3$emp.sq), intercept = median(rma.df3$`(Intercept)`), col = 'blue', size = 2)+
  geom_abline(data = slope.sample, aes(intercept = X.Intercept., slope = emp.sq, group = row.id), alpha = .5)+
  theme_classic()+
  ylab('RMA')+
  ggtitle('Predicted RMA Based on Levels of Empathy')+
  xlab('Empathy (squared)')+
  c.title