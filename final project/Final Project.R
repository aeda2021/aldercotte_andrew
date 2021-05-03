## This script combines the many previous bits of code that I had for organizing and analysing the watermelon visitation data into one
## clean file for submission with the AEDA final project paper. 

## Some of these packages might not actually be necessary for the analyses that remain in this script:
library(Rcpp)
library(dplyr)
library(lme4)
library(ggplot2)
library(lubridate)
library(fitdistrplus)
library(glmmTMB)
library(DHARMa)
library(broom)

### ---- SECTION 1: THIS IS JUST DATA CLEANING, FEEL FREE TO JUMP TO SECTION 2! (Especially because it is really ugly because I had no idea what I was doing when I started this) ---- ###

## The first step is to load the original data in:
setwd("C:/AHA Documents/Academic/Rutgers/AEDA/Final Project")

df <- read.csv("C:/AHA Documents/Academic/Rutgers/AEDA/Final Project/watermelon_obs.csv")

## Removing 2004 data as it was too sparse to be included in the analyses:
df <- subset(df, study != 'njpa2004')

## This cleans up the farmcode names and keeps just the ones to be included in the analyses:

df$farmcode <- gsub("[[:space:]]", "", df$farmcode)
remove <- c('bar','ker','kim','mapb','mapc','wil')
df <- subset(df,!(farmcode %in% remove))

## I'm going to replace the NA's in round (from years where there was only 1 round) with 1's, so that round (i.e. site-day) ID's are more
## consistent:

df$round <- as.numeric(as.character(df$round))
df$round[is.na(df$round)] <- 1

## Then I want to replace study with a year column, and get a total flowers observed column, and a unique round ID column. I need to keep farm, year,
## flowers observed, round ID and delete the rest of the columns.

df$year <- substr(df$study, 5,8)

df2 <- data.frame(df$farmcode, df$round, df$period, df$year, df$female_flowers_observed, df$male_flowers_observed, df$total_flowers)

df2 <- df2 %>% 
  rename(
    farm = df.farmcode,
    round = df.round,
    period = df.period,
    year = df.year,
    female_flowers = df.female_flowers_observed,
    male_flowers = df.male_flowers_observed,
    total_flowers = df.total_flowers
  )

df2$roundID <- paste(df2$year, df2$farm, df2$round)

## Fixing the total flowers count (it's still all NA's for 2012)

df2$female_flowers <- as.numeric(as.character(df2$female_flowers))
df2$male_flowers <- as.numeric(as.character(df2$male_flowers))

df2$total_flowers <- c(df2$male_flowers + df2$female_flowers)

df2$total_flowers[is.na(df2$total_flowers)] <- 0

## Because of the way the data is structured, I need to extract a summary of the number of flowers observed per round now, before I start
## separating bees out by group.

round_flowers <- df2 %>% group_by(roundID) %>% summarize(flowers = sum(total_flowers))

## Now I can start creating new dataframes for each bee group/sub-group that will be used directly in the models. I start with Apis mellifera
## only:

apis_only <- df

apis_only$roundID <- paste(apis_only$year, apis_only$farmcode, apis_only$round)

apis_only <- subset(apis_only, pollIDcode == 'APIS')

apis_only$visits_to_female_flowers <- as.numeric(as.character(apis_only$visits_to_female_flowers))
apis_only$visits_to_male_flowers <- as.numeric(as.character(apis_only$visits_to_male_flowers))
apis_only$visits_flower_sex_unknown <- as.numeric(as.character(apis_only$visits_flower_sex_unknown))

apis_only$total_visits <- rowSums(cbind(apis_only$visits_to_female_flowers, apis_only$visits_to_male_flowers, apis_only$visits_flower_sex_unknown), na.rm=TRUE)

## There are about half a dozen records of APIS with no # of visits to any flower type associated with them, so I'm going to assume that they
## each represented 1 flower visit. 

apis_only$total_visits[apis_only$total_visits == 0] <- 1

apis_visits <- apis_only %>% group_by(roundID) %>% summarize(visits = sum(total_visits))

apis <- merge(round_flowers, apis_visits, by = "roundID", all.x = TRUE)

apis$visits[is.na(apis$visits)] <- 0

apis$year <- substr(apis$roundID, 1,4)
apis$farm <- substr(apis$roundID, 6,9)
apis$round <- substr(apis$roundID, 10,12)

apis$farm <- gsub("[[:space:]]", "", apis$farm)

table(apis$farm)

apis$visit_rate <- apis$visits / apis$flowers

## Then Bombus

table(df$pollIDcode)

bombus_only <- df

bombus_only$roundID <- paste(bombus_only$year, bombus_only$farmcode, bombus_only$round)

bombus_only$pollIDcode <- gsub("[[:space:]]", "", bombus_only$pollIDcode)

bombus_only <- subset(bombus_only, pollIDcode %in% c('BIMAC', 'BIMAC-W', 'BOM', 'BOM-W', 'FERV-W', 'GRIS-W', "CIT", 'IMP', 'IMP-Q', 'IMP-W'))

table(bombus_only$pollIDcode)

bombus_only$visits_to_female_flowers <- as.numeric(as.character(bombus_only$visits_to_female_flowers))
bombus_only$visits_to_male_flowers <- as.numeric(as.character(bombus_only$visits_to_male_flowers))
bombus_only$visits_flower_sex_unknown <- as.numeric(as.character(bombus_only$visits_flower_sex_unknown))

bombus_only$total_visits <- rowSums(cbind(bombus_only$visits_to_female_flowers, bombus_only$visits_to_male_flowers, bombus_only$visits_flower_sex_unknown), na.rm=TRUE)

bombus_visits <- bombus_only %>% group_by(roundID) %>% summarize(visits = sum(total_visits))

bombus <- merge(round_flowers, bombus_visits, by = "roundID", all.x = TRUE)

bombus$visits[is.na(bombus$visits)] <- 0

bombus$year <- substr(bombus$roundID, 1,4)
bombus$farm <- substr(bombus$roundID, 6,9)
bombus$round <- substr(bombus$roundID, 10,12)

bombus$farm <- gsub("[[:space:]]", "", bombus$farm)

table(bombus$farm)

bombus$visit_rate <- bombus$visits / bombus$flowers

## Now all other bees: (Note, I originally included other non-bee pollinators, this code now excludes them)

other_only <- df

other_only$roundID <- paste(other_only$year, other_only$farmcode, other_only$round)

other_only$pollIDcode <- gsub("[[:space:]]", "", other_only$pollIDcode)

other_only <- subset(other_only, !(pollIDcode %in% c('BIMAC', 'BIMAC-W', 'BOM', 'BOM-W', 'FERV-W', 'GRIS-W', "CIT", 'IMP', 'IMP-Q', 'IMP-W', 'APIS', 'BUTTERFLY', 'CAB', 'FLY',
                                                     'moth', 'SKIPPER', 'SYR', 'WASP')))

other_only$visits_to_female_flowers <- as.numeric(as.character(other_only$visits_to_female_flowers))
other_only$visits_to_male_flowers <- as.numeric(as.character(other_only$visits_to_male_flowers))
other_only$visits_flower_sex_unknown <- as.numeric(as.character(other_only$visits_flower_sex_unknown))

other_only$total_visits <- rowSums(cbind(other_only$visits_to_female_flowers, other_only$visits_to_male_flowers, other_only$visits_flower_sex_unknown), na.rm=TRUE)

other_visits <- other_only %>% group_by(roundID) %>% summarize(visits = sum(total_visits))

other <- merge(round_flowers, other_visits, by = "roundID", all.x = TRUE)

other$visits[is.na(other$visits)] <- 0

other$year <- substr(other$roundID, 1,4)
other$farm <- substr(other$roundID, 6,9)
other$round <- substr(other$roundID, 10,12)

other$farm <- gsub("[[:space:]]", "", other$farm)

table(other$farm)

other$visit_rate <- other$visits / other$flowers

## .... and now all bees (also excluding other pollinators this time around):

bees_only <- df

bees_only$roundID <- paste(bees_only$year, bees_only$farmcode, bees_only$round)

bees_only$pollIDcode <- gsub("[[:space:]]", "", bees_only$pollIDcode)

bees_only <- subset(bees_only, !(pollIDcode %in% c('BUTTERFLY', 'CAB', 'FLY', 'moth', 'SKIPPER', 'SYR', 'WASP')))

bees_only$visits_to_female_flowers <- as.numeric(as.character(bees_only$visits_to_female_flowers))
bees_only$visits_to_male_flowers <- as.numeric(as.character(bees_only$visits_to_male_flowers))
bees_only$visits_flower_sex_unknown <- as.numeric(as.character(bees_only$visits_flower_sex_unknown))

bees_only$total_visits <- rowSums(cbind(bees_only$visits_to_female_flowers, bees_only$visits_to_male_flowers, bees_only$visits_flower_sex_unknown), na.rm=TRUE)

bees_visits <- bees_only %>% group_by(roundID) %>% summarize(visits = sum(total_visits))

bees <- merge(round_flowers, bees_visits, by = "roundID", all.x = TRUE)

bees$visits[is.na(bees$visits)] <- 0

bees$year <- substr(bees$roundID, 1,4)
bees$farm <- substr(bees$roundID, 6,9)
bees$round <- substr(bees$roundID, 10,12)

bees$farm <- gsub("[[:space:]]", "", bees$farm)

table(bees$farm)

bees$visit_rate <- bees$visits / bees$flowers

## Forgot to do all wild bees! Doing all wild bees:

wild_only <- df

wild_only$roundID <- paste(wild_only$year, wild_only$farmcode, wild_only$round)

wild_only$pollIDcode <- gsub("[[:space:]]", "", wild_only$pollIDcode)

wild_only <- subset(wild_only, !(pollIDcode %in% c('APIS','BUTTERFLY', 'CAB', 'FLY', 'moth', 'SKIPPER', 'SYR', 'WASP')))

table(wild_only$pollIDcode)

wild_only$visits_to_female_flowers <- as.numeric(as.character(wild_only$visits_to_female_flowers))
wild_only$visits_to_male_flowers <- as.numeric(as.character(wild_only$visits_to_male_flowers))
wild_only$visits_flower_sex_unknown <- as.numeric(as.character(wild_only$visits_flower_sex_unknown))

wild_only$total_visits <- rowSums(cbind(wild_only$visits_to_female_flowers, wild_only$visits_to_male_flowers, wild_only$visits_flower_sex_unknown), na.rm=TRUE)

wild_visits <- wild_only %>% group_by(roundID) %>% summarize(visits = sum(total_visits))

wild <- merge(round_flowers, wild_visits, by = "roundID", all.x = TRUE)

wild$visits[is.na(wild$visits)] <- 0

wild$year <- substr(wild$roundID, 1,4)
wild$farm <- substr(wild$roundID, 6,9)
wild$round <- substr(wild$roundID, 10,12)

wild$farm <- gsub("[[:space:]]", "", wild$farm)

table(wild$farm)

wild$visit_rate <- wild$visits / wild$flowers

## Adding a scaled year variable to each df:

apis$year <- as.numeric(apis$year)
bombus$year <- as.numeric(bombus$year)
other$year <- as.numeric(other$year)
bees$year <- as.numeric(bees$year)
wild$year <- as.numeric(wild$year)

apis <- apis %>% mutate(year.scaled = year - 2005)
bombus <- bombus %>% mutate(year.scaled = year - 2005)
other <- other %>% mutate(year.scaled = year - 2005)
bees <- bees %>% mutate(year.scaled = year - 2005)
wild <- wild %>% mutate(year.scaled = year - 2005)

## OK, so now I have all the dataframes I need for analyses. I'm going to delete objects in the environment that won't be needed again:

rm(apis_only, apis_visits, bees_only, bees_visits, bombus_only, bombus_visits, df2, other_only, other_visits, watermelon_obs, remove, wild_only, wild_visits)


### --- SECTION 2: MODEL SELECTION --- ###

## I initially use fitdistrplus to compare the raw data distribution with several options in terms of error distribution:

plot(fitdistrplus::fitdist(bees$visits, dist = 'nbinom'))
nbfit1 <- fitdistrplus::fitdist(bees$visits, dist = 'nbinom')
cdfcomp(nbfit1)

plot(fitdistrplus::fitdist(bees$visit_rate, dist = 'gamma'))
gammafit1 <- fitdistrplus::fitdist(bees$visit_rate, dist = 'gamma')
cdfcomp(gammafit1)

## Those both look good for the all bees data set. 

## I've only included one example here, but the models I attempted to fit on visit rate (rather than visit count) were of the form below.
## Running this model will generate an error, due to the 0's in the wild bee data set:

gamma_model <- glmer(visit_rate ~ year.scaled + (year.scaled|farm), family=Gamma(link="log"), data = wild)

## I will compare the different negative binomial family distributions here, using glmmTMB because it allows for a quasi-poisson distribution
## in mixed effect models, of a sort, which lme4 does not.

pois_test <- glmmTMB(visits ~ year.scaled + (year.scaled|farm), family=poisson(link="log"), data = bees)
qpois_test <- glmmTMB(visits ~ year.scaled + (year.scaled|farm), family=nbinom1(link="log"), data = bees)
nb_test <- glmmTMB(visits ~ year.scaled + (year.scaled|farm), family=nbinom2(link='log'), data = bees)

pois_test
qpois_test
nb_test

AIC(pois_test, qpois_test, nb_test)

## That didn't work, because glmmTMB didn't produce AIC's for the negative binomial and quasi-poisson models!

res_pois <- simulateResiduals(fittedModel = pois_test, plot = F)
res_qpois <- simulateResiduals(fittedModel = qpois_test, plot = F)
res_nb <- simulateResiduals(fittedModel = nb_test, plot = F)

plot(res_pois)
plot(res_qpois)
plot(res_nb)

## However, I'm now going to switch back to lme4's glmer.nb and glmer to compare AIC between Poisson and negative binomial.

pois_test2 <- glmer(visits ~ year.scaled + (year.scaled|farm), family=poisson(link="log"), data = bees)
nb_test2 <- glmer.nb(visits ~ year.scaled + (year.scaled|farm), data = bees)

AIC(pois_test2, nb_test2)

## The AIC of the negative binomial model is so much lower...

## Now I test the random effects structure for all the bee groups

bees_nb <- glm.nb(visits ~ year.scaled + offset(log(flowers)), data = bees)
bees_rint <- glmer.nb(visits ~ year.scaled + (1|farm) + offset(log(flowers)), data = bees)
bees_rslope <- glmer.nb(visits ~ year.scaled + (year.scaled|farm) + offset(log(flowers)), data = bees)

anova(bees_rslope, bees_rint, bees_nb)

apis_nb <- glm.nb(visits ~ year.scaled + offset(log(flowers)), data = apis)
apis_rint <- glmer.nb(visits ~ year.scaled + (1|farm) + offset(log(flowers)), data = apis)
apis_rslope <- glmer.nb(visits ~ year.scaled + (year.scaled|farm) + offset(log(flowers)), data = apis)

anova(apis_rslope, apis_rint, apis_nb)

bombus_nb <- glm.nb(visits ~ year.scaled + offset(log(flowers)), data = bombus)
bombus_rint <- glmer.nb(visits ~ year.scaled + (1|farm) + offset(log(flowers)), data = bombus)
bombus_rslope <- glmer.nb(visits ~ year.scaled + (year.scaled|farm) + offset(log(flowers)), data = bombus)

anova(bombus_rslope, bombus_rint, bombus_nb)

wild_nb <- glm.nb(visits ~ year.scaled + offset(log(flowers)), data = wild)
wild_rint <- glmer.nb(visits ~ year.scaled + (1|farm) + offset(log(flowers)), data = wild)
wild_rslope <- glmer.nb(visits ~ year.scaled + (year.scaled|farm) + offset(log(flowers)), data = wild)

anova(wild_rslope, wild_rint, wild_nb)


other_nb <- glm.nb(visits ~ year.scaled + offset(log(flowers)), data = other)
other_rint <- glmer.nb(visits ~ year.scaled + (1|farm) + offset(log(flowers)), data = other)
other_rslope <- glmer.nb(visits ~ year.scaled + (year.scaled|farm) + offset(log(flowers)), data = other)

anova(other_rslope, other_rint, other_nb)

## Adjusting the smallest p-value found for the random slope element for testing on the boundary:

0.5*((1 - pchisq(1.9579, 1)) + (1 - pchisq(1.9579, 2)))

## That is still far from significant, so onwards with the random slope model. 

## Now I get to pull in my best attempts at drawing the figures:

bees.fit <- data.frame(x=0:7)
modmat <- model.matrix(~x,bees.fit)
y <-modmat%*%fixef(bees_rint)
pvar1 <- diag(modmat %*% tcrossprod(vcov(bees_rint),modmat))
tvar1 <- pvar1+VarCorr(bees_rint)$farm[1]
bees.fit <- data.frame(
  x=bees.fit$x,
  y=exp(y),
  plo = exp(y-1.96*sqrt(pvar1)),
  phi = exp(y+1.96*sqrt(pvar1)),
  tlo = exp(y-1.96*sqrt(tvar1)),
  thi = exp(y+1.96*sqrt(tvar1))
)

## Now with bootMer for bootstrapped CI's.
predFun<-function(.) exp(modmat%*%fixef(.)) 
bb <-bootMer(bees_rint,FUN=predFun,nsim=200)
bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)])
bees.fit$blo<-bb_se[1,]
bees.fit$bhi<-bb_se[2,]

## Base R plots: (remember, replace taxa with appropriate df)
plot(visit_rate~year.scaled, bees)
lines(bees.fit$x,bees.fit$y,col="red",lty=2,lwd=3)
lines(bees.fit$x,bees.fit$plo,col="blue",lty=2,lwd=2)
lines(bees.fit$x,bees.fit$phi,col="blue",lty=2,lwd=2)
lines(bees.fit$x,bees.fit$tlo,col="orange",lty=2,lwd=2)
lines(bees.fit$x,bees.fit$thi,col="orange",lty=2,lwd=2)
lines(bees.fit$x,bees.fit$bhi,col="darkgreen",lty=2,lwd=2)
lines(bees.fit$x,bees.fit$blo,col="darkgreen",lty=2,lwd=2)
legend("topleft",legend=c("Fitted line","Confidence interval","Prediction interval","Bootstrapped CI"),col=c("red","blue","orange","darkgreen"),lty=2,lwd=2,bty="n")


## Better ggplot with ribbon for bootstrapped CI only
ggplot(bees, aes(x=year.scaled, y=visit_rate))+
  geom_point()+
  geom_line(data=bees.fit, aes(x=x, y=y, color='red'), size=2)+
  geom_ribbon(data=bees.fit, aes(x=x, y=y, ymin=blo, ymax=bhi), fill='blue', alpha=0.2)+
  xlab("Year")+
  ylab("Visits per observed flower")+
  theme_bw(base_size = 20)+
  theme(legend.position = 'none')+
  ggtitle("All Bees")+
  scale_x_continuous(breaks=c(0,2,4,6), labels=c("2005","2007","2009","2011"))

## For the remaining groups I will only plot the ggplot with what seems to me to be the best confidence interval to include, the
## bootstrapped prediction intervals for the means.

## Apis
apis.fit <- data.frame(x=0:7)
modmat <- model.matrix(~x,apis.fit)
y <-modmat%*%fixef(apis_rint)
pvar1 <- diag(modmat %*% tcrossprod(vcov(apis_rint),modmat))
tvar1 <- pvar1+VarCorr(apis_rint)$farm[1]
apis.fit <- data.frame(
  x=apis.fit$x,
  y=exp(y),
  plo = exp(y-1.96*sqrt(pvar1)),
  phi = exp(y+1.96*sqrt(pvar1)),
  tlo = exp(y-1.96*sqrt(tvar1)),
  thi = exp(y+1.96*sqrt(tvar1))
)

## Now with bootMer for bootstrapped CI's.
predFun<-function(.) exp(modmat%*%fixef(.)) 
bb <-bootMer(apis_rint,FUN=predFun,nsim=200)
bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)])
apis.fit$blo<-bb_se[1,]
apis.fit$bhi<-bb_se[2,]

## PLOT
ggplot(apis, aes(x=year.scaled, y=visit_rate))+
  geom_point()+
  geom_line(data=apis.fit, aes(x=x, y=y, color='red'), size=2)+
  geom_ribbon(data=apis.fit, aes(x=x, y=y, ymin=blo, ymax=bhi), fill='blue', alpha=0.2)+
  xlab("Year")+
  ylab("Visits per observed flower")+
  theme_bw(base_size = 20)+
  theme(legend.position = 'none')+
  ggtitle("Honeybees")+
  scale_x_continuous(breaks=c(0,2,4,6), labels=c("2005","2007","2009","2011"))

## Bombus
bombus.fit <- data.frame(x=0:7)
modmat <- model.matrix(~x,bombus.fit)
y <-modmat%*%fixef(bombus_rint)
pvar1 <- diag(modmat %*% tcrossprod(vcov(bombus_rint),modmat))
tvar1 <- pvar1+VarCorr(bombus_rint)$farm[1]
bombus.fit <- data.frame(
  x=bombus.fit$x,
  y=exp(y),
  plo = exp(y-1.96*sqrt(pvar1)),
  phi = exp(y+1.96*sqrt(pvar1)),
  tlo = exp(y-1.96*sqrt(tvar1)),
  thi = exp(y+1.96*sqrt(tvar1))
)

## Now with bootMer for bootstrapped CI's.
predFun<-function(.) exp(modmat%*%fixef(.)) 
bb <-bootMer(bombus_rint,FUN=predFun,nsim=200)
bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)])
bombus.fit$blo<-bb_se[1,]
bombus.fit$bhi<-bb_se[2,]

## PLOT
ggplot(bombus, aes(x=year.scaled, y=visit_rate))+
  geom_point()+
  geom_line(data=bombus.fit, aes(x=x, y=y, color='red'), size=2)+
  geom_ribbon(data=bombus.fit, aes(x=x, y=y, ymin=blo, ymax=bhi), fill='blue', alpha=0.2)+
  xlab("Year")+
  ylab("Visits per observed flower")+
  theme_bw(base_size = 20)+
  theme(legend.position = 'none')+
  ggtitle("Bumblebees")+
  scale_x_continuous(breaks=c(0,2,4,6), labels=c("2005","2007","2009","2011"))

## Wild
wild.fit <- data.frame(x=0:7)
modmat <- model.matrix(~x,wild.fit)
y <-modmat%*%fixef(wild_rint)
pvar1 <- diag(modmat %*% tcrossprod(vcov(wild_rint),modmat))
tvar1 <- pvar1+VarCorr(wild_rint)$farm[1]
wild.fit <- data.frame(
  x=wild.fit$x,
  y=exp(y),
  plo = exp(y-1.96*sqrt(pvar1)),
  phi = exp(y+1.96*sqrt(pvar1)),
  tlo = exp(y-1.96*sqrt(tvar1)),
  thi = exp(y+1.96*sqrt(tvar1))
)

## Now with bootMer for bootstrapped CI's.
predFun<-function(.) exp(modmat%*%fixef(.)) 
bb <-bootMer(wild_rint,FUN=predFun,nsim=200)
bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)])
wild.fit$blo<-bb_se[1,]
wild.fit$bhi<-bb_se[2,]

## PLOT
ggplot(wild, aes(x=year.scaled, y=visit_rate))+
  geom_point()+
  geom_line(data=wild.fit, aes(x=x, y=y, color='red'), size=2)+
  geom_ribbon(data=wild.fit, aes(x=x, y=y, ymin=blo, ymax=bhi), fill='blue', alpha=0.2)+
  xlab("Year")+
  ylab("Visits per observed flower")+
  theme_bw(base_size = 20)+
  theme(legend.position = 'none')+
  ggtitle("All Wild Bees")+
  scale_x_continuous(breaks=c(0,2,4,6), labels=c("2005","2007","2009","2011"))

## Other
other.fit <- data.frame(x=0:7)
modmat <- model.matrix(~x,other.fit)
y <-modmat%*%fixef(other_rint)
pvar1 <- diag(modmat %*% tcrossprod(vcov(other_rint),modmat))
tvar1 <- pvar1+VarCorr(other_rint)$farm[1]
other.fit <- data.frame(
  x=other.fit$x,
  y=exp(y),
  plo = exp(y-1.96*sqrt(pvar1)),
  phi = exp(y+1.96*sqrt(pvar1)),
  tlo = exp(y-1.96*sqrt(tvar1)),
  thi = exp(y+1.96*sqrt(tvar1))
)

## Now with bootMer for bootstrapped CI's.
predFun<-function(.) exp(modmat%*%fixef(.)) 
bb <-bootMer(other_rint,FUN=predFun,nsim=200)
bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)])
other.fit$blo<-bb_se[1,]
other.fit$bhi<-bb_se[2,]

## PLOT
ggplot(other, aes(x=year.scaled, y=visit_rate))+
  geom_point()+
  geom_line(data=other.fit, aes(x=x, y=y, color='red'), size=2)+
  geom_ribbon(data=other.fit, aes(x=x, y=y, ymin=blo, ymax=bhi), fill='blue', alpha=0.2)+
  xlab("Year")+
  ylab("Visits per observed flower")+
  theme_bw(base_size = 20)+
  theme(legend.position = 'none')+
  ggtitle("Other Bees")+
  scale_x_continuous(breaks=c(0,2,4,6), labels=c("2005","2007","2009","2011"))

summary(bees_rint)
summary(wild_rint)
summary(apis_rint)
summary(bombus_rint)
summary(other_rint)


## One of the questions that arises from trying to measure declines over 7 years is: 'Would we see a decline this big due to chance variation?"
## There is a lot of variability in the annual mean visitation rates, so I wanted to calculate those here for use in the paper discussion:

bees_means <- bees %>% group_by(year) %>% summarize(means = mean(visit_rate))
wild_means <- wild %>% group_by(year) %>% summarize(means = mean(visit_rate))
wild_means


### --- SECTION 3: PERMUTATION ANALYSIS --- ###

## That's all for the GLMM stuff, I've included my permutation analysis code below but it is all hashed so that it doesn't start a 30+ 
## hour job on the unsuspecting.

## This generates the 40320 possible ways to order 8 years...
#perms <- permn(0:7)

#play <- wild

#outcomes <- data.frame(matrix(ncol = 2, nrow=40320))
#outrbind <- data.frame(matrix(ncol= 2, nrow=0))

#for (j in 12693:length(perms[[]])){
#  play$new.year <- match(play$year.scaled, perms[[j]])-1
#  nbmodel <- glmer.nb(visits ~ new.year + (1|farm) + offset(log(flowers)), data=play)
#  beta <- fixef(nbmodel)
#  ## outrbind <- rbind(outrbind, beta)
#  outcomes[j, 1] <- beta[1]
#  outcomes[j, 2] <- beta[2]
#  print(j)
#}

#play$new.year <- match(play$year.scaled, perms[[10]])-1

#coef <- fixef(wild_nb)
#coef[2]

#perms[[1]]

#outcomes$V2 <- as.numeric(outcomes$V2)

## Now, can I do the math to find out what proportion of the data lies to the right of my value?


#p <- sum(outcomes$V2 < -0.10198)/length(outcomes$V2)
#p


#ggplot(outcomes, aes(V2))+
#  geom_histogram(bins=50, fill='white', color='black')+
#  xlab("Effect of Year on Visit Rate")+
#  ylab("Frequency")+
#  geom_vline(xintercept = -0.10198, color="red", size=3)+
#  geom_text(x=-.16, y=1400, label="Observed Effect Size", size=6)+
#  theme_classic() + theme(text = element_text(size=20)