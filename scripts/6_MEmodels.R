# Class 6: Mixed Effects Models
# Turn this exercise sheet in by 
# 1) Going through the code below, filling in any sections that are missing
# 2) If the code asks you to make a figure, be sure to save it by name from this script into your figures/ directory. Name must start with 6_
# 3) Answering any questions (labeled Q#) by typing comments in this text (start line with #)
# 4) Committing this completed script and any figures to your Git repo
# 5) Pushing your commits to Github so that we can see.
# 6) This is due no later than the start of class 7.

# Read in and examine bee data
# Spobee column has density of P. larvae spores (the bacterium). 
# Hive has the ID of the hive sampled (3 samples/hive)
# Infection has a metric quantifying the degree of infection. We will turn this into yes/no whether infection is present. 
Bees <- Bees <- read.table(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/Bees.txt'), header=TRUE)
head(Bees)

# make hive a factor
Bees$fhive <- factor(Bees$Hive)

# Make a yes/no infection column (Infection01)
Bees$Infection01 <- Bees$Infection 
Bees$Infection01[Bees$Infection01 > 0] <- 1
Bees$fInfection01 <- factor(Bees$Infection01) # turn this into a factor

# Scale BeesN to improve model convergence (mean 0, standard deviation 1)
Bees$sBeesN <- scale(Bees$BeesN)


# Make a Cleveland dot-chart of spores vs. hive
dotchart(Bees$Spobee, groups = Bees$fhive, xlab='Spores', ylab='Hive ID')


# Q1. Does variance of spore density appear homogeneous among hives? Why or why not?

## A: No, variance is very low in hives with low or no infection, and hives that are infected (12- 14) have a high variance in spore density. 

# Q2. Try some transformations of the response variable to homogenize the variances (or at least improve it). Which transformation of spore density seems reasonable? Why?

Bees$log.Spobee <- log(Bees$Spobee)
dotchart(Bees$log.Spobee, groups = Bees$fhive, xlab='Spores', ylab='Hive ID')
## That didn't set us up well for later, so we are going to add 1 before taking the lo (so as not to take logs of negatives!)
Bees$log.Spobee <- log(Bees$Spobee + 1)


## A: Wow, I'm actually surprised how well that transformed the data in terms of leveling the with-in site variances. I.e. the variance becomes
## homogenous amongst hives. Log worked better than square root.

# Q3. Develop a simple linear model for transformed spore density. Include infection (fInfection01), number of bees (sBeesN) and their interaction as explanatory variables. Check for a hive effect by plotting standardized residuals (see the residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show your code and your plots. Do residuals look homogenous among hives?

model1 <- lm(log.Spobee ~ fInfection01 + sBeesN + fInfection01:sBeesN, data=Bees)
res1 <- residuals(model1, type='pearson')
plot(res1 ~ Bees$fhive)

## A: Residuals do not look homogeneous across hives. They are similar in terms of within hive spread, but appear to have vary different
## baselines (i.e. observations from a single hive appear to be correlated)

# Q4. What are the advantages of including hive as a random effect, rather than as a fixed effect?

## A: The disadvantage of including hive as a fixed effect is that it uses up 23 degrees of freedom, while still taking account of the
## nested structure of the data.

# Apply the Zuur protocol (10-step version outlined here, as used with the barn owl nesting data in Zuur Ch. 5):
# Step 1: Fit and check a "beyond optimal" linear regression (already done above)
# Step 2: Fit a generalized least squares version of the "beyond optimal" model (no need: we will use the linear regression model).

# Q5. Step 3. Choose a variance structure or structures (the random effects). What random effects do you want to try?

## A: I can't see anything other than hive that makes sense to include as a random effect, so I want to try it as a random intercept
## and as a random slope, random intercept effect.

# We will now fit a mixed effects (ME) model. Zuur et al. used the nlme package in R, but Douglas Bates now has a newer package that is widely used and that is called lme4. The benefits of lme4 include greater flexibility in the structure of the random effects, the option to use non-Gaussian error structures (for generalized linear mixed effects models, or GLMMs), and more efficient code to fit models. The main difference between nlme's lme() function and the lmer() function in lme4 is in how random effects are specified:
# model <- lmer(response ~ explanantoryvars + (1|random), data=mydata) # a random intercept model
# model <- lmer(response ~ explanantoryvars + (slope|random), data=mydata) # a random intercept and slope model
# One of the frustrations some people run into is that the lme4 package doesn't provide p-values. This stems from disagreements and uncertainty about how best to calculate p-values. However, if p-values are important to you, approximate p-values can be derived from the lmerTest package

# install.packages('lme4') # if needed
# install.packages('lmerTest') if needed
require(lmerTest)

# Q6. Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package (transformed spore density is response, fInfection01, sBeesN, and interaction are the explanatory variables). Show your code.

model2 <- lmer(log.Spobee ~ fInfection01 + sBeesN + fInfection01:sBeesN +(1|fhive), data=Bees)
model3 <- lmer(log.Spobee ~ fInfection01 + sBeesN + fInfection01:sBeesN +(sBeesN|fhive), data=Bees)


# Q7. Step 5. Compare the linear regression and ME model(s) with a likelihood ratio test, including correction for testing on the boundary if needed. Use the anova() command. This will re-fit your lmer model with maximum likelihood, but this is OK (note there are some debates about exactly how to best compare an lm and lmer model). Show your work and the results. Which random effect structure do you choose based on the results?

anova(model3, model2, model1)

## Data: Bees
## Models:
##    model1: log.Spobee ~ fInfection01 + sBeesN + fInfection01 * sBeesN
## model2: log.Spobee ~ fInfection01 + sBeesN + fInfection01:sBeesN + (1 | 
##                                                                        model2:     fhive)
## model3: log.Spobee ~ fInfection01 + sBeesN + fInfection01:sBeesN + (sBeesN | 
##                                                                       model3:     fhive)
##         npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
## model1    5 325.73 337.11 -157.87   315.73                          
## model2    6 253.43 267.09 -120.71   241.43 74.3041  1     <2e-16 ***
## model3    8 254.50 272.72 -119.25   238.50  2.9235  2     0.2318    
## ---
##    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## A: The random intercept model looks to be the best based on AIC. 

## To make sure that the p-value for the random slope element is not significant, adjust for testing on the boundary:

0.5*((1 - pchisq(2.9235, 1)) + (1 - pchisq(2.9235, 2)))

## The adjusted p value is still > than 0.1, so I'm sticking with the random intercept only model.

# Q8. Step 6. Check the model: plot standardized residuals vs. fitted values and vs. each predictor. (You can get standardized residuals with residuals(yourmodel, type='pearson')). How do they look?


res2 <- residuals(model2, type='pearson')
plot(res2 ~ fitted(model2))
plot(res2 ~ Bees$fInfection01)
plot(res2 ~ Bees$sBeesN)

## A: They generally look good. It looks like variance might be lower for infected hives, which may be a result of the log transformation. 

# Q9. Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a reduced model without the interaction term, also fit with ML. Use anova() to compare the models. Which model do you choose? Why?


model4 <- lmer(log.Spobee ~ fInfection01 + sBeesN + fInfection01:sBeesN +(1|fhive), data=Bees, REML=FALSE)
model5 <- lmer(log.Spobee ~ fInfection01 + sBeesN +(1|fhive), data=Bees, REML=FALSE)
anova(model4, model5)

## I choose the model without the interaction term, it has a lower AIC and the p-value on the interaction term is quite high (0.45).

# Q10. Step 8. Iterate #7 to arrive at the final model. Show your work. What is your final set of fixed effects?

model6 <- lmer(log.Spobee ~ sBeesN +(1|fhive), data=Bees, REML=FALSE)
model7 <- lmer(log.Spobee ~ fInfection01 +(1|fhive), data=Bees, REML=FALSE)
anova(model5, model6, model7)

## The model with both fixed effects has a slightly lower AIC, but the p-value for including both is only marginally significant compared
## to including only infection as a predictor.


# Q11. Step 9. Fit the final model with REML. Check assumptions by plotting a histogram of residuals, plotting Pearson standardized residuals vs. fitted values, and plotting Pearson standardized residuals vs. explanatory variables. Are there issues with the model? If so, how might you address them?

model8 <- lmer(log.Spobee ~ fInfection01 + sBeesN + (1|fhive), data=Bees)
res8 <- residuals(model8, type='pearson')
hist(res8)
plot(res8 ~ fitted(model8))
plot(res8 ~ Bees$fInfection01)
plot(res8 ~ Bees$sBeesN)

## A: I don't see any major problems with the model... their does seem to be a little more variance in the residuals for the non-infected
## hives, but I still think this is primarily thanks to the data transformation we conducted. The residuals appear to be normally distributed.
## One thought: The ecological interpretation of the effect of hive size on spore load seems like it might be different in infected and
## un-infected hives. It might make sense to jut remove the infected hives from this analysis if what you are really interested in is the
## effect of colony size on spore import or something... 

# Q12. Step 10. Interpret the model. The summary() command is useful here. What have you learned about American Foulbrood? 

summary(model8)

## A: It appears that while hive infection is a good predictor of hive spore density, the # of bees in the hive is not. I've learned
## that foulbrood spores are fairly ubiquitous in this system (only a few hives had no spores at all), but that larger hives are not
## likely to have a higher spore density. 


# Q13. Calculate the correlation between observations from the same hive as variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). Given the correlation among observations from the same hive, do you think it's a good use of time to sample each hive multiple times? Why or why not?

4.8222/(4.8222+0.6033)

##A: The correlation between observations from the same hive is 0.889. That seems really high to me, though I can't remember what suggested
## cutoffs are. I think it would be more valuable to add more hives, rather than more observations per hive, because the variation that needs
## to be explained is primarily between hive means, rather than within a hive. 
