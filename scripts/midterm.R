### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands


##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

library(dplyr)

df <- pinelands_bees %>% group_by(site_name, round) %>% summarize(n())
df <- df %>% rename(n = `n()`)

## I kept round in there for now, because I thought that time of year could potentially explain a good chunk
## of the variation in # of bees collected.

df <- left_join(df, land_cover)

## 2 Data picture
# plot the data and figure out what type of model might be best

library(ggfortify)

plot(df$n ~ df$Nat1600)
lmod <- lm(n ~ Nat1600, data=df)
autoplot(lmod)

## The QQ plot of residuals for the linear model really doesn't look very good, suggesting the data
## doesn't follow a normal distribution. In fact, the data is really a count of bees collected in a site during
## a set period of time, so maybe a poisson distribution would be more appropriate!

hist(df$n)
pmod1 <- glm(n ~ Nat1600, data = df, family = poisson)
summary(pmod1)
autoplot(pmod1)

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

## The QQ plot of the residuals for the Poisson model do look a little better than those for the linear
## model, so I think we're on the right track (perhaps a negative binomial distribution would be even better!)
## The variance still increases with the mean in the Scale-Location plot, but it isn't drastic. There
## were 3 collection rounds that just had much higher counts than all others, but they weren't all from
## hi or low natural cover sites, nor the same round (2 from 1, 1 from 2).

AIC(lmod)
AIC(pmod1)
anova(pmod1, test='Chisq')

## However, I'm somehow getting a lower AIC for the linear model, which really surprises me. I'm uncomfortable
## selecting the linear model with a gaussian distribution though, because I know the data is count data, and the
## histogram of counts and QQ plot of model residuals both suggest that poisson should be a better fit.
## Either way, the anova results suggest that Nat1600 is a significant predictor of the # of bees collected in a round.

## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

summary(pmod1)
## The model suggests that there is a small but statistically significant negative relationship between
## the amount of natural land-cover in a 1600m radius and the number of bees collected. 
## The expected number of bees collected at a site with 0% natural land-cover is:
exp(2.9756)
## So about 19.6 bees (this is well outside of the sampled range in natural land-cover, mind you)
## for each % of natural land-cover gained, the number of bees sampled would decrease by a factor of:
exp(-0.00877)
## Which is approximately 0.99, which appears to be a small decrease but is compounded as % land-cover increases 
## In other words, at 50% land-cover our expected count per round is:
exp(2.9756 + (50*-0.00877))
## or 12.6 bees per collection round. At 75% land-cover:
exp(2.9756 + (75*-0.00877))
## or 10.2 bees per collection round.
## The p-value on the effect of Nat1600 is highly significant, so the correlation between land-cover and collection at these
## sites is definitely a real one. 
anova(pmod1)
## However, natural land-cover explains less than 10% of the variation in bee counts. There are likely other
## factors affecting bee abundance more directly than land-cover, some of which may be associated with natural
## land-cover. Either way, this data suggests that further conversion of natural areas is unlikely to have
## a negative effect on bee abundance and may even increase it by a small amount. 

## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want

ggplot(df, aes(x=Nat1600, y=n, color=round)) +
    geom_point() +
    geom_smooth(span = 1,  se = FALSE)

pmod1_aug = augment(pmod1, data=df, type.predict="response", se_fit=T)

ggplot(data = pmod1_aug, aes(x = Nat1600, y = n, color=round)) + 
    geom_point() + 
    geom_line(aes(x = Nat1600, y = .fitted), size=2)

## I wanted to compare both but couldn't be bothered with the CI's, already running out of time!

## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

## I'm thinking it might make sense to include either round, or some adjusted date, as a random effect. My thinking
## is that forests might differ in the way floral resource availability changes with time, and so the slope
## of the relationship between natural landcover (~forest cover) and counts might be different at different times of year.

## something like: pmod2 <- glm(n ~ Nat1600 + Nat1600|round, data=df)

## Such a model would likely explain more of the variance in the data while minimizing the degrees of freedom
## lost by including round. However, we would not gain any insight into the direction of the relationship between round and
## abundance.

## Another option would be to include collection method as a random effect, if one suspected that certain collection
## methods might be more effective in different habitat types. 

### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.


# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)

## The observed abundance looks like count data, and contains a lot of 0's, meaning that it is unlikely
## to show a Gaussian error distribution. 

plot(modSel$observedAbundance ~ modSel$meanAnnualTemp)

## right away we see indication of variance increasing with mean... I'm actually a little lost here
## on what the right way to preview model-error distributions is. Do I choose one of each alternative?

gauss.error.test <- lm(observedAbundance ~ meanAnnualTemp + annualPrecipitation + totalEdge, data=modSel)
autoplot(gauss.error.test)
pois.error.test <- glm(observedAbundance ~ meanAnnualTemp + annualPrecipitation + totalEdge, data=modSel, family = poisson)
autoplot(pois.error.test)

## Looking at the QQ plot for the residual errors, the poisson distribution looks really good actually.

# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines

temp.test1 <- glm(observedAbundance ~ meanAnnualTemp, data=modSel, family=poisson)
temp.test2 <- glm(observedAbundance ~ meanSummerTemp, data=modSel, family=poisson)
AIC(temp.test1, temp.test2)
rain.test1 <- glm(observedAbundance ~ annualPrecipitation, data=modSel, family=poisson)
rain.test2 <- glm(observedAbundance ~ summerPrecipitation, data=modSel, family=poisson)
AIC(rain.test1, rain.test2)
edge.test1 <- glm(observedAbundance ~ distance2edge, data=modSel, family=poisson)
edge.test2 <- glm(observedAbundance ~ totalEdge, data=modSel, family=poisson)
AIC(edge.test1, edge.test2)

## Based on AIC, the better predictors for each category are mean summer temp, summer precipitation,
## and total edge within 500m

# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

library(lmtest)
library(MuMIn)

mod1 <- glm(observedAbundance ~ 1, data=modSel, family=poisson)
mod2 <- glm(observedAbundance ~ meanSummerTemp, data=modSel, family=poisson)
mod3 <- glm(observedAbundance ~ meanSummerTemp + totalEdge, data=modSel, family=poisson)
mod4 <- glm(observedAbundance ~ meanSummerTemp + totalEdge + summerPrecipitation, data=modSel, family=poisson)

lrtest(mod1, mod2, mod3, mod4)

# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

## The likelihood ratio test for the above models points to mean summer temperatures and edge effects
## as important predictors of counts. The chi-squared tests show that the model including temp is a
## significant improvement over the intercept-only model, and that the model including total edge is
## as significant improvement over the summer temp only one. The p-value on the chi-squared test comparing
## the model including summer precipitation to the previous model is pretty high, suggesting that summer 
## precipitation may not be an important predictor.*

## *I'm actually not entirely sure about this. I chose the order to introduce predictors based on the AIC
## of the single variable models, but I'm not 100% sure that enables me to rule out the 3rd effect as important.

summary(rain.test2)

## OK, now I feel even better about excluding it. In a precipitation only model, precipitation is still not a significant
## predictor of abundance (p= 0.053). While that is only narrowly true, the likelihood ratio test suggests
## that there might be a correlation between precipitation and one of the other variables, because including it
## on top of the other variables improves the model fit very very little. 

summary(mod3)

## In conclusion, the best model to describe the observed abundance data includes summer temperatures
## and edge proximity. The species in question appears to fare better in warmer regions, and it prefers
## core habitat with less edge, which match two aspects of the original hypothesis. The 3rd aspect, however,
## that the species would be more abundant in wetter regions, is not well corroborated by the data set. 

## *mean summer temp appears to be more important than edge effects



