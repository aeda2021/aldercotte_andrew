#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('[your.directory]/ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

df <- amniote_data %>% filter(litter_or_clutch_size_n!=0)
df <- df %>% filter(Class=='Reptilia')
df <- df %>% select(where(~!all(is.na(.x))))
df$y <- log(df$litter_or_clutch_size_n)

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.

ggplot(df, aes(y)) + geom_histogram()
df$y %>% summary

##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.

preprocesses <- preProcess(df, method = 'medianImpute')
p_impute <- predict(preprocesses, df)

cols=c(7:30,32)
p_impute_data=p_impute[,cols]

dim(p_impute_data)
names(p_impute_data)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(p_impute_data)){
            hist(p_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}


log_cols <- c(1:24)

p_impute_data[, log_cols] <- log(p_impute_data[, log_cols])

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(p_impute_data)){
            hist(p_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}
p_impute_data <- subset(p_impute_data, select = -c(litter_or_clutch_size_n))

## I think I no longer understand this as well as I thought... the response variable should be log transformed so that it better fits
## the error distribution assumed for the linear model. I guess the data in the right-hand tails for the predictors are
## exerting undue leverage on the model fits, and taking the log of the data helps minimalize that leverage.


##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?

apriori_formula <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
reptile_m0_lm <- train(apriori_formula, data = p_impute_data, method = 'lm', trControl = trcntrl, na.action = na.omit)

plotCV(reptile_m0_lm)

reptile_m0_lm

summary(reptile_m0_lm$finalModel)

## This is similar to the mammal model in that it predicts a similar amount of the variance in the data
## I don't recall exactly what it was for the mammal linear model, but the adjusted R-squared is 0.335 for this one.


##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?

folds <- createFolds(df$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
reptile_m1_enet <- train(y ~ ., data = p_impute_data, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)

plotCV(reptile_m1_enet)
reptile_m1_enet$results$Rsquared %>% max

reptile_m1_enet$results %>%
    ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
    geom_line() +
    geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')

## That really doesn't look very good...


##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
reptile_m2_gp <- train(y ~ ., data = p_impute_data, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

plotCV(reptile_m2_gp)

reptile_m2_gp$results$Rsquared %>% max

reptile_m2_gp$results %>% ggplot(aes(sigma, Rsquared)) +
    geom_line() + geom_point() + xlab('Sigma')

## While the elastic net model barely improved predictive performance over the linear model, the gp model increases r-squared quite a bit.


##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
##What does the number of random predictors selected indicate about interaction depth?

rf_gr <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
reptile_m3_rf <- train(y ~ ., data = p_impute_data, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

plotCV(reptile_m3_rf)

reptile_m3_rf$results$Rsquared %>% max

reptile_m3_rf$results %>%
    ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
    geom_line() +
    geom_point() +
    labs(colour = 'min.node.size')

## The fact that small node size maximizes r-squared would seem to suggest that there isn't that much noise in the data, compared to
## variation that might be explained by the predictor variables. I think we can interpret the small number of random predictors as 
## meaning that there aren't that many significant interactions between variables???

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?

## The Rnadom Forest model wins again! It has the higher r-squared, and so it is making the most accurate predictions. The predicted vs.
## observed plot is also visibly better. I'm becoming more and more convinced that machine learning, and espeially random forest, is a 
## really powerful predictive tool.

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 

varImp(reptile_m1_enet)
varImp(reptile_m2_gp)
varImp(reptile_m3_rf)

## Size matters most! Adult body mass is consistently the best predictor of clutch size, followed by various other size metrics

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.

partial(reptile_m2_gp, pred.var = c('adult_body_mass_g'), plot = TRUE)
partial(reptile_m3_rf, pred.var = c('adult_body_mass_g'), plot = TRUE)

##How do they differ?
##What does this say about the likely relation ship between litter/clutch size and the best predictor variable?

## They are pretty similar at the higher ends of adult body mass, which to me indicate there is a true, smooth relationship between
## the two that is saturating at the high end (plateaus somewhere around 22kg). I don't know what is going on at the low end, where
## random forest model predicts an increase in clutch size for really tiny reptiles (i.e. clutch size decreases with weight from 0 to 2.5 g)
## That reversal of the direction of the relationship is not picked up in the gaussian process model, but I suspect it might be picked
## out by a GAM with enough splines (minimally smoothed)

