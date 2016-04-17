####################################################################################################
## Kobe Modeling
####################################################################################################

## Set working directory
setwd('C:/Users/michael.hoyer/Documents/R/Kaggle/Kobe')

## Load packages
library(dplyr)
library(randomForest)
library(glmnet)
library(gbm)

## Load dataset
data.all <- read.csv('data.csv')
str(data.all)

## Subset train and test
train <- as.integer(row.names(data.all)[!is.na(data.all$shot_made_flag)])
test  <- as.integer(row.names(data.all)[ is.na(data.all$shot_made_flag)])

## Leave raw data alone, clone raw data for feature engineering
model.data <- data.all

####################################################################################################
## Feature engineering
####################################################################################################

## Remove columns that we won't use initially
notUsed <- c('game_event_id','game_id','lat','loc_x','loc_y','lon','team_id','team_name')
model.data <- model.data %>% select(which(!names(model.data) %in% notUsed))

## Initialize feature.names with features to be used as is
feature.names <- c('combined_shot_type','period','playoffs','shot_type','shot_zone_area',
                   'shot_zone_basic','shot_zone_range','opponent')

## Create home/away feature
model.data <- model.data %>% mutate(homeAway = factor(ifelse(substring(matchup,5,6) == 'vs',
                                                             'Home',
                                                             'Away')))
feature.names <- c(feature.names,'homeAway')

## Create buckets for time left on the clock (based loosely on clustering done in EDA)
model.data <- model.data %>% mutate(timeLeft = factor(
                                               ifelse(minutes_remaining >= 8,'min 8-11',
                                               ifelse(minutes_remaining >= 4,'min 4-7',
                                               ifelse(minutes_remaining >= 1,'min 1-3',
                                               ifelse(seconds_remaining > 50,'sec 51-60',
                                               ifelse(seconds_remaining > 40,'sec 41-50',
                                               ifelse(seconds_remaining > 30,'sec 31-40',
                                               ifelse(seconds_remaining > 20,'sec 21-30',
                                               ifelse(seconds_remaining > 10,'sec 11-20','sec 10 or less')))))))))
                                               )
feature.names <- c(feature.names,'timeLeft')

## Create conference feature
oppMap <- data.frame(opponent=factor(levels(data.all$opponent),levels=levels(data.all$opponent)),
                     conference=c('East','East','East','East','East','East','West','West','East',
                                  'West','West','East','West','West','East','East','West','East',
                                  'West','West','East','West','East','East','West','West','West',
                                  'West','West','East','West','West','East'))
model.data <- model.data %>% inner_join(.,oppMap,by='opponent')
feature.names <- c(feature.names,'conference')

## If we want to group rare shots within action type
action_type_rare <- data.all %>% 
    select(action_type) %>% 
    dplyr::group_by(action_type) %>%
    dplyr::summarise(count=n()) %>%
    filter(count < 30) %>%
    select(action_type) %>% 
    droplevels
action_type_rare <- action_type_rare$action_type

action_type <- data.all$action_type
levelsToKeep <- levels(action_type)[!levels(action_type) %in% levels(action_type_rare)]

action_type_2 <- factor(ifelse(action_type %in% action_type_rare,0,action_type),
                        labels=c('Rare Shot',levelsToKeep))
model.data <- cbind(model.data,action_type_2)

feature.names <- c(feature.names,'action_type_2')

####################################################################################################
## Random forest model
####################################################################################################

set.seed(824)
rfMakes <- randomForest(model.data[train,feature.names],
                        factor(model.data[train,'shot_made_flag']),
                        mtry=10,
                        ntree=20,
                        sample=1000,
                        do.trace=TRUE)
plot(rfMakes)
importance(rfMakes)

rfPred <- predict(rfMakes,newdata=model.data[test,!colnames(model.data) %in% c('shot_made_flag')],
                  type='prob')[,2]
submission <- data.frame(shot_id=model.data[test,'shot_id'],shot_made_flag=rfPred)
write.csv(submission,file='randomForest1.csv',row.names=FALSE)


####################################################################################################
## glmnet model
####################################################################################################

x <- model.matrix( ~ .,model.data[,c(feature.names)])[,-1]
y <- model.data$shot_made_flag

cv.ridge <- cv.glmnet(x[train,],
                      y[train],
                      type.measure='deviance',
                      alpha=0,
                      lambda=10 ^ seq(1,-5,length=100),
                      nfolds=10)
plot(cv.ridge)

ridgePred <- predict(cv.ridge,x[test,],s=cv.ridge$lambda.1se,type='response')
plot(density(ridgePred))

submission <- data.frame(shot_id=model.data[test,'shot_id'],shot_made_flag=ridgePred)
colnames(submission) <- c('shot_id','shot_made_flag')
head(submission)
write.csv(submission,file='ridge1.csv',row.names=FALSE)


####################################################################################################
## GBM model
####################################################################################################

gbmMakes <- gbm(shot_made_flag ~ .,
                model.data[train,c(feature.names,'shot_made_flag')],
                n.trees=100,
                )
