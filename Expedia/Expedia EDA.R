#==================================================================================================#
#### Expedia Exploratory Analysis ####
#==================================================================================================#

## Set working directory
setwd('C:/Users/Michael/Documents/R/kaggle/Expedia')

## Load packages
library(data.table)
library(dplyr)
library(ggplot2)

## Read in data
data.train <- fread('train.csv')

str(data.train)
head(sample_n(data.train,1000000))

## High level exploration
data.train[,.N,by='is_booking'][,per := prop.table(.N),by='']

data.train[,.N,by='cnt'][order(-cnt)] ## wonder what this value even means with values so high

data.train[,.N,by='site_name'][order(-N)] ## 45 total sites with 11 sites less than 10,000

data.train[,.N,by=c('posa_continent','user_location_country')]

