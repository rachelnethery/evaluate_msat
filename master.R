## master script for data processing and analyses in:
## Mobile source benzene regulations and risk of childhood 
## and young adult hematologic cancers in Alaska: a quasi-experimental study

library(data.table)
library(dplyr)
library(np)
library(devtools)
library(cowplot)
library(ggplot2)
library(panelView)
library(gsynth)
library(clubSandwich)
library(lme4)
library(reshape2)
library(ggbrace)
library(zoo)
library(splines)
library(tidycensus)

########################
## 1. DATA PROCESSING ##
########################

## read in and process the measured confounders ##
source('01-process_confounders.R')

## plot time trends in measured confounders ##
source('02-plot_confounder_trends.R')

## read in and process the outcome data and merge with the measured confounders ##
source('03-process_outcomes.R')

## plot time trends in outcomes ##
source('04-plot_outcome_trends.R')

############################################
## 2. AGGREGATE RATES (ACROSS TIME/SPACE) ##
############################################

## make table S1 ##
source('05-table_aggregate_rates.R')

#######################################
## 3. DID ANALYSES WITH MIXED MODELS ##
#######################################

source('06-did_models.R')

source('07-did_plots.R')

########################################################
## 4. MATRIX COMPLETION MODELS TO RELAX PT ASSUMPTION ##
########################################################

source('08-mc_models.R')

source('09-mc_plots.R')

