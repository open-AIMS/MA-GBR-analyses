###############################################################################
#Set working directory
###############################################################################
#setwd("~/scripts")



###############################################################################
#libraries and directory paths necessary for all scripts
###############################################################################

## ---- pathPreparations

#directory path shortcuts used throughout
if(!dir.exists('../data')) dir.create('../data')
if(!dir.exists('../data/LTMP')) dir.create('../data/LTMP')
if(!dir.exists('../data/LTMP/primary')) dir.create('../data/LTMP/primary')
if(!dir.exists('../data/LTMP/processed')) dir.create('../data/LTMP/processed')
if(!dir.exists('../data/LTMP/modelled')) dir.create('../data/LTMP/modelled')

if(!dir.exists('../data/REA')) dir.create('../data/REA')
if(!dir.exists('../data/REA/primary')) dir.create('../data/REA/primary')
if(!dir.exists('../data/REA/processed')) dir.create('../data/REA/processed')
if(!dir.exists('../data/REA/modelled')) dir.create('../data/REA/modelled')

if(!dir.exists('../figures')) dir.create('../figures')
if(!dir.exists('../figures/REA')) dir.create('../figures/REA')
if(!dir.exists('../figures/LTMP')) dir.create('../figures/LTMP')

if(!dir.exists('../docs')) dir.create('../docs')

if(!dir.exists('../final')) dir.create('../final')
if(!dir.exists('../final/REA')) dir.create('../final/REA')
if(!dir.exists('../final/LTMP')) dir.create('../final/LTMP')
if(!dir.exists('../final/LTMP/GBRandShelf_resids')) dir.create('../final/LTMP/GBRandShelf_resids')
if(!dir.exists('../final/LTMP/reef_resids')) dir.create('../final/LTMP/reef_resids')
if(!dir.exists('../final/LTMP/reef_resids/all')) dir.create('../final/LTMP/reef_resids/all')

## ----end


## ---- loadlibraries
library(tidyverse)
library(INLA)
library(knitr)
library(emmeans)
library(lubridate)
library(ggnewscale)
library(ggsci)
library(viridis)
library(RColorBrewer)
library(effects)
library(glmmTMB)
library(DHARMa)
library(ggdist)
library(naniar)
library(inlatools)
library(INLAutils)
library(ggfortify)
library(gridExtra)
library(extrafont)
## ----end


###############################################################################
#                          SET ITERATION
###############################################################################

iter<-"final"

################################################################################
#                           SET PATHS
################################################################################

REA_PRIMARY_PATH<- "../data/REA/primary/"
REA_PROCESSED_PATH<-"../data/REA/processed/"
REA_MODEL_PATH<-"../data/REA/modelled/"
REA_FIGURES_PATH <- "../figures/REA/"
FINAL_REA_PATH<- "../final/REA/"

 if (iter=="final"){
        SCRIPT_PATH <- ""
        LTMP_PRIMARY_PATH <- "../data/LTMP/primary/"
        LTMP_PROCESSED_PATH<-"../data/LTMP/processed/"
        LTMP_MODEL_PATH <- "../data/LTMP/modelled/"
        LTMP_FIGURES_PATH <- "../figures/LTMP/"
        FINAL_LTMP_PATH <- "../final/LTMP/"
        
}

###############################################################################
#Data files necessary for scripts
###############################################################################

#Files do not need to be loaded here, they are loaded as necessary throughout the scripts

#*********************
        ##For scripts:
        ##    '1_LTMP_Macroalgae_temporal.R'
        ##    '1b_LTMP_Macroalgae_temporal_reeflevel_all.R'

        ##Need files:
        #load(file='../data/LTMP/processed/ltmp.benthos.cover.rm.RData')
        #load(file='../data/LTMP/processed/sample.reef.ltmp.rm.withDate.RData')

        
#*********************
        ##For scripts:
        ##    '2a_REA_hc.ma_model_final.R'
        ##    '2b_REA_hc.ma_compare_models.R'

        ##Need file:
        #read.csv(file=paste0(REA_PRIMARY_PATH, "mdat2_2021_09.csv"))


################################################################################
#Modelling macroalgae temporal trends for shelf position and Whole GBR using LTMP data
                #Includes:
                        #data wrangling
                        #EDA
                        #model/summary/figures/stats to report
################################################################################
source(file=paste0(SCRIPT_PATH, '1a_LTMP_Macroalgae_temporal.R'))



################################################################################
#Modelling macroalgae temporal trends for LTMP reefs:
        #Includes:
                #EDA
                #models/summaries/figure/stats to report
################################################################################
source(file=paste0(SCRIPT_PATH, '1b_LTMP_Macroalgae_temporal_reeflevel_all.R'))



################################################################################
#Model relationship between hard coral and macroalgae
        #One-off Rapid Ecological Assessments. Percent cover of benthos collected across various spatial scales - 
                #sector
                #shelf position
                #habitat
                #depth

#Includes:
        #Understanding sampling design
        #EDA
        #Data Wrangling for model
        #Model/summary/predictions/figure
        #Calculate average rate of MA decline for every 10% increase in HC
        #Extract probabilities of differences for specific comparisons of interest
################################################################################
source(file=paste0(SCRIPT_PATH, '2a_REA_hc.ma_model_final.R'))



################################################################################
#Model relationship between hard coral and macroalgae
#Various model structures compared to select the final model,
#particular emphasis on whether to include latitudinal variation in the model in some form
#comparisons based on DIC and WAIC
################################################################################
source(file=paste0(SCRIPT_PATH, '2b_REA_hc.ma_compare_models.R'))

