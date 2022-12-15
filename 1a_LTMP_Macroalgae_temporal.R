###############################################################################
#                          SET ITERATION
###############################################################################

iter<-"final"


#***************
#Data Wrangling*
#***************

#Load benthic data; grouped by 'BENTHOS_CODE' at transect level.
#Includes
#number of points per transect
#total number of benthic points per transect
#points as 'percent cover' (percentage of total benthic points)
load(file='../data/LTMP/processed/ltmp.benthos.cover.rm.RData')

#Select BENTHOS_CODE 'MA' (macroalgae) only, remove transect 6, calculate MA as proportion of benthic points
MA.1<- ltmp.benthos.cover.rm %>% filter(BENTHOS_CODE=="MA") %>% droplevels %>%
        mutate(P_CODE=as.factor(P_CODE),
               REPORT_YEAR=as.factor(REPORT_YEAR),
               SITE_NO=as.factor(SITE_NO),
               TRANSECT_NO=as.factor(TRANSECT_NO)) %>%
        filter(!TRANSECT_NO=='6') %>% droplevels %>%
        mutate(MA.prop=n.points/total.points)


#Check survey coverage in visits 1 and 2
v1<- MA.1 %>% filter(REPORT_YEAR=="1993") %>% droplevels
v1.spread<- v1 %>% ungroup %>% dplyr::select(A_SECTOR, SHELF, REEF_NAME) %>% distinct()
#29 reefs
#Most outer-shelf reefs in the CA and SW and WH sectors are missing

v2<- MA.1 %>% filter(REPORT_YEAR=="1994") %>% droplevels
v2.spread<- v2 %>% ungroup %>% dplyr::select(A_SECTOR, SHELF, REEF_NAME) %>% distinct()
#29 reefs
#No CL inshore reefs, only one TO inshore. Very few CB and CA offshore reefs (1 each). 

save(MA.1, file='../data/LTMP/processed/MA.1.RData')

#Visits 1 and 2 are excluded due to unevenly distributed sampling across shelfxsector combinations
#Remove sectors 'Princess Charlotte Bay' and 'Cape Grenville', as surveys only began in 2019
#site and transect are converted into nested variables
#Shelf levels are arranged so that an outer-shelf/year combination is the base of comparison, since inshore reefs are not surveyed in many of the years and are a poor option for a base comparison.
load(file='../data/LTMP/processed/MA.1.RData')
ma<-MA.1 %>% 
        filter(!VISIT_NO<3, !A_SECTOR %in% c("PC", "CG")) %>% droplevels %>% 
        mutate(REPORT_YEAR=factor(REPORT_YEAR, 
                                  levels=c("1995", "1996", "1997", "1998", "1999", "2000", "2001",
                                           "2002", "2003", "2004", "2005", "2006", "2007","2008", "2009",
                                           "2010", "2011", "2012", "2013",  "2014", "2015", "2016",
                                           "2017", "2018", "2019", "2020", "2021")),
               SITE_NO = factor(paste0(AIMS_REEF_NAME, SITE_NO)),
               TRANSECT_NO =factor(paste0(SITE_NO, TRANSECT_NO)),
               SHELF = factor(SHELF, levels=c('O','M','I'))) %>% droplevels


save(ma, file='../data/LTMP/processed/ma.RData')


#**********
#EDA reef *
#**********

load(file='../data/LTMP/processed/ma.RData')

mean.ma.reef<- ma %>% group_by(A_SECTOR, AIMS_REEF_NAME, REPORT_YEAR) %>%
        summarise(mean.ma=(mean(MA.prop))*100)

mean.ma.reef %>% ggplot() +
        geom_line(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma, colour=AIMS_REEF_NAME)) +
        geom_point(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma, colour=AIMS_REEF_NAME))+
        facet_wrap(~A_SECTOR)+
        theme_bw()+
        theme(legend.position = "none")


mean.ma.reef.CL<- mean.ma.reef %>% filter(A_SECTOR =="CL") %>% droplevels

mean.ma.reef.CL %>% ggplot() +
        geom_line(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma)) +
        geom_point(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma))+
        facet_wrap(~AIMS_REEF_NAME)+
        theme_bw()+
        theme(legend.position = "none")

mean.ma.reef.CA<- mean.ma.reef %>% filter(A_SECTOR =="CA") %>% droplevels

mean.ma.reef.CA %>% ggplot() +
        geom_line(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma)) +
        geom_point(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma))+
        facet_wrap(~AIMS_REEF_NAME)+
        theme_bw()+
        theme(legend.position = "none")


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# NOTE: Further investigation showed that the macroalgae spikes in 1999 for the following reefs reflected observer variation:
#Linnet Reef
#Lizard Isles
#Macgillivray Reef
#Martin Reef
#North Direction Island

#In this analysis the year 1999 is removed from models that represent shelf and whole GBR level

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#**************
# Remove 1999 *
#**************

load(file='../data/LTMP/processed/ma.RData')

ma.excl.1999<- ma %>% filter(!REPORT_YEAR=="1999") %>% droplevels

save(ma.excl.1999, file='../data/LTMP/processed/ma.excl.1999.RData')

#*********************
#Add variable 'month'*
#*********************

load(file= '../data/LTMP/processed/ma.excl.1999.RData')

load(file='../data/LTMP/processed/sample.reef.ltmp.rm.withDate.RData')

sample.data.month<- sample.reef.ltmp.rm.withDate %>%
        mutate(AIMS_REEF_NAME=as.factor(AIMS_REEF_NAME),
               SITE_NO = factor(paste0(AIMS_REEF_NAME, SITE_NO)),
               SHELF = factor(SHELF, levels=c('O','M','I')),
               REPORT_YEAR=factor(REPORT_YEAR, 
                                  levels=c("1995", "1996", "1997", "1998", "1999", "2000", "2001",
                                           "2002", "2003", "2004", "2005", "2006", "2007","2008", "2009",
                                           "2010", "2011", "2012", "2013",  "2014", "2015", "2016",
                                           "2017", "2018", "2019", "2020", "2021")),
               month=as.factor(month(Date))) %>%
        filter(!VISIT_NO<3, !A_SECTOR %in% c("PC", "CG"), !REPORT_YEAR=="1999") %>% droplevels 
        
ma=ma.excl.1999 %>% left_join(sample.data.month %>% dplyr::select(AIMS_REEF_NAME, SITE_NO, REPORT_YEAR, month))

save(ma, file=paste0(LTMP_PROCESSED_PATH, 'ma.', iter, '.RData'))


################################################################################
# Shelf level temporal Macroalgae model
################################################################################

#**********
#EDA plots*
#**********

#Plot raw data:

load(file=paste0(LTMP_PROCESSED_PATH, 'ma.', iter, '.RData'))

ma %>% ggplot()+ geom_boxplot(aes(y=MA.prop, x=REPORT_YEAR)) +
                facet_wrap(~SHELF)+
                theme_classic()+
                theme(axis.text.x=element_text(angle=90, hjust=1))
        
ggplot(ma, aes(y=MA.prop, x=REPORT_YEAR, fill=SHELF))+
                geom_boxplot()

        
        
#***************************************
#create dataframe for model predictions*
#***************************************
        
load(file=paste0(LTMP_PROCESSED_PATH, 'ma.', iter, '.RData'))

        #REPORT_YEAR order
        levels(ma$REPORT_YEAR)
        
        #SHELF order
        levels(ma$SHELF)
        
        #Create dataframe for predictions
        newdata <- ma %>% 
                ungroup() %>%
                dplyr::select(REPORT_YEAR, SHELF) %>%
                distinct() %>%
                mutate(n.points=NA,
                       total.points=1,
                       AIMS_REEF_NAME=NA,
                       SITE_NO=NA,
                       TRANSECT_NO=NA,
                       month=NA) 
        
        #Add newdata rows to the observation dataframe
        #Add observation-level random effects
        #create variable for non-macroalgae points
        dat <- ma %>% ungroup %>%
                dplyr::select(n.points, total.points, REPORT_YEAR, SHELF,
                              AIMS_REEF_NAME, SITE_NO, TRANSECT_NO, month) %>%
                bind_rows(newdata) %>% mutate(Obs = factor(1:n()),
                                              x.points = total.points-n.points)
        
        #index the newdata rows
        newdata.index <- 1:nrow(newdata) + nrow(ma)

save(dat, file=paste0(LTMP_PROCESSED_PATH,'dat.', iter, '.RData'))
save(newdata.index, file=paste0(LTMP_PROCESSED_PATH,'newdata.index.', iter, '.RData'))


#**********
#Run Model*
#**********

#load observation dataframe with newdata rows
load(file=paste0(LTMP_PROCESSED_PATH,'dat.', iter, '.RData'))

#load index for newdata rows
load(file=paste0(LTMP_PROCESSED_PATH,'newdata.index.', iter, '.RData'))

#Define linear combinations
newdata <- dat[newdata.index,]
Xmat <- model.matrix(~REPORT_YEAR*SHELF, data=newdata)
lincomb <- inla.make.lincombs(as.data.frame(Xmat))

#fit the model
   # binomial distribution with observation level random effects
set.seed(123)
tempMA.shelf.inla <- inla(n.points ~
                               1+REPORT_YEAR*SHELF +
                               f(AIMS_REEF_NAME, model = 'iid') +
                               f(SITE_NO, model = 'iid') +
                               f(TRANSECT_NO, model = 'iid')+
                               f(month, model = 'iid')+
                               f(Obs, model = 'iid',
                                 hyper = list(theta = list(prior = 'loggamma',
                                                           param = c(0.001, 0.1)))),
                       data = dat,
                       Ntrials = dat$total.points,
                       family = 'binomial',
                       control.predictor = list(compute = TRUE, link = 1),
                       control.fixed = list(mean = 0, prec = 0.001,
                                            mean.intercept = 0, prec.intercept = 0.01),
                       lincomb = lincomb,
                       control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE, mlik=TRUE)) #, neffp=TRUE))

#save(tempMA.shelf.inla, file=paste0(LTMP_MODEL_PATH, 'tempMA.shelf.inla.', iter, '.RData'))
save(tempMA.shelf.inla, file=paste0(FINAL_LTMP_PATH, 'tempMA.shelf.inla.', iter, '.RData'))


#*******************
# Model Validation *
#*******************

#load model
load(file=paste0(FINAL_LTMP_PATH,'tempMA.shelf.inla.', iter, '.RData'))

#load observation data
load(file=paste0(LTMP_PROCESSED_PATH, 'ma.', iter, '.RData'))

#draw 1000 posterior samples
draws <- inla.posterior.sample(1000, result=tempMA.shelf.inla, seed=123) %>% suppressWarnings()
##Rearrange draws so that it is a matrix of cellmeans, rather than a list
cellmeans = sapply(draws, function(x) x[['latent']])
##Index the cell means for fixed effects
i.mod <- sapply(c('APredictor','^Predictor','^REPORT_YEAR[0-9]{4}:1$', '^SHELF.:1$',
                  '^REPORT_YEAR[0-9]{4}:SHELF.:1$',
                  'AIMS_REEF_NAME','SITE_NO','month', 'Intercept',
                  'TRANSECT_NO', 'Obs'),
                function(x) grep(x, draws[[1]]$latent %>% rownames))

## check correct number of fixed effects under each name
str(i.mod)

## to get exact names;
draws[[1]]$latent %>% rownames %>% tail(100)

##Get the predictions for fixed effects.
##Generate model matrix
Xmat <- model.matrix(~1+REPORT_YEAR*SHELF, data = ma)
wch <- c(grep('Intercept', names(i.mod)),
        grep('^\\^REPORT_YEAR\\[0-9\\]\\{4\\}:1\\$$', names(i.mod)),
        grep('^\\^SHELF.:1\\$$', names(i.mod)),
        grep('^\\^REPORT_YEAR\\[0-9\\]\\{4\\}:SHELF.:1\\$$', names(i.mod)))

##Check that they are in the right order
names(i.mod)  
wch

ii = unlist(i.mod[wch])

##multiply the predictions by the fixed effects for the covariates (if there are no covariates, they are just multiplied by 1)
cellmeans.full.1 <- t(cellmeans[ii,]) %*% t(Xmat)
cellmeans.full.1 %>% colMeans() %>% head

#draw the hyperparameters
hyperpars = matrix(sapply(draws, function(x) x[['hyperpar']]), ncol = 1000)

##Index the hyperpars
j.mod <- sapply(c('AIMS_REEF_NAME','SITE_NO','TRANSECT_NO', 'month', 'Obs'),
                function(x) grep(x, draws[[1]]$hyperpar %>% names))

sd <- sqrt(1/hyperpars[j.mod,])

#backtransform predictions, accounting for random effects
## There is a distribution for each random effect
pred.inla <- plogis(cellmeans.full.1 +
                            rnorm(nrow(ma), 0, sd[1,]) +
                            rnorm(nrow(ma), 0, sd[2,]) +
                            rnorm(nrow(ma), 0, sd[3,]) +
                            rnorm(nrow(ma), 0, sd[4,]) +
                            rnorm(nrow(ma), 0, sd[5,]))

#Simulate data
sim <- apply(pred.inla, 1, function(x) rbinom(nrow(ma), size = ma$total.points, x))

#assess residuals
mod.resids <- createDHARMa(
        simulatedResponse = sim,
        observedResponse = ma$n.points,
        fittedPredictedResponse = apply(plogis(cellmeans.full.1), 2, median),
        integerResponse = TRUE
)

#png(paste0(LTMP_MODEL_PATH, 'tempMA.shelf.inla.resids.', iter, '.png'), width = 1152, height = 765)
png(paste0(FINAL_LTMP_PATH, 'GBRandShelf_resids/tempMA.shelf.inla.resids.', iter, '.png'), width = 1152, height = 765)
tempMA.shelf.inla.resids<-mod.resids %>% plot()
dev.off()


#****************
# Model Summary *
#****************

load(file=paste0(FINAL_LTMP_PATH, 'tempMA.shelf.inla.', iter, '.RData'))
summary(tempMA.shelf.inla)

#Export fixed effects summary 
tempMA.shelf.inla.fixed<- as.data.frame(tempMA.shelf.inla$summary.fixed) 
#write.csv(tempMA.shelf.inla.fixed, file=paste0(LTMP_MODEL_PATH, 'tempMA.shelf.inla.fixed.', iter, '.csv'))
write.csv(tempMA.shelf.inla.fixed, file=paste0(FINAL_LTMP_PATH, 'tempMA.shelf.inla.fixed.', iter, '.csv'))

#Export hyperparameters summary 
tempMA.shelf.inla.hyperpar<- as.data.frame(tempMA.shelf.inla$summary.hyperpar) 
#write.csv(tempMA.shelf.inla.hyperpar, file=paste0(LTMP_MODEL_PATH, 'tempMA.shelf.inla.hyperpar.', iter, '.csv'))
write.csv(tempMA.shelf.inla.hyperpar, file=paste0(FINAL_LTMP_PATH, 'tempMA.shelf.inla.hyperpar.', iter, '.csv'))

#******************
#Model Predictions*
#******************

load(file=paste0(LTMP_PROCESSED_PATH,'dat.', iter, '.RData'))
load(file=paste0(FINAL_LTMP_PATH,'tempMA.shelf.inla.', iter, '.RData'))

tempMA.shelf.inla.cellmeans <- tempMA.shelf.inla$summary.lincomb.derived %>%
        as.data.frame() %>%
        select(mean,`0.025quant`, `0.975quant`) %>%
        mutate(across(everything(), plogis)) %>%
        bind_cols(dat[newdata.index,]) %>%
        mutate(SHELF=factor(SHELF, levels=c('I','M','O'), 
                            labels=c('Inshore','Midshelf','Offshore')))
        
    
## ----end

save(tempMA.shelf.inla.cellmeans, file=paste0(LTMP_MODEL_PATH,'tempMA.shelf.inla.cellmeans.', iter, '.RData'))


################################################################################
# Whole GBR temporal macroalgae model
################################################################################

#***************************************
#create dataframe for model predictions*
#***************************************

#load observation data
load(file=paste0(LTMP_PROCESSED_PATH,'ma.', iter, '.RData'))
ma.gbr=ma

#create dataframe for model predictions
newdata.gbr <- ma.gbr %>% 
        ungroup() %>%
        dplyr::select(REPORT_YEAR) %>%
        distinct() %>%
        mutate(n.points=NA,
               total.points=1,
               SHELF=NA,
               AIMS_REEF_NAME=NA,
               SITE_NO=NA,
               TRANSECT_NO=NA,
               month=NA) 

#Add newdata rows to the observation dataframe
#Add observation-level random effects
#create variable for non-macroalgae points        
dat.gbr <- ma.gbr %>% ungroup %>%
        dplyr::select(n.points, total.points, REPORT_YEAR, SHELF,
                      AIMS_REEF_NAME, SITE_NO, TRANSECT_NO, month) %>%
        bind_rows(newdata.gbr) %>% mutate(Obs = factor(1:n()),
                                          x.points = total.points-n.points)
#Index newdata rows
newdata.index.gbr <- 1:nrow(newdata.gbr) + nrow(ma.gbr)

save(dat.gbr, file=paste0(LTMP_PROCESSED_PATH,'dat.gbr.', iter, '.RData'))
save(newdata.index.gbr, file=paste0(LTMP_PROCESSED_PATH,'newdata.index.gbr.', iter, '.RData'))
        

     
#**********
#Run Model*
#**********

load(file=paste0(LTMP_PROCESSED_PATH,'dat.gbr.', iter, '.RData'))
load(file=paste0(LTMP_PROCESSED_PATH,'newdata.index.gbr.', iter, '.RData'))

#Define linear combinations
newdata <- dat.gbr[newdata.index.gbr,]
Xmat <- model.matrix(~REPORT_YEAR, data=newdata)
lincomb <- inla.make.lincombs(as.data.frame(Xmat))

#fit the model
  #binomial distribution, observation-level random effects
set.seed(123)
tempMA.gbr.inla <- inla(n.points ~
                                   1+REPORT_YEAR +
                                   f(SHELF, model = 'iid') +
                                   f(AIMS_REEF_NAME, model = 'iid') +
                                   f(SITE_NO, model = 'iid') +
                                   f(TRANSECT_NO, model = 'iid')+
                                   f(month, model = 'iid')+
                                   f(Obs, model = 'iid',
                                     hyper = list(theta = list(prior = 'loggamma',
                                                               param = c(0.001, 0.1)))),
                           data = dat.gbr,
                           Ntrials = dat.gbr$total.points,
                           family = 'binomial',
                           control.predictor = list(compute = TRUE, link = 1),
                           control.fixed = list(mean = 0, prec = 0.001,
                                                mean.intercept = 0, prec.intercept = 0.01),
                           lincomb = lincomb,
                        control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE, mlik=TRUE)
)

save(tempMA.gbr.inla, file=paste0(FINAL_LTMP_PATH, 'tempMA.gbr.inla.', iter, '.RData'))
        

#*******************
# Model Validation *
#*******************
load(file=paste0(FINAL_LTMP_PATH,'tempMA.gbr.inla.', iter, '.RData'))
load(file=paste0(LTMP_PROCESSED_PATH, 'ma.', iter, '.RData'))

draws <- inla.posterior.sample(1000, result=tempMA.gbr.inla, seed=123) %>% suppressWarnings()
##Rearrange draws so that it is a matrix of cellmeans, rather than a list
cellmeans = sapply(draws, function(x) x[['latent']])
##Index the cell means for fixed effects
i.mod <- sapply(c('^REPORT_YEAR[0-9]{4}:1$', 'Intercept'),
                function(x) grep(x, draws[[1]]$latent %>% rownames))

str(i.mod)

draws[[1]]$latent %>% rownames %>% tail(50)

##Get the predictions for fixed effects.
##Generate model matrix
Xmat <- model.matrix(~1+REPORT_YEAR, data = ma)
wch <- c(grep('Intercept', names(i.mod)),
         grep('^\\^REPORT_YEAR\\[0-9\\]\\{4\\}:1\\$$', names(i.mod)))

names(i.mod)
wch

ii = unlist(i.mod[wch])

##multiply the predictions by the fixed effects for the covariates
cellmeans.full.1 <- t(cellmeans[ii,]) %*% t(Xmat)
cellmeans.full.1 %>% colMeans() %>% head

## hyperparameters
hyperpars = matrix(sapply(draws, function(x) x[['hyperpar']]), ncol = 1000)

##Index the hyperpars
j.mod <- sapply(#c('Obs'),
        c('SHELF','AIMS_REEF_NAME','SITE_NO','TRANSECT_NO', 'month', 'Obs'),
        function(x) grep(x, draws[[1]]$hyperpar %>% names))
sd <- sqrt(1/hyperpars[j.mod,])

#backtransform predictions, accounting for random effects
## There is a distribution for each random effect
pred.inla <- plogis(cellmeans.full.1 +
                            rnorm(nrow(ma), 0, sd[1,]) +
                            rnorm(nrow(ma), 0, sd[2,]) +
                            rnorm(nrow(ma), 0, sd[3,]) +
                            rnorm(nrow(ma), 0, sd[4,]) +
                            rnorm(nrow(ma), 0, sd[5,]) +
                            rnorm(nrow(ma), 0, sd[6,]))

#Simulate data
sim <- apply(pred.inla, 1, function(x) rbinom(nrow(ma), size = ma$total.points, x))

#Assess residuals
mod.resids <- createDHARMa(
        simulatedResponse = sim,
        observedResponse = ma$n.points,
        fittedPredictedResponse = apply(plogis(cellmeans.full.1), 2, median),
        integerResponse = TRUE
)

#png(paste0(LTMP_MODEL_PATH, 'tempMA.GBR.inla.resids.', iter, '.png'), width = 1152, height = 765)
png(paste0(FINAL_LTMP_PATH, 'GBRandShelf_resids/tempMA.GBR.inla.resids.', iter, '.png'), width = 1152, height = 765)
tempMA.GBR.inla.resids<-mod.resids %>% plot()
dev.off()


#****************
# Model Summary *
#****************

load(file=paste0(FINAL_LTMP_PATH, 'tempMA.gbr.inla.', iter, '.RData'))
summary(tempMA.gbr.inla)

#Export summary of fixed effects
tempMA.gbr.inla.fixed<- as.data.frame(tempMA.gbr.inla$summary.fixed) 
#write.csv(tempMA.gbr.inla.fixed, file=paste0(LTMP_MODEL_PATH, 'tempMA.gbr.inla.fixed.', iter, '.csv'))
write.csv(tempMA.gbr.inla.fixed, file=paste0(FINAL_LTMP_PATH, 'tempMA.gbr.inla.fixed.', iter, '.csv'))
        
#Export summary of hyperparameters
tempMA.gbr.inla.hyperpar<- as.data.frame(tempMA.gbr.inla$summary.hyperpar) 
#write.csv(tempMA.gbr.inla.hyperpar, file=paste0(LTMP_MODEL_PATH, 'tempMA.gbr.inla.hyperpar.', iter, '.csv'))
write.csv(tempMA.gbr.inla.hyperpar, file=paste0(FINAL_LTMP_PATH, 'tempMA.gbr.inla.hyperpar.', iter, '.csv'))
    
#******************
#Model Predictions*
#******************

load(file=paste0(FINAL_LTMP_PATH, 'tempMA.gbr.inla.', iter, '.RData'))
load(file=paste0(LTMP_PROCESSED_PATH,'dat.gbr.', iter, '.RData'))

tempMA.gbr.inla.cellmeans <- tempMA.gbr.inla$summary.lincomb.derived %>%
        as.data.frame() %>%
        select(mean,`0.025quant`, `0.975quant`) %>%
        mutate(across(everything(), plogis)) %>%
        bind_cols(dat.gbr[newdata.index.gbr,])

save(tempMA.gbr.inla.cellmeans, file=paste0(LTMP_MODEL_PATH,'tempMA.gbr.inla.cellmeans.', iter, '.RData'))


###############################################################################
# Combined plot
###############################################################################

#***************
#Data Wrangling*
#***************

load(file=paste0(LTMP_MODEL_PATH,'tempMA.shelf.inla.cellmeans.', iter, '.RData'))
load(file=paste0(LTMP_MODEL_PATH,'tempMA.gbr.inla.cellmeans.', iter, '.RData'))

gbr=tempMA.gbr.inla.cellmeans %>% dplyr::select(REPORT_YEAR,SHELF,mean,"0.025quant","0.975quant")%>%
    dplyr::mutate(SHELF = "GBR")


shelf.gbr=merge(tempMA.shelf.inla.cellmeans, gbr, 
                by=c("REPORT_YEAR", "SHELF", "mean", 
                     "0.025quant", "0.975quant"), all=TRUE)

shelf.gbr.lincombs<- shelf.gbr %>% mutate(SHELF=factor(SHELF, levels=c("GBR", "Inshore", "Midshelf", "Offshore")))

save(shelf.gbr.lincombs, file = paste0(LTMP_MODEL_PATH, 'shelf.gbr.lincombs.', iter, '.RData'))


#***********************************************************
#Plot model predictions for shelf model and GBR-wide model *
#***********************************************************

##PDF & .png Version

load(file = paste0(LTMP_MODEL_PATH, 'shelf.gbr.lincombs.', iter, '.RData'))    
levels(shelf.gbr.lincombs$SHELF)

sPalette=c("#252525","#d7301f", "#fc8d59", "#fdcc8a") ## order of colours GBR, inshore, mid-shelf, outer

plot.tempMA.shelf.lincombs<-ggplot(shelf.gbr.lincombs, aes(y=`mean`, 
                                           x=as.numeric(as.character(REPORT_YEAR)), 
                                           colour=SHELF,shape=SHELF)) +
    scale_colour_manual(values=sPalette)+
    geom_linerange(aes(ymin=`0.025quant`, ymax=`0.975quant`, 
                       x=as.numeric(as.character(REPORT_YEAR)), 
                       colour=SHELF), alpha=0.5) +
    geom_point(aes(y=`mean`, x=as.numeric(as.character(REPORT_YEAR)), colour=SHELF), size=2) +
    scale_shape_manual(values=c(15, 16, 17, 18))+
    geom_line(aes(y=`mean`, x=as.numeric(as.character(REPORT_YEAR)), 
                  colour=SHELF), alpha=0.5)+
    scale_y_continuous('Macroalgae cover (%)',labels=function(x) x*100, limits=c(0,0.1), n.breaks=8) +
    theme_classic() +
    geom_text(x=1995, y=0.1, label="a)", colour="black", size=5)+
    theme(  axis.line = element_line(),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=10,family = "sans"),
            #legend.title = element_blank(),
            axis.text.x=element_text(size=10, family = "sans"),
            axis.text.y=element_text(size=10, family = "sans"),
            #legend.text = element_text(size=20),
            legend.position = "none",  
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())


plot.tempMA.shelf.lincombs
        
#save(plot.tempMA.shelf.lincombs, file=paste0(LTMP_PROCESSED_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.RData'))
#ggsave(plot.tempMA.shelf.lincombs, file=paste0(LTMP_FIGURES_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.png'), width =18, height = 9, units="cm")
#ggsave(plot.tempMA.shelf.lincombs, file=paste0(LTMP_FIGURES_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.eps'), width =18, height = 9, units="cm") 

save(plot.tempMA.shelf.lincombs, file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.RData'))
ggsave(plot.tempMA.shelf.lincombs, file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.png'), width =7.5, height = 4, units="in")
ggsave(plot.tempMA.shelf.lincombs, file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.pdf'), width =7.5, height = 4, units="in")
ggsave(plot.tempMA.shelf.lincombs, file=paste0(FINAL_LTMP_PATH, 'Fig7a.', iter, '.tiff'), width =7.5, height = 4, units="in")


##EPS version
#Doesn't support specification of font or semi-transparency

load(file = paste0(LTMP_MODEL_PATH, 'shelf.gbr.lincombs.', iter, '.RData'))    
levels(shelf.gbr.lincombs$SHELF)

sPalette=c("#252525","#d7301f", "#fc8d59", "#fdcc8a") ## order of colours GBR, inshore, mid-shelf, outer

plot.tempMA.shelf.lincombs<-ggplot(shelf.gbr.lincombs, aes(y=`mean`, 
                                                           x=as.numeric(as.character(REPORT_YEAR)), 
                                                           colour=SHELF,shape=SHELF)) +
        scale_colour_manual(values=sPalette)+
        geom_linerange(aes(ymin=`0.025quant`, ymax=`0.975quant`, 
                           x=as.numeric(as.character(REPORT_YEAR)), 
                           colour=SHELF)) +
        geom_point(aes(y=`mean`, x=as.numeric(as.character(REPORT_YEAR)), colour=SHELF), size=2) +
        scale_shape_manual(values=c(15, 16, 17, 18))+
        geom_line(aes(y=`mean`, x=as.numeric(as.character(REPORT_YEAR)), 
                      colour=SHELF))+
        scale_y_continuous('Macroalgae cover (%)',labels=function(x) x*100, limits=c(0,0.1), n.breaks=8) +
        theme_classic() +
        geom_text(x=1995, y=0.1, label="a)", colour="black", size=5)+
        theme(  axis.line = element_line(),
                axis.title.x=element_blank(),
                axis.title.y=element_text(size=10),
                #legend.title = element_blank(),
                axis.text.x=element_text(size=10),
                axis.text.y=element_text(size=10),
                #legend.text = element_text(size=20),
                legend.position = "none",  
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank())


plot.tempMA.shelf.lincombs

save(plot.tempMA.shelf.lincombs, file=paste0(FINAL_LTMP_PATH, 'Fig7a.', iter, '.RData'))
ggsave(plot.tempMA.shelf.lincombs, file=paste0(FINAL_LTMP_PATH, 'Fig7a.', iter, '.eps'), width =7.5, height = 4, units="in")


#**********************
#Some stats to report *
#**********************

load(file = paste0(LTMP_MODEL_PATH, 'shelf.gbr.lincombs.', iter, '.RData'))

#GBR-wide mean
mean.gbr<-shelf.gbr.lincombs %>% filter(SHELF=='GBR') %>% droplevels
    
mean(mean.gbr$mean)

#Shelf x year means in descending order
mean.desc<-shelf.gbr.lincombs %>% filter(!SHELF=='GBR') %>% 
arrange(-mean) %>% dplyr::select(SHELF,REPORT_YEAR, mean, `0.025quant`, `0.975quant`)

mean.desc[1:20,]

#Shelf x year means in ascending order
mean.asc<-shelf.gbr.lincombs %>% filter(!SHELF=='GBR') %>% 
arrange(mean) %>% dplyr::select(SHELF,REPORT_YEAR, mean,  `0.025quant`, `0.975quant`)

mean.asc[1:10,]

mid<- mean.desc %>% filter(SHELF=="Midshelf") %>% droplevels

offshore<- mean.desc %>% filter(SHELF=="Offshore") %>% droplevels

inshore<- mean.desc %>% filter(SHELF=="Inshore") %>% droplevels

#inshore range
ma.shelf.inshore<- shelf.gbr.lincombs %>% filter(SHELF=='Inshore') %>% droplevels
range(ma.shelf.inshore$mean)

#GBR mean in 2021
shelf.gbr.lincombs %>% filter(SHELF=='GBR', REPORT_YEAR=="2021") %>% 
droplevels %>% dplyr::select(mean)

#each shelf and also whole GBR mean across the temporal series
shelf.gbr.lincombs %>% group_by(SHELF) %>% dplyr::select(SHELF, mean,`0.025quant`, `0.975quant`) %>% 
summarise(temp.mean=mean(mean), mean.lowerUI=mean(`0.025quant`), mean.upperUI=mean(`0.975quant`))
