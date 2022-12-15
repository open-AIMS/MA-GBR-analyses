################################################################################
#Model relationship between hard coral and macroalgae
#percent cover data collected across various spatial scales - 
#latitude
#shelf position
#habitat
#depth
################################################################################

#*******************************************************************************
#Fix raw data error
#*******************************************************************************
#from Email:
# There is one reef Nomad, that is classified as inshore and mid-shelf. It’s the same reef. It was surveyed in two different years. I have fixed this up, but you will need to as well. 

mdat.original<-read.csv(file=paste0(REA_PRIMARY_PATH, "mdat2_2021_09.csv"))
mdat.original<- mdat.original %>% mutate(sector=as.factor(sector))
glimpse(mdat.original)
levels(mdat.original$sector)

reef.names<- mdat.original %>% dplyr::select(reef) %>% unique
nrow(reef.names)

reef.names.shelf<- mdat.original %>% dplyr::select(shelf, reef) %>% unique()
nrow(reef.names.shelf)

nomad.original<- mdat.original %>% filter(reef=="Nomad")
nomad.original

#Change 'Nomad' reef to be 'inshore' for all records
mdat<- mdat.original %>% mutate(shelf=case_when(reef=="Nomad"~"I",
                                                !reef=="Nomad"~shelf))
nomad<- mdat %>% filter(reef=="Nomad")
nomad

save(mdat, file=paste0(REA_PROCESSED_PATH, 'mdat.RData'))

#*******************************************************************************
#Understanding sampling design
#*******************************************************************************

load(file=paste0(REA_PROCESSED_PATH, 'mdat.RData'))

#How many depth levels?
mdat$depth.mn
range(mdat$depth.mn)
depth.levels<- mdat %>% dplyr::select(depth.mn) %>% unique
depth.levels
#5 depth levels

#How is depth4 different?
depth4.levels<- mdat %>% dplyr::select(depth4) %>% unique
depth4.levels
#depths 1 & 2 are merged into one level

#What do transects represent?
range(mdat$trno)
transects<- mdat %>% dplyr::select(trno) %>% unique
nrow(transects)
#I would call them sites. Most site/depth combinations only have one transect, but for a handful there is more than one

#How is season represented?
range(mdat$Season)
seasons<- mdat %>% dplyr::select(Season) %>% unique
nrow(seasons)

#Sampling design =
#Sector
#Shelf
#Reef
#Habitat(loctn)
#Site (trno) - this is the main level of replication
#Depth
#Transect (*new)= lat/long combo. At some date/reef/habitat/site(trno)/depth combinations, there are multiple records where the only difference is in the lat/longs. Otherwise, they are mostly unique.
#Time - are any revisited? No

mdat.tidy<- mdat %>% mutate(sector=as.factor(sector),
                            shelf=as.factor(shelf),
                            reef=as.factor(reef),
                            depth.mn=as.factor(depth.mn),
                            loctn=as.factor(loctn),
                            trno=as.factor(trno),
                            date=dmy(date),
                            Season=as.factor(Season)) %>%
        rename(site=trno, season=Season, habitat=loctn, depth=depth.mn) %>% 
        relocate(season, habitat, site, depth, depth4, X, .after=date)

#select the variables of interest
ess.vars<- mdat.tidy %>% dplyr::select(sector:X, lat, long, hc, sc, hc.sc, total.ma, -depth4)

#How many rows are transect replicates?
replicates.latlong<- ess.vars %>% mutate(lat.long=paste0(lat, long)) %>%
        group_by(date, reef, habitat, lat.long, depth) %>% 
        summarise(sample.size=n()) %>% arrange(-sample.size)
(reps.latlong<-nrow(replicates.latlong))
#33 less rows when counting sample size across lat/longs

#Any other potentially useful variables to add?
#year
#Month
#subregion (sector/shelf combo)
#lat.long combo
mdat.3<- ess.vars %>% mutate(month=as.factor(month(date)),
                             year=as.factor(year(date)),
                             subregion=factor(paste0(sector,shelf)),
                             latlong=factor(paste0(lat, '.', long))) %>%
        relocate(year, month, .after=date) %>% 
        relocate(subregion, .after=shelf) %>%
        relocate(latlong, .after=long)

#Which seasons capture which months?
season.1<- mdat.3 %>% filter(season=='1') %>% droplevels
levels(season.1$month)
#Season 1 = Jan, Feb
season.2<- mdat.3 %>% filter(season=='2') %>% droplevels
levels(season.2$month)
#Season 2 = April, May, June
season.3<- mdat.3 %>% filter(season=='3') %>% droplevels
levels(season.3$month)
#Season 3 = July, August, September
season.4<- mdat.3 %>% filter(season=='4') %>% droplevels
levels(season.4$month)
#Season 4 = October, December

#No surveys in March or November??
levels(mdat.3$month)
#Nope

#how many levels of relevant spatial factors?
levels(mdat.3$sector)
#11 sectors
levels(mdat.3$shelf)
#3 shelf positions
levels(mdat.3$habitat)
#4 habitats
levels(mdat.3$depth)
#5 depths
levels(mdat.3$site)
#324 sites
levels(mdat.3$latlong)
#323 latlongs
replicates<- mdat.3 %>% group_by(sector, shelf, subregion, reef, date, year, 
                                 month, season, habitat, site, lat, 
                                 long, latlong, depth) %>% 
        summarise(sample.size=n()) %>% arrange(-sample.size)
(reps<-nrow(replicates))
(rows.all<-nrow(ess.vars))
(transect.replicates<-rows.all-reps)
#33 replicate transects across 324 date/reef/habitat/site/depth combinations

#Where are the extra transects?
(replicates.2<- replicates %>% filter(sample.size>1) %>% droplevels)

#Since there are replicate transects they will need unique transect names....
#rename the unique identifier (X) as 'transect'
mdat.4<- mdat.3 %>% rename(transect=X)

save(mdat.4, file=paste0(REA_PROCESSED_PATH,'mdat.4.RData'))

#******************************************************************************
#variable distributions
#response
#predictors
#covariates
#******************************************************************************

load(file=paste0(REA_PROCESSED_PATH,'mdat.4.RData'))
glimpse(mdat.4)

#What is the distribution of the main predictor (hard coral cover)?
hist(mdat.4$hc)

#What is the distribution of the response variable (macroalgae cover)?
hist(mdat.4$total.ma)

#For the relationship between hard coral cover and macroalgae, depth, shelf and latitude are the main potentially interacting spatial scales of interest

#What is the date range of all surveys?
range(mdat.4$date)
#between August 1996 and December 2008

#How are the surveys distributed through time for shelf x depth combination?
ggplot(mdat.4, aes(y=as.numeric(as.character(year)), x=depth)) +
        geom_boxplot()+
        facet_wrap(~shelf)+
        theme_bw()

#How are the surveys distributed through time for sector x depth combination?
ggplot(mdat.4, aes(y=date, x=depth)) +
        geom_boxplot()+
        facet_wrap(~sector)+
        theme_bw()

#How are the surveys distributed through time for habitat x depth combination?
ggplot(mdat.4, aes(y=as.numeric(as.character(year)), x=depth)) +
        geom_boxplot()+
        facet_wrap(~habitat)+
        theme_bw()

#Lagoon habitats generally fewer surveys before 2000 and deep reef slopes don't exist in these habitats

#How are the surveys distributed through time for habitat x depth combination?
ggplot(mdat.4, aes(y=as.numeric(as.character(year)), x=depth, colour=shelf)) +
        geom_boxplot()+
        facet_wrap(~season)+
        theme_bw()

#******************************************************************************
#EDA
#******************************************************************************

load(file=paste0(REA_PROCESSED_PATH, file='mdat.4.RData'))
glimpse(mdat.4)

#Plot raw data:
mdat.4 %>% ggplot()+ geom_smooth(aes(y=total.ma, x=hc, colour=shelf)) +
        facet_wrap(~depth)+
        theme_classic() 

mdat.4 %>% ggplot()+ geom_smooth(aes(y=total.ma, x=hc, colour=depth)) +
        facet_wrap(~habitat)+
        theme_classic()
#Relationships at each depth are pretty similar across habitats


#******************************************************************************
#Data Wrangling for models
#******************************************************************************

load(file=paste0(REA_PROCESSED_PATH,'mdat.4.RData'))
glimpse(mdat.4)


#site and transect are converted into nested random variables
#year, season and habitat are additional (but not nested) random variables
#the greatest survey coverage through time is for:
#shelf -inshore
#sector - Whitsundays
#habitat - Front and Back
#season - 4
#depth - 2,5,10
#So these levels will be the first levels in each factor for the model
mdat.5<-mdat.4 %>% mutate(habitat=factor(habitat, levels=c('Back','Front', 'Lagoon', 'Channel')))

mdat.6<- mdat.5 %>% 
        mutate(site=factor(paste0(reef, habitat, site)),
               transect=factor(paste0(site, transect)),
               sector=factor(sector, levels=c('WH','CA','TO','SW','CB','CG','PC','CL','IN','CU','PO')),
               season=factor(season, levels=c('4', '1', '2', '3')),
               depth=factor(depth, levels=c('2', '5', '10', '15', '1'))) %>% droplevels

table(mdat.6$total.ma)
#The response variable (total.ma) is percent cover data, suited to a Beta distribution.
#Needs to be on a scale from 0-1, with no zeros and no ones
#Are there zeros?
zeros<-mdat.6 %>% filter(total.ma==0) %>% droplevels
(zeros.count<- nrow(zeros))
#Yes -677

#Are there 100s?
ones<-mdat.6 %>% filter(total.ma==100) %>% droplevels
(ones.count<- nrow(ones))
#No

#transform to 0-1 scale
dat.beta.prop<- mdat.6 %>% mutate(total.ma=total.ma/100)

#Convert zeros to 0.01
dat.beta<- dat.beta.prop %>% mutate(Cover.ma = total.ma,
                                    total.ma=case_when(total.ma==0.0~0.001,
                                                       total.ma>0.0~total.ma))

#check again
zeros.beta<-dat.beta %>% filter(total.ma==0) %>% droplevels
nrow(zeros.beta)

ones.beta<-dat.beta %>% filter(total.ma==1) %>% droplevels
nrow(ones.beta)

save(dat.beta, file=paste0(REA_PROCESSED_PATH, file='dat.beta.RData'))


###############################################################################
# macroalgae~ hard coral model:
#Depth*Shelf
###############################################################################

#*****************
#Model structure *
#*****************

#response - total macroalgae (percent cover)
#         - beta distribution
#predictors: hc*shelf*depth
#random variables - reef/site/transect (nested)

#***********************************************
#Set up dataframes and indices for predictions *
#***********************************************
##Set up dataframes and indices for predictions for:
## 1. (Rawdata) - get predictions associated with observed data (1:nrow(dat.beta.prop))
## 2. (Coral/depth/shelf predictions) - for plotting the trends (newdata.index.depthshelf)
## 3. (residuals) - coral/depth/shelf predictions associated with observed predictors (residuals.newdata.index.depthshelf)

load(file=paste0(REA_PROCESSED_PATH, file='dat.beta.RData'))
##1
#raw data
raw.data.hc.nospatial<-dat.beta %>% ungroup %>%
        dplyr::select(total.ma, Cover.ma, hc, depth, year, season, date, sector,
                      shelf, reef, habitat, site, lat, long, transect)

raw.index.hc.nospatial<- 1:nrow(raw.data.hc.nospatial)

##2
#model predictions
newdata.depthshelf.hc.nospatial <- raw.data.hc.nospatial %>% 
        tidyr::expand(shelf, depth, hc=modelr::seq_range(hc, n=100)) %>%
        mutate(total.ma=NA,
               Cover.ma=NA,
               year=NA,
               season=NA,
               date=NA,
               sector=NA,
               reef=NA,
               habitat=NA,
               site=NA,
               lat=NA,
               long=NA,
               transect=NA)

newdata.index.depthshelf.hc.nospatial <- 1:nrow(newdata.depthshelf.hc.nospatial) + nrow(raw.data.hc.nospatial)

##3
#residuals
residuals.newdata.hc.nospatial <- raw.data.hc.nospatial %>% dplyr::select(hc, depth, shelf)

residuals.newdata.index.depthshelf.hc.nospatial <- 1:nrow(residuals.newdata.hc.nospatial) + nrow(newdata.depthshelf.hc.nospatial) + nrow(raw.data.hc.nospatial)

##bind dataframes for raw data, predictions and residuals
dat.depthshelf.hc.nospatial <- raw.data.hc.nospatial %>%
        bind_rows(newdata.depthshelf.hc.nospatial)%>%
        bind_rows(residuals.newdata.hc.nospatial)

save(raw.data.hc.nospatial, file=paste0(REA_PROCESSED_PATH,'raw.data.hc.nospatial.RData'))
save(raw.index.hc.nospatial, file=paste0(REA_PROCESSED_PATH,'raw.index.hc.nospatial.RData'))
save(newdata.depthshelf.hc.nospatial, file=paste0(REA_PROCESSED_PATH, 'newdata.depthshelf.hc.nospatial.RData'))
save(newdata.index.depthshelf.hc.nospatial, file=paste0(REA_PROCESSED_PATH, 'newdata.index.depthshelf.hc.nospatial.RData'))
save(residuals.newdata.hc.nospatial, file=paste0(REA_PROCESSED_PATH, 'residuals.newdata.hc.nospatial.RData'))
save(residuals.newdata.index.depthshelf.hc.nospatial, file=paste0(REA_PROCESSED_PATH, 'residuals.newdata.index.depthshelf.hc.nospatial.RData'))
save(dat.depthshelf.hc.nospatial, file=paste0(REA_PROCESSED_PATH,'dat.depthshelf.hc.nospatial.RData'))


################################################################################
# MA~HC model in INLA
################################################################################

#*******************************************************************************
# Inla presence/absence
#*******************************************************************************

#load dataframe with observations, newdata and residual prediction rows
load(file=paste0(REA_PROCESSED_PATH,'dat.depthshelf.hc.nospatial.RData'))
dat.depthshelf.hc.nospatial %>% head

#Create a presence/absence response variable
hc.ma.pa<- dat.depthshelf.hc.nospatial %>%
        mutate(total.ma.pa= ifelse(Cover.ma !=0, 1, Cover.ma))

save(hc.ma.pa, file=paste0(REA_PROCESSED_PATH, 'hc.ma.pa.RData'))

set.seed(123)
depthshelf.inla.hc.nospatial.pa <- inla(total.ma.pa ~ hc*depth*shelf  +
                                             f(reef, model='iid')+
                                             f(site, model='iid')+
                                             f(transect, model='iid'),
                                     data=hc.ma.pa, family="binomial",
                                     control.predictor=list(compute=TRUE, link=1),
                                     control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))


save(depthshelf.inla.hc.nospatial.pa, file=paste0(FINAL_REA_PATH, 'depthshelf.inla.hc.nospatial.pa.RData'))

#*******************
# Model validation *
#*******************

#Load presence/absence model
load(file=paste0(FINAL_REA_PATH, 'depthshelf.inla.hc.nospatial.pa.RData'))

#load dataframe with presence/absence variable
load(paste0(REA_PROCESSED_PATH, 'hc.ma.pa.RData'))
#subset to observation rows
hc.ma.pa.raw<- hc.ma.pa[1:1257,]

#draw 1000 posterior samples
draws <- inla.posterior.sample(1000, result=depthshelf.inla.hc.nospatial.pa, seed=123) %>% suppressWarnings()
##Rearrange draws so that it is a matrix of cellmeans, rather than a list
cellmeans = sapply(draws, function(x) x[['latent']])
##Index the cell means for fixed effects
i.mod <- sapply(c('^hc:1$', '^depth[0-9]{1,2}:1$', '^shelf.:1$',
                  '^hc:depth[0-9]{1,2}:1$',
                  '^hc:shelf.:1$',
                  '^depth[0-9]{1,2}:shelf.:1$',
                  '^hc:depth[0-9]{1,2}:shelf.:1$',
                  'Intercept'),
                function(x) grep(x, draws[[1]]$latent %>% rownames))

str(i.mod)

draws[[1]]$latent %>% rownames %>% tail(50)
##Get the predictions for fixed effects.
##Generate model matrix
Xmat <- model.matrix(~1+hc*depth*shelf, data = hc.ma.pa.raw)
wch <- c(grep('Intercept', names(i.mod)),
         grep('^\\^hc:1\\$$', names(i.mod)),
         grep('^\\^depth\\[0-9\\]\\{1,2\\}:1\\$$', names(i.mod)),
         grep('^\\^shelf.:1\\$$', names(i.mod)),
         grep('^\\^hc:depth\\[0-9\\]\\{1,2\\}:1\\$$', names(i.mod)),
         grep('^\\^hc:shelf.:1\\$$', names(i.mod)),
         grep('^\\^depth\\[0-9\\]\\{1,2\\}:shelf.:1\\$$', names(i.mod)),
         grep('^\\^hc:depth\\[0-9\\]\\{1,2\\}:shelf.:1\\$$', names(i.mod)))

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
        c('reef', 'site', 'transect'),
        function(x) grep(x, draws[[1]]$hyperpar %>% names))
sd <- sqrt(1/hyperpars[j.mod,])

## Predictions, accounting for distributions of random effects
pred.inla <- plogis(cellmeans.full.1 +
                            rnorm(nrow(hc.ma.pa.raw), 0, sd[1,]) +
                            rnorm(nrow(hc.ma.pa.raw), 0, sd[2,]) +
                            rnorm(nrow(hc.ma.pa.raw), 0, sd[3,])) #+

#simulate data
sim <- apply(pred.inla, 1, function(x) rbinom(nrow(hc.ma.pa.raw), size = 1, x))

#Assess residuals
mod.resids <- createDHARMa(
        simulatedResponse = sim,
        observedResponse = hc.ma.pa.raw$total.ma.pa,
        fittedPredictedResponse = apply(plogis(cellmeans.full.1), 2, median),
        ## fittedPredictedResponse = apply(pred.inla, 2, median),
        integerResponse = TRUE
)

png(paste0(FINAL_REA_PATH, 'mahc.inla.pa.resids.png'), width = 1152, height = 765)
mahc.inla.pa.resids<-mod.resids %>% plot()
dev.off()

#Use glmmTMB with the same structure and DHARMa residuals to validate model
pa.glmm.mahc <- glmmTMB(total.ma.pa ~ hc*depth*shelf  + 
                           (1|reef/site/transect),
                   data=hc.ma.pa, family="binomial")

pa.glmm.mahc.resids<-simulateResiduals(pa.glmm.mahc, plot=TRUE)

summary(pa.glmm.mahc)

#****************
# Model Summary *
#****************
load(file=paste0(FINAL_REA_PATH, 'depthshelf.inla.hc.nospatial.pa.RData'))

summary(depthshelf.inla.hc.nospatial.pa)

#Export fixed effects summary 
mahc.inla.pa.fixed<- as.data.frame(depthshelf.inla.hc.nospatial.pa$summary.fixed) 
write.csv(mahc.inla.pa.fixed, file=paste0(FINAL_REA_PATH, 'mahc.inla.pa.fixed.csv'))

#Export hyperparameters summary 
mahc.inla.pa.hyperpar<- as.data.frame(depthshelf.inla.hc.nospatial.pa$summary.hyperpar) 
write.csv(mahc.inla.pa.hyperpar, file=paste0(FINAL_REA_PATH, 'mahc.inla.pa.hyperpar.csv'))

#********************
# Model Predictions *
#********************

#Get predictions
nd.pa <- newdata.depthshelf.hc.nospatial %>%
        bind_cols(depthshelf.inla.hc.nospatial.pa$summary.fitted.values[newdata.index.depthshelf.hc.nospatial,]) %>%
        group_by(shelf, depth) %>%
        slice(50) %>%
        mutate(depth = factor(depth, levels = c("1", "2", "5", "10", "15"), 
                              labels=c("Reef flat", "Reef crest", "Upper slope", "Mid slope", "Deep slope")),
               shelf= factor(shelf, levels=c("I", "M", "O"),
                             labels=c("Inshore", "Midshelf", "Offshore"))) %>%
        rename(Shelf=shelf)

save(nd.pa, file=paste0(REA_PROCESSED_PATH, 'nd.pa.RData'))

#**********
# plot    *
#**********
load(file=paste0(REA_PROCESSED_PATH, 'nd.pa.RData'))

sPalette=c("#d7301f", "#fc8d59", "#fdcc8a")

mahc.pa.plot<- ggplot(nd.pa, aes(y = mean, x = depth, colour = Shelf)) +
        scale_colour_manual(values=sPalette) +
        geom_pointrange(aes(ymin = `0.025quant`, ymax = `0.975quant`), lwd=1,
                        position = position_dodge(0.2))+
        ylab("probability of presence")+
        xlab("")+
        theme_bw()

## ----end

ggsave(mahc.pa.plot, file=paste0(FINAL_REA_PATH, 'mahc.pa.plot.png'))


#*******************************************************************************
# Inla beta regression for presence data
#*******************************************************************************

load(file=paste0(REA_PROCESSED_PATH,'dat.depthshelf.hc.nospatial.RData'))

#Only investigate the relationship between MA~HC when MA is present
dat.ma.present<- dat.depthshelf.hc.nospatial %>% 
        filter(Cover.ma >0) %>% droplevels()

save(dat.ma.present, file=paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))

i.start <- 1 + dat.ma.present %>% nrow()

newdata.1 <- dat.beta %>%
        filter(Cover.ma>0) %>% droplevels() %>%
        group_by(shelf, depth) %>%
        nest() %>%
        mutate(hc = map(.x = data, .f=function(x=.x) seq(min(x$hc), max(x$hc), length=100))) %>%
        #restrict predictions to actual range of observed HC
        unnest(hc) %>%
        dplyr::select(-data)

dat.ma.present <- dat.ma.present %>%
        bind_rows(newdata.1)
i.end <- dat.ma.present %>% nrow()

set.seed(123)
depthshelf.inla.presence <- inla(Cover.ma ~ hc*depth*shelf  +
                                             f(reef, model='iid')+
                                             f(site, model='iid')+
                                             f(transect, model='iid'),
                                     data=dat.ma.present, 
                                     family="beta",
                                     control.predictor=list(compute=TRUE, link=1),
                                     control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))

## ----end

save(depthshelf.inla.presence, file=paste0(FINAL_REA_PATH, 'depthshelf.inla.presence.RData'))


#*******************
# Model validation *
#*******************

#load model
load(file=paste0(FINAL_REA_PATH, 'depthshelf.inla.presence.RData'))
#load dataframe with observations and newdata rows
load(paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))
#select observation rows only
dat.ma.present.obs <- dat.ma.present[1:580,]

#draw 1000 posterior samples from model
draws <- inla.posterior.sample(1000, result=depthshelf.inla.presence, seed=123) %>% suppressWarnings()
##Rearrange draws so that it is a matrix of cellmeans, rather than a list
cellmeans = sapply(draws, function(x) x[['latent']])
##Index the cell means for fixed effects
i.mod <- sapply(c('APredictor','^Predictor', '^hc:1$', '^depth[0-9]{1,2}:1$', '^shelf.:1$',
                  '^hc:depth[0-9]{1,2}:1$',
                  '^hc:shelf.:1$',
                  '^depth[0-9]{1,2}:shelf.:1$',
                  '^hc:depth[0-9]{1,2}:shelf.:1$', 'reef', 'site',
                  'transect','Intercept'),
                function(x) grep(x, draws[[1]]$latent %>% rownames))

draws[[1]]$latent %>% rownames %>% tail(50)

str(i.mod)

##Get the predictions for fixed effects.
##Generate model matrix
Xmat <- model.matrix(~1+hc*depth*shelf, data = dat.ma.present.obs)
wch <- c(grep('Intercept', names(i.mod)),
         grep('^\\^hc:1\\$$', names(i.mod)),
         grep('^\\^depth\\[0-9\\]\\{1,2\\}:1\\$$', names(i.mod)),
         grep('^\\^shelf.:1\\$$', names(i.mod)),
         grep('^\\^hc:depth\\[0-9\\]\\{1,2\\}:1\\$$', names(i.mod)),
         grep('^\\^hc:shelf.:1\\$$', names(i.mod)),
         grep('^\\^depth\\[0-9\\]\\{1,2\\}:shelf.:1\\$$', names(i.mod)),
         grep('^\\^hc:depth\\[0-9\\]\\{1,2\\}:shelf.:1\\$$', names(i.mod)))

names(i.mod)

wch

ii = unlist(i.mod[wch])

##multiply the predictions by the fixed effects for the covariates
cellmeans.full.1 <- t(cellmeans[ii,]) %*% t(Xmat)
cellmeans.full.1 %>% colMeans() %>% head

## hyperparameters
hyperpars = matrix(sapply(draws, function(x) x[['hyperpar']]), ncol = 1000)

##Index the hyperpars
j.mod <- sapply(c('reef', 'site', 'transect'),
        function(x) grep(x, draws[[1]]$hyperpar %>% names))
sd <- sqrt(1/hyperpars[j.mod,])
k.mod <- sapply('beta',
        function(x) grep(x, draws[[1]]$hyperpar %>% names))

##dipsersion parameter
phi <- hyperpars[k.mod,]

## Get predictions, accounting for random effects
pred.inla <- plogis(cellmeans.full.1 +
                            rnorm(nrow(dat.ma.present.obs), 0, sd[1,]) +
                            rnorm(nrow(dat.ma.present.obs), 0, sd[2,]) +
                            rnorm(nrow(dat.ma.present.obs), 0, sd[3,])) #+

a <- pred.inla * phi
b <- (1 - pred.inla) * phi

#Simulate data
sim <- sapply(1:nrow(pred.inla),function(x) rbeta(nrow(dat.ma.present.obs), a[x,], b[x,]))

#Assess residuals
mod.resids <- createDHARMa(
        simulatedResponse = sim,
        observedResponse = dat.ma.present.obs$Cover.ma,
        fittedPredictedResponse = apply(plogis(cellmeans.full.1), 2, median),
        ## fittedPredictedResponse = apply(pred.inla, 2, median),
        integerResponse = FALSE
)


png(paste0(FINAL_REA_PATH, 'mahc_inla_resids.png'), width = 1152, height = 765)
mahc_inla_resids<- mod.resids %>% plot()
dev.off()

#****************
# Model Summary *
#****************

load(file=paste0(FINAL_REA_PATH, 'depthshelf.inla.presence.RData'))
summary(depthshelf.inla.presence)

#Export fixed effects summary 
mahc.inla.presence.fixed<- as.data.frame(depthshelf.inla.presence$summary.fixed) 
write.csv(mahc.inla.presence.fixed, file=paste0(FINAL_REA_PATH, 'mahc.inla.presence.fixed.csv'))

#Export hyperparameters summary 
mahc.inla.presence.hyperpar<- as.data.frame(depthshelf.inla.presence$summary.hyperpar) 
write.csv(mahc.inla.presence.hyperpar, file=paste0(FINAL_REA_PATH, 'mahc.inla.presence.hyperpar.csv'))

#********************
# Model Predictions *
#********************
#*
nd.presence <- newdata.1 %>%
        bind_cols(depthshelf.inla.presence$summary.fitted.values[i.start:i.end,]) %>%
        mutate(depth = factor(depth, levels = c("1", "2", "5", "10", "15"), 
                              labels=c("Reef flat", "Reef crest", "Upper slope", "Mid slope", "Deep slope")),
               shelf= factor(shelf, levels=c("I", "M", "O"),
                             labels=c("Inshore", "Midshelf", "Offshore"))) %>%
        rename(Shelf=shelf)

save(nd.presence, file=paste0(REA_PROCESSED_PATH, 'nd.presence.RData'))

#**********
# plot    *
#**********

##PDF and .png version

load(file=paste0(REA_PROCESSED_PATH, 'nd.presence.RData'))

sPalette=c("#d7301f", "#fc8d59", "#fdcc8a")

(mahc.inla.presence.plot<-ggplot(nd.presence, aes(y = mean, x = hc, colour = Shelf)) +
        scale_colour_manual(values=sPalette) +
        geom_line() +
        geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`, fill = Shelf),alpha=0.3) +
        scale_fill_manual(values=sPalette) +
        scale_y_continuous('Macroalgae cover (%)',
                           labels=function(x) x*100, limits=c(0,0.6)) +
        xlab("Hard Coral Cover (%)")+
        theme_bw()+
        facet_grid(~depth)+
        theme(legend.title= element_blank(),
              axis.title.x=element_text(size=10, margin=margin(t=10), family = "sans"),
              axis.title.y=element_text(size=10, margin=margin(r=10), family = "sans"),
              axis.text.x=element_text(size=8, family = "sans"),
              axis.text.y=element_text(size=8, family = "sans"),
              legend.text = element_blank(),
              legend.position = "none",
              strip.text = element_text(size=10, family = "sans"), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()))

ggsave(mahc.inla.presence.plot, file=paste0(FINAL_REA_PATH, 'mahc.inla.presence.plot.png'), width=20)
ggsave(mahc.inla.presence.plot, file=paste0(FINAL_REA_PATH, 'mahc.inla.presence.plot.pdf'), width=7.5, height=2.5)
ggsave(mahc.inla.presence.plot, file=paste0(FINAL_REA_PATH, 'Fig5.tiff'), width=7.5, height=2.5)
save(mahc.inla.presence.plot, file=paste0(FINAL_REA_PATH, 'mahc.inla.presence.plot.RData'))

##EPS version
#Doesn't support specification of font or semi-transparency
load(file=paste0(REA_PROCESSED_PATH, 'nd.presence.RData'))

sPalette=c("#d7301f", "#fc8d59", "#fdcc8a")

(mahc.inla.presence.plot<-ggplot(nd.presence, aes(y = mean, x = hc, colour = Shelf)) +
                scale_colour_manual(values=sPalette) +
                geom_line() +
                geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`, fill = Shelf),alpha=0.3) +
                scale_fill_manual(values=sPalette) +
                scale_y_continuous('Macroalgae cover (%)',
                                   labels=function(x) x*100, limits=c(0,0.6)) +
                xlab("Hard Coral Cover (%)")+
                theme_bw()+
                facet_grid(~depth)+
                theme(legend.title= element_blank(),
                      axis.title.x=element_text(size=10, margin=margin(t=10)),
                      axis.title.y=element_text(size=10, margin=margin(r=10)),
                      axis.text.x=element_text(size=8),
                      axis.text.y=element_text(size=8),
                      legend.text = element_blank(),
                      legend.position = "none",
                      strip.text = element_text(size=10), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank()))

## ----end

ggsave(mahc.inla.presence.plot, file=paste0(FINAL_REA_PATH, 'Fig5.eps'), width=7.5, height=2.5)
save(mahc.inla.presence.plot, file=paste0(FINAL_REA_PATH, 'Fig5.RData'))

#***********************
# rates of HC decline  *
#***********************

#calculate the average rate of change in MA for every 10% increase in HC.
load(file=paste0(REA_PROCESSED_PATH, 'nd.presence.RData'))

shelf.list.minmax = vector(mode='list', length=length(levels(nd.presence$Shelf)))
names(shelf.list.minmax) <- levels(nd.presence$Shelf)

for(sh in levels(nd.presence$Shelf)) {
        
        
        shelf.dat= nd.presence %>% 
                filter(Shelf==sh) %>% droplevels
        
        depth.list.minmax = vector(mode='list', length=length(levels(shelf.dat$depth)))
        names(depth.list.minmax) <- levels(shelf.dat$depth)
        
        for(d in levels(shelf.dat$depth)){
                
                dat= shelf.dat %>% 
                        filter(depth==d) %>% droplevels
                
                min.hc.pred= dat %>% dplyr::select(Shelf, depth, hc, mean) %>% 
                        filter(hc==min(hc)) %>% droplevels %>% mutate(ma.HCmin=mean*100) %>%
                        rename(hc.min=hc) %>% dplyr::select(-mean)
                max.hc.pred= dat %>% dplyr::select(Shelf, depth, hc, mean) %>% 
                        filter(hc==max(hc)) %>% droplevels %>% mutate(ma.HCmax=mean*100) %>%
                        rename(hc.max=hc) %>% dplyr::select(-mean)
                
                d.range= min.hc.pred %>% 
                        left_join(max.hc.pred %>% 
                                          dplyr::select(Shelf, depth, hc.max, ma.HCmax) %>% 
                                          distinct) %>%
                        mutate(rate.exp=(hc.max-hc.min)/10, rate.per.10=(ma.HCmin-ma.HCmax)/rate.exp)
                
                depth.list.minmax[[d]]=d.range
                
        }
        
        ma.rate.per.10hc=do.call('rbind', depth.list.minmax) 
        
        shelf.list.minmax[[sh]]= ma.rate.per.10hc %>% mutate(Shelf=sh)
        
}

ma.change.rate.presence=do.call('rbind', shelf.list.minmax) %>% as.data.frame()

save(ma.change.rate.presence, file=paste0(FINAL_REA_PATH, 'ma.change.rate.presence.RData'))

#***************
# Create table *
#***************

load(file=paste0(FINAL_REA_PATH, 'ma.change.rate.presence.RData'))
write.csv(ma.change.rate.presence, file=paste0(FINAL_REA_PATH, 'ma.change.rate.presence.csv'))

ma.change.rate.presence %>% kable

#******************
# stats to report *
#******************

#extract values from the model so that inferences about differences, based on probabilities, can be made

load(file=paste0(FINAL_REA_PATH,'depthshelf.inla.presence.RData'))

# Extract posterior distributions, say 1,000 draws.
post <- inla.posterior.sample(n = 1e3, depthshelf.inla.presence)
#find from Intercept onwards.
post[[1]]$latent
post[[1]]$latent[3035:3064, , drop = FALSE] # bingo.
# extract above indices from each of the 1,000 elements in `post`
#   then transform the output to have each fixed-effect estimate as columns,
#   and each posterior draw as rows.
out.presence <- lapply(post, function(x)x$latent[3035:3064, , drop = FALSE]) %>%
        do.call("cbind.data.frame", args = .) %>%
        t %>%
        data.frame(row.names = NULL, check.names = FALSE)
# Clean the names in the data.frame so they correspond to the names of the
#   fixed effects in the model summary table
names(out.presence) <- gsub("\\:1$", "", names(out.presence))

save(out.presence, file=paste0(REA_MODEL_PATH, 'out.presence.RData'))

load(file=paste0(REA_MODEL_PATH, 'out.presence.RData'))
## Inshore depth 1 vs Inshore depth 2 at 10% hc
out.presence.in.10 <- out.presence %>%
        dplyr::mutate(
                d2in_int = `(Intercept)`,
                d2in_slp = hc ,
                d1in_int = `(Intercept)` + depth1,
                d1in_slp = hc + `hc:depth1`,
                d2in_est_at_hc10 = plogis(d2in_int + d2in_slp * 10),
                d1in_est_at_hc10 = plogis(d1in_int + d1in_slp * 10))

#mean macroalgae at 10% HC cover on inshore reefs at 2m depth
mean(out.presence.in.10$d2in_est_at_hc10)

#mean macroalgae at 10% HC cover on inshore reefs at 1m depth
mean(out.presence.in.10$d1in_est_at_hc10)

#fold increase in ma from 2m depth to 1m depth at 10% hc for inshore reefs
fold_change.in.10 <- out.presence.in.10$d1in_est_at_hc10 / out.presence.in.10$d2in_est_at_hc10
ggdist::mean_hdci(fold_change.in.10)

# probability of ma being greater at 1m depth compared to 2m depth, at 10% HC on inshore reefs
prob_greater <- out.presence.in.10$d1in_est_at_hc10 > out.presence.in.10$d2in_est_at_hc10
sum(prob_greater) / length(prob_greater)

#*****************************************************************************
## Inshore depth 1 vs Inshore depth 2 at 50% hc
out.presence <- out.presence %>%
        dplyr::mutate(
                d2in_int = `(Intercept)`,
                d2in_slp = hc ,
                d1in_int = `(Intercept)` + depth1,
                d1in_slp = hc + `hc:depth1`,
                d2in_est_at_hc50 = plogis(d2in_int + d2in_slp * 50),
                d1in_est_at_hc50 = plogis(d1in_int + d1in_slp * 50))

mean(out.presence$d2in_est_at_hc50)
mean(out.presence$d1in_est_at_hc50)


fold_change <- out.presence$d2in_est_at_hc50 / out.presence$d1in_est_at_hc50
ggdist::mean_hdci(fold_change)

# Or probability of depth 2 being greater
prob_greater <- out.presence$d2in_est_at_hc50 > out.presence$d1in_est_at_hc50
sum(prob_greater) / length(prob_greater)


load(file=paste0(REA_PROCESSED_PATH, 'nd.presence.RData'))
(means<- nd.presence %>% group_by(Shelf, depth) %>% summarise(means=100*mean(mean)))

load(file=paste0(REA_PROCESSED_PATH, 'nd.presence.RData'))
max.ma<- nd.presence %>% group_by(Shelf, depth) %>% filter(mean==max(mean)) %>% droplevels 

