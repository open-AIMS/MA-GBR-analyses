###############################################################################
#                          SET ITERATION
###############################################################################

iter<-"final"


###############################################################################
# MA model for one reef
###############################################################################

load(file='../data/LTMP/processed/ma.RData')

ma.qaqc.flag<- ma %>% mutate(ReefYear=paste0(REPORT_YEAR,AIMS_REEF_NAME))
ma.excl.1999.CLIM<- ma.qaqc.flag %>% filter(!ReefYear %in% c("1999Linnet Reef",
                                                        "1999Martin Reef",
                                                        "1999Lizard Isles",
                                                        "1999Macgillivray Reef",
                                                        "1999North Direction Island")) %>% droplevels

save(ma.excl.1999.CLIM, file=paste0(LTMP_PROCESSED_PATH, 'ma.excl.1999.CLIM.', iter, '.RData'))



#**********
#EDA reef *
#**********

load(file=paste0(LTMP_PROCESSED_PATH, 'ma.excl.1999.CLIM.', iter, '.RData'))

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
               ReefYear=paste0(REPORT_YEAR,AIMS_REEF_NAME),
               month=as.factor(month(Date))) %>%
        filter(!VISIT_NO<3, !A_SECTOR %in% c("PC", "CG"),
               !ReefYear %in% c("1999Linnet Reef",
                                "1999Martin Reef",
                                "1999Lizard Isles",
                                "1999Macgillivray Reef",
                                "1999North Direction Island")) %>% droplevels 

ma.reef=ma.excl.1999.CLIM %>% left_join(sample.data.month %>% dplyr::select(AIMS_REEF_NAME, SITE_NO, REPORT_YEAR, month))

save(ma.reef, file=paste0(LTMP_PROCESSED_PATH, 'ma.reef.', iter, '.RData'))

load(file=paste0(LTMP_PROCESSED_PATH, 'ma.reef.', iter, '.RData'))
ma=ma.reef

glimpse(ma)
mean.ma.reef<- ma %>% group_by(A_SECTOR, AIMS_REEF_NAME, REPORT_YEAR) %>%
        summarise(mean.ma=(mean(MA.prop))*100)

mean.ma.reef %>% ggplot() +
        geom_line(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma, colour=AIMS_REEF_NAME)) +
        geom_point(aes(x=as.numeric(as.character(REPORT_YEAR)), y=mean.ma, colour=AIMS_REEF_NAME))+
        facet_wrap(~A_SECTOR)+
        theme_bw()+
        theme(legend.position = "none")

#******************
# Model Structure *
#******************

#Loop individual models for each reef

#response - number of macroalgae points/total benthic points
#         - binomial distribution
#predictors: year
#random variables - site, transect (nested), month

#Collate:
        #Fixed effects
        #hyperparameters
        #model predictions


#****************
#Data Wrangling *
#****************

load(file=paste0(LTMP_PROCESSED_PATH, 'ma.reef.', iter, '.RData'))
ma=ma.reef
glimpse(ma)
levels(ma$REPORT_YEAR)

#Set first two levels to be an odd year with no sampling gaps, followed by an even year with no sampling gaps
ma$REPORT_YEAR<-relevel(ma$REPORT_YEAR, "2001")
levels(ma$REPORT_YEAR)
ma$REPORT_YEAR<-relevel(ma$REPORT_YEAR, "2002")
levels(ma$REPORT_YEAR)

#**********************
#Set up results lists *
#**********************

modelsummary.list = vector(mode='list', length=length(levels(ma$AIMS_REEF_NAME)))
names(modelsummary.list)<- levels(ma$AIMS_REEF_NAME)

fixedeffects.list = vector(mode='list', length=length(levels(ma$AIMS_REEF_NAME)))
names(fixedeffects.list)<- levels(ma$AIMS_REEF_NAME)

hyperparameters.list = vector(mode='list', length=length(levels(ma$AIMS_REEF_NAME)))
names(hyperparameters.list)<- levels(ma$AIMS_REEF_NAME)

predictions.list = vector(mode='list', length=length(levels(ma$AIMS_REEF_NAME)))
names(predictions.list)<- levels(ma$AIMS_REEF_NAME)


#*********************************
#Loop model for individual reefs *
#*********************************

count=0

for (reef in levels(ma$AIMS_REEF_NAME)) {

        
#Subset data by reef        
reef.data<- ma %>% ungroup() %>% filter(AIMS_REEF_NAME==reef) %>% droplevels

print(reef)
count=count+1
print(count)

#***************************************
#create dataframe for model predictions*
#***************************************

newdata.reef <- reef.data %>% 
        ungroup() %>%
        dplyr::select(REPORT_YEAR) %>%
        distinct() %>%
        mutate(n.points=NA,
               total.points=1,
               SITE_NO=NA,
               TRANSECT_NO=NA,
               month=NA) 

dat.reef <- reef.data %>% ungroup %>%
        dplyr::select(n.points, total.points, REPORT_YEAR,
                      SITE_NO, TRANSECT_NO, month) %>%
        bind_rows(newdata.reef) %>% mutate(Obs = factor(1:n()),
                                           x.points = total.points-n.points)

newdata.index.reef <- 1:nrow(newdata.reef) + nrow(reef.data) 

#********************************************************************************

newdata <- dat.reef[newdata.index.reef,]
Xmat <- model.matrix(~REPORT_YEAR, data=newdata)
lincomb <- inla.make.lincombs(as.data.frame(Xmat))


#**********
#Run Model*
#**********

#fit the model
set.seed(123)
tempMA.reef.inla <- inla(n.points ~
                                   1+REPORT_YEAR +
                                   f(SITE_NO, model = 'iid') +
                                   f(TRANSECT_NO, model = 'iid')+
                                   f(month, model = 'iid')+
                                   f(Obs, model = 'iid',
                                     hyper = list(theta = list(prior = 'loggamma',
                                                               param = c(0.001, 0.1)))),
                           data = dat.reef,
                           Ntrials = dat.reef$total.points,
                           family = 'binomial',
                           control.predictor = list(compute = TRUE, link = 1),
                           control.fixed = list(mean = 0, prec = 0.001,
                                                mean.intercept = 0, prec.intercept = 0.01),
                           lincomb = lincomb,
                         control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE, mlik=TRUE)
)


#*******************
# Model Validation *
#*******************

draws <- inla.posterior.sample(1000, result=tempMA.reef.inla, seed=123) %>% suppressWarnings()
##Rearrange draws so that it is a matrix of cellmeans, rather than a list
cellmeans = sapply(draws, function(x) x[['latent']])
##Index the cell means for fixed effects
i.mod <- sapply(c('APredictor','^Predictor','^REPORT_YEAR[0-9]{4}:1$',
                  'SITE_NO','Intercept', 'month',
                  'TRANSECT_NO', 'Obs'),
                function(x) grep(x, draws[[1]]$latent %>% rownames))

##Get the predictions for fixed effects.
##Generate model matrix
Xmat <- model.matrix(~1+REPORT_YEAR, data = reef.data)
wch <- c(grep('Intercept', names(i.mod)),
         grep('^\\^REPORT_YEAR\\[0-9\\]\\{4\\}:1', names(i.mod)))
ii = unlist(i.mod[wch])

##multiply the predictions by the fixed effects for the covariates
cellmeans.full.1 <- t(cellmeans[ii,]) %*% t(Xmat)
cellmeans.full.1 %>% colMeans() %>% head

## hyperparameters
hyperpars = matrix(sapply(draws, function(x) x[['hyperpar']]), ncol = 1000)

##Index the hyperpars
j.mod <- sapply(c('SITE_NO','TRANSECT_NO', 'month', 'Obs'),
        function(x) grep(x, draws[[1]]$hyperpar %>% names))
sd <- sqrt(1/hyperpars[j.mod,])

## Predictions, accounting for distributions of random effects
pred.inla <- plogis(cellmeans.full.1 +
                            rnorm(nrow(reef.data), 0, sd[1,]) +
                            rnorm(nrow(reef.data), 0, sd[2,]) +
                            rnorm(nrow(reef.data), 0, sd[3,]) +
                            rnorm(nrow(reef.data), 0, sd[4,])) 

#Simulate Data
sim <- apply(pred.inla, 1, function(x) rbinom(nrow(reef.data), size = reef.data$total.points, x))

#Assess residuals
mod.resids <- createDHARMa(
        simulatedResponse = sim,
        observedResponse = reef.data$n.points,
        fittedPredictedResponse = apply(plogis(cellmeans.full.1), 2, median),
        integerResponse = TRUE
)

#png(paste0(LTMP_MODEL_PATH, 'reef_resids/all/tempMA_', reef, '_inla_resids.', iter, '.png'), width = 1152, height = 765)
png(paste0(FINAL_LTMP_PATH, 'reef_resids/all/tempMA_', reef, '_inla_resids.', iter, '.png'), width = 1152, height = 765)
tempMA.reef.inla.resids<- mod.resids %>% plot()
dev.off()


#****************
# Model Summary *
#****************

model.summary.reef<-summary(tempMA.reef.inla)

modelsummary.list[[reef]]=model.summary.reef


#****************
# Fixed Effects *
#****************

# #Export fixed effects summary
 ma.inla.fixed.reef<- as.data.frame(tempMA.reef.inla$summary.fixed)

fixedeffects.list[[reef]]=ma.inla.fixed.reef %>%
        mutate(AIMS_REEF_NAME=reef) 

#******************
# Hyperparameters *
#******************
#
# #Export hyperparameters summary
 ma.inla.hyperpar.reef<- as.data.frame(tempMA.reef.inla$summary.hyperpar) 

hyperparameters.list[[reef]]= ma.inla.hyperpar.reef %>%
        mutate(AIMS_REEF_NAME=reef)

#******************
#Model Predictions*
#******************

#Model predictions
tempMA.reef.inla.cellmeans <- tempMA.reef.inla$summary.lincomb.derived %>%
        as.data.frame() %>%
        select(mean,`0.025quant`, `0.975quant`) %>%
        mutate(across(everything(), plogis)) %>%
        bind_cols(dat.reef[newdata.index.reef,])

predictions.list[[reef]]=tempMA.reef.inla.cellmeans  %>% 
        mutate(AIMS_REEF_NAME=reef)

}

tempMA.reef.models<-do.call('list', modelsummary.list)
#save(tempMA.reef.models, file = paste0(LTMP_MODEL_PATH, 'tempMA.reef.models.', iter, '.RData'))
save(tempMA.reef.models, file = paste0(FINAL_LTMP_PATH, 'tempMA.reef.models.', iter, '.RData'))

tempMA.reef.fixeffs<-do.call('rbind', fixedeffects.list) %>%
         mutate(AIMS_REEF_NAME=as.factor(AIMS_REEF_NAME))
save(tempMA.reef.fixeffs, file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.fixeffs.', iter, '.RData'))
#write.csv(tempMA.reef.fixeffs, file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.fixeffs.', iter, '.csv'))
write.csv(tempMA.reef.fixeffs, file=paste0(FINAL_LTMP_PATH, 'tempMA.reef.fixeffs.', iter, '.csv'))


tempMA.reef.hyperparameters<-do.call('rbind', hyperparameters.list) %>% 
        mutate(AIMS_REEF_NAME=as.factor(AIMS_REEF_NAME))
save(tempMA.reef.hyperparameters, file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.hyperparameters.', iter, '.RData'))
#write.csv(tempMA.reef.hyperparameters, file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.hyperparameters.', iter, '.csv'))
write.csv(tempMA.reef.hyperparameters, file=paste0(FINAL_LTMP_PATH, 'tempMA.reef.hyperparameters.', iter, '.csv'))

tempMA.reef.predictions<-do.call('rbind', predictions.list) %>% 
        mutate(AIMS_REEF_NAME=as.factor(AIMS_REEF_NAME)) %>%
        left_join(ma %>% ungroup() %>% dplyr::select(AIMS_REEF_NAME, REPORT_YEAR,
                                                     A_SECTOR, SHELF) %>% distinct)
save(tempMA.reef.predictions, file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.predictions.', iter, '.RData'))

#***********************************************
#Plot temporal macroalgae trends for all reefs *
#***********************************************

load(file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.predictions.', iter, '.RData'))

##Confidence bands
ma.ltmp.reef<-ggplot(tempMA.reef.predictions, 
                     aes(y=`mean`, 
                         x=as.numeric(as.character(REPORT_YEAR)), color=AIMS_REEF_NAME)) +
        geom_pointrange(aes(ymin=`0.025quant`, ymax=`0.975quant`,
                            x=as.numeric(as.character(REPORT_YEAR)),
                            fill=AIMS_REEF_NAME), alpha=0.3) +
        geom_line(aes(color=AIMS_REEF_NAME)) +
        scale_y_continuous('Macroalgae cover (%)', 
                           labels=function(x) x*100, limits=c(0,1)) +
        theme_classic() +
        theme(  axis.line = element_line(),
                axis.title.x=element_blank(),
                axis.title.y=element_text(size=15),
                legend.title = element_blank(),
                axis.text.x=element_text(size=15),
                axis.text.y=element_text(size=15),
                legend.text = element_text(size=5),
                legend.position = "none",  
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank())

X11()
ma.ltmp.reef


#****************************************
#Investigate wide Uncertainty Intervals *
#****************************************

load(file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.predictions.', iter, '.RData'))
head(tempMA.reef.predictions)

(wide.UIs<- tempMA.reef.predictions %>% 
        arrange(-`0.975quant`) %>% head)

load(file=paste0(LTMP_PROCESSED_PATH, 'ma.', iter, '.RData'))

sumMA.acrossTransects<- ma %>% group_by(AIMS_REEF_NAME, REPORT_YEAR) %>%
        summarise(n.points.sum=sum(n.points))

(zeroMA.reefyears<- sumMA.acrossTransects %>% filter(n.points.sum==0) %>% droplevels)


#Samples with wide uncertainty intervals are 7/8 of the samples where no macroalgae was recorded on any transect in that year
#The INLA method has had difficulty predicting where variation is = 0
#Predictions for these samples will be replaced with NAs and described as such in the supplementary materials

#*************************************************
#Exclude samples with wide Uncertainty Intervals *
#*************************************************

#Set predictions for samples with no variation (macroalgae = 0 for all transects) to NA

load(file=paste0(LTMP_MODEL_PATH, 'tempMA.reef.predictions.', iter, '.RData'))
glimpse(tempMA.reef.predictions)

predictions.with.NAs.lincomb<- tempMA.reef.predictions %>%
        mutate(mean=ifelse(`0.975quant`>0.9, NA, mean),
               `0.025quant`=ifelse(`0.975quant`>0.9, NA, `0.025quant`),
               `0.975quant`=ifelse(`0.975quant`>0.9, NA, `0.975quant`),
               total.points=ifelse(`0.975quant`>0.9, NA, total.points))

save(predictions.with.NAs.lincomb, file=paste0(LTMP_MODEL_PATH, 'predictions.with.NAs.lincomb.', iter, '.RData'))

#**************************************************************
#Subset by reefs with high macroalgae for plotting aesthetics *
#**************************************************************

load(file=paste0(LTMP_MODEL_PATH, 'predictions.with.NAs.lincomb.', iter, '.RData'))
glimpse(predictions.with.NAs.lincomb)

#Remove Reefs where mean macroalgae cover never exceeded 14% in any one year, to declutter the Reef level plot
reefs.sort.temporal.ma.means<- predictions.with.NAs.lincomb %>% 
        group_by(AIMS_REEF_NAME) %>%
        filter(mean==max(mean)) %>% droplevels %>% arrange(-mean)

reefs.highest.temporal.ma.means<- reefs.sort.temporal.ma.means %>% 
        filter(mean>=0.14) %>% droplevels %>% 
        pull(AIMS_REEF_NAME) %>% unique %>% as.data.frame() %>%
        rename(AIMS_REEF_NAME = ".")

predictions.reef.high.ma.lincomb<- predictions.with.NAs.lincomb %>% 
        filter(AIMS_REEF_NAME %in% c("Havannah Island",
                                     "Farquarson Reef",
                                     "Green Island",
                                     "Davies Reef","Reef 21-550",
                                     "Reef 21-529","Low Isles", "Centipede Reef",
                                     "Gannet Cay","Reef 20-104","Hyde Reef",
                                     "Reef 21-064"
                                     )) %>%  #"Lady Musgrave Island", "Rib Reef", "Peart Reef"
        droplevels

levels(predictions.reef.high.ma.lincomb$AIMS_REEF_NAME)

#save(predictions.reef.high.ma.lincomb, file=paste0(LTMP_MODEL_PATH, 'predictions.reef.high.ma.lincomb.', iter, '.RData'))
save(predictions.reef.high.ma.lincomb, file=paste0(FINAL_LTMP_PATH, 'predictions.reef.high.ma.lincomb.', iter, '.RData'))


#*******************************************************
#Plot temporal macroalgae trends for a subset of reefs *
#*******************************************************

##PDF and .png version

load(file=paste0(FINAL_LTMP_PATH, 'predictions.reef.high.ma.lincomb.', iter, '.RData'))

predictions.reef.high.ma.lincomb$AIMS_REEF_NAME<- fct_relevel(predictions.reef.high.ma.lincomb$AIMS_REEF_NAME, 
                                        "Green Island", "Low Isles", "Havannah Island",  
                                        "Farquarson Reef","Centipede Reef", #"Peart Reef",
                                        "Davies Reef", #"Rib Reef",
                                        "Reef 20-104", "Reef 21-064","Reef 21-529", "Gannet Cay","Reef 21-550",
                                        "Hyde Reef") #, "Lady Musgrave Island")


reefPalette=c("deepskyblue1", "mediumblue","deeppink2", 
              "purple1","purple2",
              "red", #"red1,
              "grey5","grey1","grey2", "grey3", "grey4", 
              "darkorange2") #, "orange")

##Confidence bands
plot.tempMA.reef.lincombs <- ggplot(predictions.reef.high.ma.lincomb, 
                     aes(y=`mean`, 
                         x=as.numeric(as.character(REPORT_YEAR)), color=AIMS_REEF_NAME)) +
        scale_colour_manual(values=reefPalette) + 
        geom_pointrange(aes(ymin=`0.025quant`, ymax=`0.975quant`,
                            x=as.numeric(as.character(REPORT_YEAR)),
                            fill=AIMS_REEF_NAME), alpha=0.3) +
        geom_line(aes(color=AIMS_REEF_NAME)) +
        scale_y_continuous('Macroalgae cover (%)', 
                           labels=function(x) x*100, limits=c(0,1)) +
        theme_classic() +
        geom_text(x=1995, y=1, label="b)", colour="black", size=5) +
        theme(  axis.line = element_line(),
                axis.title.x=element_blank(),
                axis.title.y=element_text(size=10, family = "sans"),
                axis.text.x=element_text(size=10, family = "sans"),
                axis.text.y=element_text(size=10, family = "sans"),
                legend.position = "none",  
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank()) 
 
plot.tempMA.reef.lincombs

#ggsave(plot.tempMA.reef.lincombs, file=paste0(LTMP_FIGURES_PATH, 'plot.tempMA.reef.lincombs.', iter, '.png'), width =18, height = 9, units="cm" )
#ggsave(plot.tempMA.reef.lincombs, file=paste0(LTMP_FIGURES_PATH, 'plot.tempMA.reef.lincombs.', iter, '.eps'), width =18, height = 9, units="cm" )

ggsave(plot.tempMA.reef.lincombs, file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.reef.lincombs.', iter, '.png'), width =7.5, height = 4, units="in" )
save(plot.tempMA.reef.lincombs, file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.reef.lincombs.', iter, '.RData'))
ggsave(plot.tempMA.reef.lincombs, file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.reef.lincombs.', iter, '.pdf'), width =7.5, height = 4, units="in" )
ggsave(plot.tempMA.reef.lincombs, file=paste0(FINAL_LTMP_PATH, 'Fig7b.', iter, '.tiff'), width =7.5, height = 4, units="in" )


##EPS version
#Doesn't support specification of font or semi-transparency

load(file=paste0(FINAL_LTMP_PATH, 'predictions.reef.high.ma.lincomb.', iter, '.RData'))

predictions.reef.high.ma.lincomb$AIMS_REEF_NAME<- fct_relevel(predictions.reef.high.ma.lincomb$AIMS_REEF_NAME, 
                                                              "Green Island", "Low Isles", "Havannah Island",  
                                                              "Farquarson Reef","Centipede Reef", #"Peart Reef",
                                                              "Davies Reef", #"Rib Reef",
                                                              "Reef 20-104", "Reef 21-064","Reef 21-529", "Gannet Cay","Reef 21-550",
                                                              "Hyde Reef") #, "Lady Musgrave Island")


reefPalette=c("deepskyblue1", "mediumblue","deeppink2", 
              "purple1","purple2",
              "red", #"red1,
              "grey5","grey1","grey2", "grey3", "grey4", 
              "darkorange2") #, "orange")

##Confidence bands
plot.tempMA.reef.lincombs <- ggplot(predictions.reef.high.ma.lincomb, 
                                    aes(y=`mean`, 
                                        x=as.numeric(as.character(REPORT_YEAR)), color=AIMS_REEF_NAME)) +
        scale_colour_manual(values=reefPalette) + 
        geom_pointrange(aes(ymin=`0.025quant`, ymax=`0.975quant`,
                            x=as.numeric(as.character(REPORT_YEAR)),
                            fill=AIMS_REEF_NAME)) +
        geom_line(aes(color=AIMS_REEF_NAME)) +
        scale_y_continuous('Macroalgae cover (%)', 
                           labels=function(x) x*100, limits=c(0,1)) +
        theme_classic() +
        geom_text(x=1995, y=1, label="b)", colour="black", size=5) +
        theme(  axis.line = element_line(),
                axis.title.x=element_blank(),
                axis.title.y=element_text(size=10),
                axis.text.x=element_text(size=10),
                axis.text.y=element_text(size=10),
                legend.position = "none",  
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank()) 

plot.tempMA.reef.lincombs

save(plot.tempMA.reef.lincombs, file=paste0(FINAL_LTMP_PATH, 'Fig7b.', iter, '.RData'))
ggsave(plot.tempMA.reef.lincombs, file=paste0(FINAL_LTMP_PATH, 'Fig7b.', iter, '.eps'), width =7.5, height = 4, units="in")

#************************
# some stats to report
#************************
load(file=paste0(FINAL_LTMP_PATH, 'predictions.reef.high.ma.lincomb.', iter, '.RData'))

havannah<- predictions.reef.high.ma.lincomb %>% filter(AIMS_REEF_NAME=="Havannah Island") %>% droplevels

(hav.tem.mean<- havannah %>% summarise(temp.mean=mean(mean)))


################################################################################
#Figure 7 panelled
################################################################################

load(file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.shelf.lincombs.', iter, '.RData'))
load(file=paste0(FINAL_LTMP_PATH, 'plot.tempMA.reef.lincombs.', iter, '.RData'))

figure7<-grid.arrange(plot.tempMA.shelf.lincombs, plot.tempMA.reef.lincombs, nrow=2)

ggsave(figure7, file=paste0(FINAL_LTMP_PATH, 'Fig7.png'), width =7.5, height = 6, units="in" )
save(figure7, file=paste0(FINAL_LTMP_PATH, 'figure7.noFontSpecs.', iter, '.RData'))
ggsave(figure7, file=paste0(FINAL_LTMP_PATH, 'Fig7.pdf'), width =7.5, height = 6, units="in" )
ggsave(figure7, file=paste0(FINAL_LTMP_PATH, 'Fig7.tiff'), width =7.5, height = 6, units="in" )


#EPS

load(file=paste0(FINAL_LTMP_PATH, 'Fig7a.', iter, '.RData'))
load(file=paste0(FINAL_LTMP_PATH, 'Fig7b.', iter, '.RData'))

figure7.eps<- grid.arrange(plot.tempMA.shelf.lincombs, plot.tempMA.reef.lincombs, nrow=2) 
save(figure7.eps, file=paste0(FINAL_LTMP_PATH, 'Fig7.RData'))
ggsave(figure7.eps, file=paste0(FINAL_LTMP_PATH, 'Fig7.eps'), width =7.5, height = 4, units="in" )
