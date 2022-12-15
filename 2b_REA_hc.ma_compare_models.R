################################################################################
#Compare to other model structures
################################################################################

## Mostly interested in whether there should be some version of latitudinal spatial structure in the model
## OR
## Whether the relationship should be fit with a polynomial rather than a linear relationship

#******************************************************************************
#Final model; Hc*Depth*Shelf
#******************************************************************************

load(file=paste0(FINAL_REA_PATH, 'depthshelf.inla.presence.RData'))

dic.final<- data.frame(model= "final",
                       dic=depthshelf.inla.presence$dic$dic,
                       waic=depthshelf.inla.presence$waic$waic)


#******************************************************************************
#Depth*Shelf + sector
#******************************************************************************

load(file=paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))
load(file=paste0(REA_PROCESSED_PATH, 'dat.beta.RData'))

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
mahc.present.addsector <- inla(Cover.ma ~ hc*depth*shelf  +  sector +
                                         f(reef, model='iid')+
                                         f(site, model='iid')+
                                         f(transect, model='iid'),
                                 data=dat.ma.present, 
                                 family="beta",
                                 control.predictor=list(compute=TRUE, link=1),
                                 control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))

dic.sector<- data.frame(model= "add.sector",
                        dic=mahc.present.addsector$dic$dic,
                        waic=mahc.present.addsector$waic$waic)

#******************************************************************************
#Depth*Shelf + latitude
#******************************************************************************

load(file=paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))
load(file=paste0(REA_PROCESSED_PATH, 'dat.beta.RData'))

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
mahc.present.addLat <- inla(Cover.ma ~ hc*depth*shelf  +  lat +
                                       f(reef, model='iid')+
                                       f(site, model='iid')+
                                       f(transect, model='iid'),
                               data=dat.ma.present, 
                               family="beta",
                               control.predictor=list(compute=TRUE, link=1),
                               control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))

dic.latitude<- data.frame(model= "add.latitude",
                        dic=mahc.present.addLat$dic$dic,
                        waic=mahc.present.addLat$waic$waic)

#******************************************************************************
#Depth*Shelf + region
#******************************************************************************

load(file=paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))
load(file=paste0(REA_PROCESSED_PATH, 'dat.beta.RData'))

dat.beta<- dat.beta %>% 
        mutate(region=as.factor(case_when(sector %in% c("CA","IN","TO","CU","WH")~"Central",
                                          sector %in% c("CG","PC","CL")~"North",
                                          sector %in% c("PO","SW","CB")~"South")))
levels(dat.beta$region)

dat.ma.present<- dat.ma.present %>%
        mutate(region=as.factor(case_when(sector %in% c("CA","IN","TO","CU","WH")~"Central",
                                          sector %in% c("CG","PC","CL")~"North",
                                          sector %in% c("PO","SW","CB")~"South")))
levels(dat.ma.present$region)


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
mahc.present.addregion <- inla(Cover.ma ~ hc*depth*shelf  +  region +
                                    f(reef, model='iid')+
                                    f(site, model='iid')+
                                    f(transect, model='iid'),
                            data=dat.ma.present, 
                            family="beta",
                            control.predictor=list(compute=TRUE, link=1),
                            control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))

dic.region<- data.frame(model= "add.region",
                          dic=mahc.present.addregion$dic$dic,
                          waic=mahc.present.addregion$waic$waic)




#***********************************************************
# with sector as an interacting predictor                  *
#macroalgae~ hc)*Depth*Shelf*sector                        *
#***********************************************************

load(file=paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))
load(file=paste0(REA_PROCESSED_PATH, 'dat.beta.RData'))

i.start <- 1 + dat.ma.present %>% nrow()
newdata.1 <- dat.beta %>%
        filter(Cover.ma>0) %>% droplevels() %>%
        group_by(sector, shelf, depth) %>%
        nest() %>%
        mutate(hc = map(.x = data, .f=function(x=.x) seq(min(x$hc), max(x$hc), length=100))) %>%
        #restrict predictions to actual range of observed HC
        unnest(hc) %>%
        dplyr::select(-data)

dat.ma.present <- dat.ma.present %>%
        bind_rows(newdata.1)
i.end <- dat.ma.present %>% nrow()

set.seed(123)
mahc.present.sector.interact <- inla(Cover.ma ~ hc*depth*shelf*sector +
                                       f(reef, model='iid')+
                                       f(site, model='iid')+
                                       f(transect, model='iid'),
                               data=dat.ma.present, 
                               family="beta",
                               control.predictor=list(compute=TRUE, link=1),
                               control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))

dic.sector.interact<- data.frame(model= "sector.interact",
                        dic=mahc.present.sector.interact$dic$dic,
                        waic=mahc.present.sector.interact$waic$waic)

#***********************************************************
# with sector as a random effect                           *
#macroalgae~ hc*Depth*Shelf                                *
#***********************************************************


load(file=paste0(REA_PROCESSED_PATH, 'dat.ma.present.RData'))
load(file=paste0(REA_PROCESSED_PATH, 'dat.beta.RData'))

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
mahc.present.sector.random <- inla(Cover.ma ~ hc*depth*shelf  +  
                                       f(sector, model='iid') +
                                       f(reef, model='iid')+
                                       f(site, model='iid')+
                                       f(transect, model='iid'),
                               data=dat.ma.present, 
                               family="beta",
                               control.predictor=list(compute=TRUE, link=1),
                               control.compute=list(config = TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))

dic.sector.random<- data.frame(model= "sector.random",
                        dic=mahc.present.sector.random$dic$dic,
                        waic=mahc.present.sector.random$waic$waic)


#******************************************************
# Compare models
#******************************************************

compare.models<- bind_rows(dic.final,
                           dic.sector,
                           dic.latitude,
                           dic.region,
                           dic.sector.interact,
                           dic.sector.random
                           ) %>% 
        arrange(waic) %>%
        mutate(waic.diff=waic-first(waic))

#******************************************************
# Conclusion
#******************************************************
##Based on DIC and WAIC, the final model, which did not have a spatial interacting variable or spatial covariate, was the best model structure