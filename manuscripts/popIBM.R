# popIBM model - cleaned for publication in Communications in medicine titled:
# "Digital twin simulation modelling shows that 
# mass testing and local lockdowns effectively controlled COVID-19 in Denmark"
# further commented for submission to ECDC

# This is a detailed, individual-based simulation model of SARS-CoV-2 transmission 
# to evaluate mass testing and local lockdowns during the Alpha wave in Denmark 
# in counterfactual scenarios. 

# Denmark tested ten times more than median country in EU during alpha wave
# local lockdown was based on observed 7 day incidence in parish/municipality

# please note that the model is coded using data.table
# this has a specific syntax, please see: r-datatable.com for an introduction

library(doParallel)
library(data.table)

###########################################################
#### Parameters that can be sensibly changed by user ####
##########################################################

# set working directory
# setwd("/ngc/projects/ssi_mg/kaagra/popIBM/nTestEffect")

# number of repetitions - 100 in the publication
nSamples <- 1 

# are automatic lockdowns activated in the model
activateLockdown <- TRUE 

# fraction of tests available compared to observed, 1=mass test, .1=limited test
fracNtests <- .1         

# choose number of cores to be used by do par
use.cores = 1 

# choose number of threads to be used by data.table, likely do not work well with doParallel
setDTthreads(1) 

# Print which elements are loaded
load.info <- FALSE 

# the model cannot run without the input of data. - not submitted
# many parameters enter through this file , and also the basic structure of the individuals
# load("./popIBMinit.Rdata")

# dates
newendtimes <- as.Date("2021-06-30")
endTimes <- as.numeric(newendtimes)-as.numeric(as.Date("2020-01-01"))
times <- seq(startDK,endTimes,1)
xdates <- as.Date(times,origin="2020-01-01")

# Proportion of transmission within municipality
w.kom <- .9

# Seasonal factor (relative to estimate from Sweden)
season.fac <- .8 

##############################################################
#### parameters beyond this point should NOT be changed #######
##############################################################

# vaccination groups to output
n.vac.gr.out <- 3 
# days after vaccination groups are divided into
br.vac.out <- c(14, 14+28, Inf) 

# data collection arrays
sim.tp2 <- array(0L, dim = c(length(times), n.ageGr, n.variants ))#, n.rep))
sim.hos <-  array(0L, dim = c(length(times), n.ageGr, n.variants))#, n.rep))

# data collection arrays that include vaccination status
sim.tp2.vac <- array(0L,dim = c(length(times),n.ageGr,n.variants,n.vac.gr.out))
sim.hos.vac <-  array(0,dim = c(length(times),n.ageGr,n.variants,n.vac.gr.out))

# individuals are stored in the data.table called 'ibm'
# a data.table is equivalent to a data.frame but has additional functionality
# each line in the data.table i equivalent to a person

# number of parishes (sogn) in the input
n.sogn <- ibm[,uniqueN(sognekode)]
# number of municipalities (kommune) in the input
n.kommuner <- ibm[,uniqueN(kommunekode)]

# data collection arrays
sim.sogn <- array(0L, dim=c(length(times),n.sogn))
sim.kom <- array(0L, dim=c(length(times),n.kommuner))

# IDs of the parish and municipalities
u.sognekoder <- ibm[,unique(sognekode)]
u.kommunekoder <- ibm[,unique(kommunekode)]

# the population by parish and municipality - .N is data.table special character
popSognKom <- ibm[,.N, keyby = .(sognekode, kommunekode)]

# the population by parish and municipality from alternative sources
pop.sogn <- sogn[.(sognekode=u.sognekoder),`Indbyggertal i sogn`,on="sognekode"]
tmp <- sogn[,sum(`Indbyggertal i sogn`),by=.(kommunekode)]
pop.kommune <- tmp[.(kommunekode=u.kommunekoder),V1,on="kommunekode"]
pop.dk <- NROW(ibm)
pop.age <- ibm[,.N,by=.(ageGrp)]

dt.pop.kom <- sogn[,sum(`Indbyggertal i sogn`),by=.(kommunekode)]
names(dt.pop.kom)[2] <- "pop"

mfka <- data.table(kommunekode=rep(u.kommunekoder,each=9),ageGrp=1:9)

# dates of changing restriction
daycha   <- as.numeric(c(LDSceBetaList$Fyn$S5.3$listBetaDates) - as.Date("2020-01-01")) - startDK

# list of activity matrices - age stratified
listbeta <- LDSceBetaList$Fyn$S5.3$listBeta

# dates of changing incidence limits for imposing local lockdown
daylockVec <- as.numeric(as.Date(c("2021-03-01", "2021-04-30", "2021-05-28", "2021-07-16", "2021-09-10", "2021-11-15")) - as.Date("2020-01-01")) - startDK

# functions for lockdown:
ld.sogn.fun <- list(approxfun(x=c(300, 400), y=c(0, 0.5), yleft = 0, yright = 1),
                    approxfun(x=c(375, 500), y=c(0, 0.5), yleft = 0, yright = 1),
                    approxfun(x=c(450, 600), y=c(0, 0.5), yleft = 0, yright = 1),
                    approxfun(x=c(750, 1000), y=c(0, 0.5), yleft = 0, yright = 1),
                    approxfun(x=c(1000, 4000), y=c(0, 0.2), yleft = 0, yright = 0.5),
                    approxfun(x=c(800, 3200), y=c(0, 0.2), yleft = 0, yright = 0.5))

ld.kom.fun <- list(approxfun(x=c(150, 200), y=c(0, 0.5), yleft = 0, yright = 1),
                   approxfun(x=c(188, 250), y=c(0, 0.5), yleft = 0, yright = 1),
                   approxfun(x=c(225, 300), y=c(0, 0.5), yleft = 0, yright = 1),
                   approxfun(x=c(375, 500), y=c(0, 0.5), yleft = 0, yright = 1),
                   approxfun(x=c(500, 2000), y=c(0, 0.2), yleft = 0, yright = 0.5),
                   approxfun(x=c(400, 1600), y=c(0, 0.2), yleft = 0, yright = 0.5))

# Automatic for last date i data (NTAL)
dayFixPTest <- as.numeric(NTAL[, as.Date(max(PrDate))] - as.Date("2020-01-01")) - startDK  #Update

# 
redvacFacTrans <- ifelse(exists("inputRedvacFacTrans"),inputRedvacFacTrans,0.1) # reduction factor on transmission when effectively vaccinated # .1 / .2 / .3
redprobHosp <- 0.25 # reduction factor on risk going to hospital when effectively vaccinated

# introducing delta variant in simulation
dayDeltaIntroSce <- as.numeric(as.Date("2021-06-01") - as.Date("2020-01-01")) - startDK
probDeltaIntro <- 0.02 # Converting X% of infected to delta variant on this day
variantIDDelta <- 3 # Variant ID for delta

SceFacCurBeta <- 1 # should be 1.05 if 5% increase, 0.95 if 5% decrease
SceTestRed <- 1 # Factor for probability of taking a test
testRedFac <- 1 # Internal copy of ScceTest,Red when paste date

registerDoParallel(cores = use.cores)

SceFacCurBetaVec <- ifelse(exists("inputFacBeta"), inputFacBeta, c(1)) # Update if given as input

# maximal number of vaccination doses in the simulation - depend on endtime
nMaxDoses <- 3

# Set seed for generating parameter combinations
set.seed( ifelse(exists("inputSeed"),inputSeed, 1 )) 

# this simulation was implemented at a time with uncertainties on delta variant parameters
# therefore scenarios of different parameter values for delta are included
sceCombi <- data.frame(parID = 1:nSamples,
                       SceFacCurBeta = SceFacCurBetaVec,
                       #irep = 1:nSamples, # Should be made obsolete
                       DeltaRecred = 1- runif(nSamples, min = 0.6, max = 0.8), # VE of infection
                       redvacFacTrans = 1- runif(nSamples, min = 0.5, max = 0.8), # Transmission
                       relAlphaDelta = runif(nSamples, min = 1.65, max = 1.95)
)

firstRun <- ifelse(exists("inputStart"),inputStart, 1 )
nRuns <- ifelse(exists("inputNRuns"),inputNRuns, nSamples ) # Run all if not specified


tic <- Sys.time()

# branch out to parrallel processes
sim.list <- foreach(runThis = (firstRun - 1 + 1:nRuns), .packages = c("data.table"), .verbose = TRUE) %dopar%{
  tmp <- unlist(sceCombi[runThis,])
  for (i in 1: length(tmp)){
    assign( x= names(tmp)[i], value = tmp[i])
  }
  cat("\t run: ",runThis," ")
  # start and end times are in init file
  delta.redprobHosp <- (1 - 0.9) * redprobHosp/DeltaRecred*2
  delta.vacEffect <- DeltaRecred*c(1,1)
  
  rm("ibm")
  # make sure parameters are loaded fresh onto all cores
  load("./popIBMinit.Rdata")
  
  endTimes <- as.numeric(newendtimes)-as.numeric(as.Date("2020-01-01"))
  times <- seq(startDK,endTimes,1)
  xdates <- as.Date(times,origin="2020-01-01")
  
  # intialise spatial heterogeneity in parishes
  ibm[,lockdownfac := 1.]
  ibm[,relrisksogn := relrisksogn^(1/3)]
  ibm[,relrisksogn := relrisksogn*.N/sum(relrisksogn)]
  
  # set initial parameters, that will change over time
  ibm[,nonIso := 1L] 
  ibm[,pTest := 2e5/pop.dk] 
  
  # make sure individual already vaccinated are correctly labelled
  ibm[,vacFacTrans := 1.0]
  ibm[vacFac<1 , vacFacTrans := redvacFacTrans]
  
  ibm[,vacEffDose := 0L]
  ibm[vacTime>14,vacEffDose := 1L]
  ibm[vacTime>(14+28),vacEffDose := 2L]
  
  # Setting seed per rep
  set.seed(123456 + runThis - 1)
  
  # change some params
  v.relBeta <- c(1,1.55,1.55*relAlphaDelta) 
  v.scaleI <- rep(5.3,9)/v.shapeI
  ibm[disease==2L, tt:=tt+round(rexp(.N,1/2))]
  
  # get history of incidence in parish/municipality
  inc.his.sogn <- array(0, dim=c(length(times),n.sogn))
  inc.his.kom <- array(0, dim=c(length(times),n.kommuner))
  
  # index individuals for faster runtime
  setkey(ibm, kommunekode, sognekode, ageGrp, VacMaalGr)
  
  # reference transmission risk scaling - fitted prior
  Rref <- 0.7

  # profiler for testing bottlenecks in code - only for test runs
  # profvis({
  
  # time loop
  for (day in 1:length(times))
  {
    
    # set 'beta' based on restriction levels and seasonal change
    if (day>daycha[1]){
      i.beta <- max(which(daycha<=day))
      curbeta <- (1-season.fac*(1-
                                  SeasonalRelBeta(as.Date(startDK,origin = "2020-01-01"),day)))*
        Rref*.35*listbeta[[i.beta]]
      lockdownfactor <- sqrt(eigen(listbeta[[1]])$values[1]/eigen(listbeta[[i.beta]])$values[1])
    } else {
      curbeta <-  (1-season.fac*(1-
                                   SeasonalRelBeta(as.Date(startDK,origin = "2020-01-01"),day)))*
        Rref*.35*listbeta[[1]]
      
    }
    
    
    ## Change some to delta variant
    if (day == dayDeltaIntroSce & probDeltaIntro>0){
      ibm[variant==2,variant := sample(c(2,variantIDDelta),size = .N, replace = TRUE, prob=c(1-probDeltaIntro,probDeltaIntro))]
    }
    
    # make pTest ~ 7 day incidense
    nTest <- nTestDK(as.Date(startDK,origin = "2020-01-01"),day)
    nTestAge <- nTestDKAge(as.Date(startDK,origin = "2020-01-01"),day)
    nTestAgeVac <- nTestDKAgeVac(as.Date(startDK,origin = "2020-01-01"),day)
    #if (day> dayCorPasEnd) nTestAge <- nTestDKAge(as.Date(startDK,origin = "2020-01-01"),day,0)
    
    # adjust number of test according to scenario
    nTest <- nTest * fracNtests
    nTestAge$wtest <- nTestAge$wtest * fracNtests
    nTestAgeVac$wtest <- nTestAgeVac$wtest * fracNtests
    
    # when incidences are available, adjust test behaviour according to incidence
    if (day>7){
      inc <- colSums(sim.kom[(day-7):(day-1),],na.rm=TRUE) / pop.kommune *1e5 #LAEC2: not including today
      pTestcor <- pTestInc(inc)

      if (day <= dayFixPTest) {
        # should maybe be done per kommune
        tmp <- ibm[,.N,keyby=.(ageGrp,!(vacTime<14 | is.na(vacTime)))]
        Tpop.ageVac <- tmp[nTestAgeGrIntVac,,on=c("ageGrp","vacTime")]
        Tpop.ageVac[is.na(N),N:=0]
        
        tmp <- ibm[,.(pop=.N),keyby=.(kommunekode,ageGrp,!(vacTime<14 | is.na(vacTime)))]
        tmp <- tmp[Tpop.ageVac,,on=c("ageGrp","vacTime")]
        names(tmp)[c(3,5)] <- c("vacStatus","Tpop")
        names(nTestAgeVac)[1] <- "ageGrp"
        nTestAgeVac$ageGrp <- as.integer(nTestAgeVac$ageGrp)
        
        #tmp[nTestAgeVac,,on=c("ageGrp","vacStatus")]
        
        tmpTest <- tmp[nTestAgeVac,,on=c("ageGrp","vacStatus")]
        
        tmpTest[,pTestcor :=  wtest/Tpop ]
        
      }
      
      tmp2 <- data.table(kommunekode=u.kommunekoder,
                         pTestFac=pTestcor)
      
      tmp <- tmpTest[tmp2,,on=c("kommunekode")]
      
      tmp[,pTestcor :=  pTestcor * pTestFac]
      tmp[,pTestcor := pTestcor*sum(nTestAgeVac$wtest)/sum(pTestcor*pop)]
      
      #tmp[,sum(pTestcor*pop)]
      
      tmp[,vacEffDose := as.integer(vacStatus)]
      
      tmp2 <- copy(tmp[vacEffDose==1L,])
      tmp2[,vacEffDose := 2L]
      
      tmp <- rbindlist(list(tmp,tmp2))
      
      ibm[tmp,on=c("kommunekode","ageGrp","vacEffDose"),pTest:=pTestcor]
      
    } else {
      ibm[,pTest := (nTest[1,1]+0.5*nTest[2,1])/pop.dk * testRedFac]
    }
    
    # determine who is detected by tests
    id.tp <- ibm[nonIso==1L & (disease %in% c(1:2) | (disease==3L & tt>= -5)) & 
                   (is.na(vacType)  | vacTime < 14),
                 .(ID,pTest)][runif(.N) < pTest,ID] 
    ibm[ID %in% id.tp, ttSymp:=0L] # a little ugly but faster
    
    # collect data on the number of test positives each day - by variant, age and vaccination status
    for (k in 1:n.variants) {
      sim.tp2[day,,k] <- ibm[ttSymp==0L & variant==k,.N,
                             by=.(ageGrp)][.(ageGrp=1:9),on="ageGrp"]$N
      
      sim.tp2.vac[day,,k,1] <- ibm[ttSymp==0L & variant==k & 
                                     (vacTime < br.vac.out[1] | is.na(vacTime)),.N,
                                   by=.(ageGrp)][.(ageGrp=1:9),on="ageGrp"]$N
      for (kk in 2:n.vac.gr.out) { #LAEC: 1 stik for sig
        sim.tp2.vac[day,,k,kk] <- ibm[ttSymp==0L & variant==k & vacTime >= br.vac.out[kk-1] & vacTime < br.vac.out[kk],.N,
                                      by=.(ageGrp)][.(ageGrp=1:9),on="ageGrp"]$N
      }
    }
    
    # collect number of test positives by parish
    sim.sogn[day,] <- ibm[ttSymp==0L,.N,
                          by=.(sognekode, ageGrp)][,sum(N),by=sognekode][
                            .(sognekode=u.sognekoder),on="sognekode"]$V1
    
    # collect number of test positives by municipality
    sim.kom[day,] <- ibm[ttSymp==0L,.N,
                         by=.(kommunekode, ageGrp)][,sum(N),by=kommunekode][
                           .(kommunekode=u.kommunekoder),on="kommunekode"]$V1
    
    # test positives isolate themselves
    ibm[ttSymp==0L,nonIso:=0L]
    
    # collect probability of hospitalisation each day - by variant, age and vaccination status
    for (k in 1:n.variants) {
      sim.hos[day,,k] <- ibm[disease==2L & tt==0 & variant==k,sum(probHosp),
                             by=.(ageGrp)][.(ageGrp=1:9),on="ageGrp"]$V1
      
      sim.hos.vac[day,,k,1] <- ibm[disease==2L & tt==0 & variant==k & 
                                     (vacTime < br.vac.out[1] | is.na(vacTime)),
                                   sum(probHosp),
                                   by=.(ageGrp)][.(ageGrp=1:9),on="ageGrp"]$V1
      
      for (kk in 2:n.vac.gr.out) {
        sim.hos.vac[day,,k,kk] <- ibm[disease==2L & tt==0 & variant==k & vacTime >= br.vac.out[kk-1] & vacTime < br.vac.out[kk],
                                      sum(probHosp),
                                      by=.(ageGrp)][.(ageGrp=1:9),on="ageGrp"]$V1
      }
      
      
    }
    
    # recover from disease I -> R
    ibm[disease==2L & tt==0, disease := 3L]
    
    # E -> I
    # for (i in 1:n.ageGr)
    # {
    #   ibm[ageGrp==i & disease==1L & tt==0,':='(disease = 2L,
    #                                            tt = pmax(1L,round( rgamma( .N , v.shapeI[i] , 
    #                                                                        scale = v.scaleI[i]) )) )]
    # }
    
    # when all agegroups have same disease progression E-> I
    # also draw time to being symptomatic
    ibm[disease==1L & tt==0,':='(disease = 2L,
                                 tt = pmax(1L,round( rgamma( .N , v.shapeI[1] , 
                                                             scale = v.scaleI[1]) )),
                                 ttSymp = pmax(1L,round( rgamma( .N , v.shapettSymp[1] , 
                                                                 scale = v.scalettSymp[1]) )) * 
                                   sample(c(1L,NA_integer_),.N,replace = TRUE,prob=c(.5,.5)) )]
    
    # do not double count people found in E states
    ibm[disease==2L & nonIso==0L & ttSymp>0, ttSymp := -1L]
    
    
    # implement the effects of local lockdown
    if (activateLockdown) {
      
      # determine if there should be lockdown!
      if (day>daylockVec[1]){

        # lockdown
        i.lock <- sum(day > daylockVec)

        # parish (sogn)
        no.cases <- colSums(sim.sogn[(day-7):(day-1),],na.rm=TRUE) 

        inc.his.sogn[day,] <- (no.cases>=20) * no.cases / pop.sogn * 1e5 
        
        max7dInc <- apply(inc.his.sogn[(day-6):day,],2, max, na.rm = TRUE)
        ld.sogn.fac <- ld.sogn.fun[[i.lock]](max7dInc)
        
        # municipality (kommune)

        inc.his.kom[day,] <- colSums(sim.kom[(day-7):(day-1),],na.rm=TRUE) / pop.kommune * 1e5
        max7dInc <- apply(inc.his.kom[(day-6):day,],2, max, na.rm = TRUE)
        ld.kom.fac <- ld.kom.fun[[i.lock]](max7dInc)
        
        popSognKom[data.table(sognekode = u.sognekoder, ld.sogn.fac), on = "sognekode", sogn.fac := ld.sogn.fac]
        popSognKom[data.table(kommunekode = u.kommunekoder, ld.kom.fac), on = "kommunekode", kom.fac := ld.kom.fac]
        
        popSognKom[, ld.max := pmax(sogn.fac, kom.fac)]
        popSognKom[, ld.fac := 1* (1-ld.max) + lockdownfactor * ld.max] # weighted sum as lockdown factor
        
        # Merging on ibm:
        ibm[popSognKom, on=c("sognekode", "kommunekode"), lockdownfac := ld.fac]
        
      }
      
    }
    
    #Infected individuals with different strains
    for (k in 1:n.variants){
      # Calculate the infection pressure
      infperskom <- ibm[disease == 2L & variant == k, 
                        .(infpers=sum(lockdownfac*nonIso*vacFacTrans)),
                        by = .(kommunekode,ageGrp)][mfka, on=c("kommunekode","ageGrp")]
      infperskom[is.na(infpers),infpers:=0]
      infperskom[,infpers:=infpers*v.relBeta[k]]
      
      infpres <- infperskom[,curbeta%*%infpers,by=.(kommunekode)][,ageGrp:=rep(1:9,n.kommuner)]
      names(infpres)[2] <- "rinfkom"
      
      infpres <- dt.pop.kom[infpres,,on="kommunekode"]
      infpres[,rinfkom:=rinfkom/pop]
      
      infpers <- infperskom[,.(infpers=sum(infpers)),by=.(ageGrp)]
      
      tmp <- infpers$infpers
      infpresDK <- infpers[,.(rinfDK=sum( curbeta[ageGrp,]*tmp/pop.dk )),by=.(ageGrp)]
      
      infprestot <- infpresDK[infpres,,on="ageGrp"]
      infprestot[,probinf:=(1-exp(-(1-w.kom)*rinfDK - w.kom*rinfkom))]
      
      tmp2 <- infprestot[,c(1,3,6)]
      names(tmp2)[3] <- "probinfnew"
      
      # Evaluate probability of infection
      ibm[tmp2,on=c("kommunekode","ageGrp"),probinf:=probinfnew]

      # NB for future use - should maybe include a check on reasonable values [0;1]
      ibm[, probinf := probinf * vacFac * relrisksogn * lockdownfac ]
      
      # Randomly infect some individuals based on probability
      ibm[disease==0L & runif(.N) < probinf,
          ':='(disease = 1L,
               tt = pmax(1L,round(rgamma(n = .N, shape = v.shapeE[ageGrp],
                                         scale = v.scaleE[ageGrp]))),
               variant = k)]
    }
    
    # count down to change in disease state or symptoms
    ibm[,tt:=tt-1L]
    ibm[,ttSymp:=ttSymp-1L]
    
    # count up to vaccination time
    ibm[,vacTime:=vacTime+1L]
    
    
    # implement the effect of vaccination
    for (k in 1:n.Vac){
     
        ibm[vacType==k & vacTime==v.vacTTeffect[k],
            ':='(vacFac = delta.vacEffect[k],
                 probHosp = delta.redprobHosp * probHosp,
                 vacFacTrans = redvacFacTrans)] 
    
    }
    
    # vaccination doses takes effect - TODO move parameters to input file
    ibm[vacTime==15,vacEffDose := 1L]
    ibm[vacTime==(14+28+1),vacEffDose := 2L]
    
  }
  
  #})
  return(list(tp2 = sim.tp2[,,], tp2.vac = sim.tp2.vac[,,,], hos=sim.hos[,,], 
              hos.vac=sim.hos.vac[,,,], sogn = sim.sogn[,], kom = sim.kom[,], parID = parID)) 
}
gc()
toc <- Sys.time()

toc-tic

stopImplicitCluster() 

rm(ibm)
parString <- paste0("popIBM",substr(Sys.time(),1,10),"rep",nSamples,"perT",fracNtests*100)

# save output together with parameters
save.image(file=paste0("./",parString,".RData") )
