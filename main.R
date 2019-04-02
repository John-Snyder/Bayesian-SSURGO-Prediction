# sudo yum install libcurl-devel openssl-devel libxml2-devel udunits2-devel
#install.packages(c("httr","jsonlite"))

BartMemoryInGB <- 30
MemToJavaParam <- paste0("-Xmx",BartMemoryInGB,"g")
options(java.parameters = MemToJavaParam)
library(bartMachine) 
set_bart_machine_num_cores(30)

source("functions.R")
library(soilDB) 
library(dplyr)
library(ALEPlot)

YLD_Data <- read.csv("CornYLD_County_2015to2018.csv")
YLD_Data <- YLD_Data %>% filter(Year==2018, !grepl(pattern = "OTHER",YLD_Data$County))
CountyList <- paste0(YLD_Data$County," county, ",YLD_Data$State) %>% tolower

# N = NH4 + NO3
DBcols <- 
"compname,
taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp,taxmoistscl,
areatypename, areaname,
frag3to10_r, claytotal_r, om_r, caco3_r, gypsum_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_r, ph01mcacl2_r,
freeiron_r, feoxalate_r, extracid_r, extral_r, aloxalate_r, pbray1_r, ptotal_r, awc_r"

library(parallel)
start <- Sys.time()
cl <- makePSOCKcluster(30)
setDefaultCluster(cl)

clusterEvalQ(NULL, library(soilDB))
clusterEvalQ(NULL, library(dplyr))
clusterExport(NULL, c('YLD_Data','DBcols','CountyList','GetDatForCounty'))
dat2 <- parLapply(NULL, 1:nrow(YLD_Data), function(z) GetDatForCounty(z, YLD_Data=YLD_Data,DBcols=DBcols, CountyList = CountyList))
dat2 <- dat2[which(lapply(dat2,ncol) %>% unlist == max(lapply(dat2,ncol) %>% unlist))]
stopCluster(cl)

YLD_Data <- do.call("rbind", dat2) 
Sys.time() - start

YLD_Data %>% 
  group_by(State) %>% 
  summarise(Frequency = mean(ph1to1h2o_r)) %>% View~
#

YLD_Data <- apply(YLD_Data,2,function(x) as.numeric(as.character(x)))
keep <- !apply(YLD_Data,2,function(x) sum(is.na(x))>.5*length(x))
YLD_Data <- YLD_Data[,keep]
YLD_Data <- YLD_Data %>% data.frame %>% dplyr::select(-c("Year","State.ANSI","Ag.District.Code","County.ANSI","watershed_code" )) %>%
  filter(!is.na(Value))

X.BART <- YLD_Data  %>% dplyr::select(-"Value") %>%  select_if(is.numeric)
Y.BART <- YLD_Data  %>% dplyr::select("Value") %>% unlist

##build BART regression model
bart_machine = bartMachine(X.BART , Y.BART, num_trees = 50, num_burn_in = 1000,
                           num_iterations_after_burn_in = 3000, use_missing_data = TRUE)
summary(bart_machine)
investigate_var_importance(bart_machine)

#ARR <- bartMachineArr(bart_machine, R = 10)

X.ALE <- X.BART  %>% na.omit

X.ALE %>% names
X.ALE[,11] %>% hist
ALEPlot(X = X.ALE, X.model = bart_machine, pred.fun = PostPredFun, J=11, K = 100, NA.plot = TRUE)

aac <- bart_machine_get_posterior(bart_machine,new_data = X.ALE)$y_hat_posterior_samples

asd <- PostPredFun(bart_machine,X.ALE,1)


