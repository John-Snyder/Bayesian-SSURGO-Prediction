# sudo yum install libcurl-devel openssl-devel libxml2-devel udunits2-devel
#install.packages(c("httr","jsonlite"))

BartMemoryInGB <- 50
MemToJavaParam <- paste0("-Xmx",BartMemoryInGB,"g")
options(java.parameters = MemToJavaParam)
library(bartMachine) 
set_bart_machine_num_cores(30)
library(soilDB) 
library(dplyr)

YLD_Data <- read.csv("~/CornYLD_County_2015to2018.csv")
YLD_Data <- YLD_Data %>% filter(Year==2018, !grepl(pattern = "OTHER",YLD_Data$County))
CountyList <- paste0(YLD_Data$County," county, ",YLD_Data$State) %>% tolower

DBcols <- 
  "component.mukey, chorizon.cokey, chkey,comppct_r, compname,
taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp,taxmoistscl,
areatypename, areaname,
frag3to10_r, claytotal_r, om_r, caco3_r, gypsum_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_r, ph01mcacl2_r,
freeiron_r, feoxalate_r, extracid_r, extral_r, aloxalate_r, pbray1_r, ptotal_r, awc_r"

##
CovMat <- {}
numresp<- rep(0,length(CountyList))
for(i in 1:length(CountyList))
{
  q <- paste0("SELECT ",DBcols,
              " FROM legend
              INNER JOIN mapunit ON mapunit.lkey = legend.lkey
              LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
              LEFT JOIN chorizon ON chorizon.cokey = component.cokey
              WHERE legend.areaname LIKE '%",CountyList[i],"%' ;")
  res <- SDA_query(q)
  
  if(is.null(res))
  {
    q <- paste0("SELECT ",DBcols,
                " FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
                LEFT JOIN chorizon ON chorizon.cokey = component.cokey
                WHERE legend.areaname LIKE '%",YLD_Data$County[i],"%' AND legend.areaname LIKE '%",YLD_Data$State[i],"%';")
    res <- SDA_query(q)
  }
  
  if(is.null(res))
  {
    q <- paste0("SELECT ",DBcols,
                " FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
                LEFT JOIN chorizon ON chorizon.cokey = component.cokey
                WHERE legend.areaname LIKE '%",gsub(pattern = " ",replacement = "%",x = YLD_Data$County[i]),"%' AND legend.areaname LIKE '%",YLD_Data$State[i],"%';")
    res <- SDA_query(q)
  }
  
  if(!is.null(res))
  {
    YLD_Data$PropUDIC[i] <- mean(res$taxmoistscl == "Udic",na.rm=T)
    CovMat <- rbind(CovMat,res %>% select(-c( "mukey","cokey","chkey","comppct_r")) %>% select_if(is.numeric) %>% apply(2,median,na.rm=T))
  }else{
    YLD_Data$PropUDIC[i] <- NA
    CovMat <- rbind(CovMat,rep(NA,ncol(CovMat)))
  }
  
  print(sprintf("%s rows returned for %s (%s of %s)",ifelse(test = !is.null(res),nrow(res),0),CountyList[i],i,length(CountyList)))
  if(!is.null(res)) numresp[i] <- nrow(res)
}

YLD_Data <- cbind(YLD_Data,CovMat)

plot(YLD_Data$cec7_r,y=YLD_Data$Value,col=as.factor(YLD_Data$County))


X.fin <- YLD_Data %>% select(-c("Value")) %>% select_if(is.numeric)
Y.fin <- YLD_Data %>% select("Value") %>% unlist

##build BART regression model
bart_machine = bartMachine(X.fin , Y.fin, num_trees = 200, num_burn_in = 500,
                           num_iterations_after_burn_in = 1000, use_missing_data = TRUE)
summary(bart_machine)

## Not run: 
##Build another BART regression model
bart_machine = bartMachine(X,y, num_trees = 200, num_burn_in = 500,
                           num_iterations_after_burn_in = 1000)







