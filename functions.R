PostPredFun <- function(X.model,newdata,i)
{
  bart_machine_get_posterior(X.model,newdata)$y_hat
}


GetDatForCounty <- function(i,YLD_Data,DBcols,CountyList)
{
RetRow <- YLD_Data[i,]

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
  RetRow$PropUDIC <- mean(res$taxmoistscl == "Udic",na.rm=T)
  RetRow <- cbind(RetRow, res %>% apply(2,median,na.rm=T) %>% t %>% data.frame)
}else{
  RetRow <- rep(NA,36)
}

return(RetRow)
}




