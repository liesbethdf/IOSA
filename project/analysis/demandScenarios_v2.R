############################################
############ Construction demand input vector for difference 2Deg & BAU CPI coal export projections
############################################

## Coal scenarios from CPI

df.scen <- df.CPI.total %>% filter(df.CPI.total$Unit %in% c("ZAR m, constant2018","mt","ZAR/t, constant2018"))

############ function for demand scenarios with coal demand from CPI, the others sectors 0

domesticDemand.coal.zeros <- function(df.scen, yearHere, market)
{
  df.dD              <- data.frame(Industry)
  colnames(df.dD)    <- "Industry"
  df.dD$SIC          <- SIC
  df.dD$Year         <- rep(yearHere,n)
  
  demandHere              <- rep(0,50)
  demandHere[4]           <- df.scen[df.scen$Year==yearHere & 
                                     df.scen$Case=="BAU" &
                                     df.scen$Var.type=="Volume" &
                                     df.scen$Market==market,"Value"]
  df.dD$Vol.demandD.BAU   <- diag(s.d.exp)   %*% demandHere
  df.dD$Vol.demandM.BAU   <- diag(s.m.exp)   %*% demandHere
  
  priceHere               <- rep(1,50)
  priceHere[4]            <- df.scen[df.scen$Year==yearHere & 
                                       df.scen$Case=="BAU" &
                                       df.scen$Var.type=="Price.c" &
                                       df.scen$Market==market,"Value"]
  df.dD$Price.e.BAU       <- priceHere

  priceHere               <- rep(1,50)
  priceHere[4]            <- df.scen[df.scen$Year==yearHere & 
                                     df.scen$Case=="BAU" &
                                     df.scen$Var.type=="Price.c" &
                                     df.scen$Market=="Domestic","Value"]
  df.dD$Price.d.BAU       <- priceHere
    
  demandHere              <- rep(0,50)
  demandHere[4]           <- df.scen[df.scen$Year==yearHere & 
                                       df.scen$Case=="2DS" &
                                       df.scen$Var.type=="Volume" &
                                       df.scen$Market==market,"Value"]
  df.dD$Vol.demandD.2DS   <- diag(s.d.exp)  %*% demandHere
  df.dD$Vol.demandM.2DS   <- diag(s.m.exp)  %*% demandHere
  
  priceHere               <- rep(1,50)
  priceHere[4]            <- df.scen[df.scen$Year==yearHere & 
                                       df.scen$Case=="2DS" &
                                       df.scen$Var.type=="Price.c" &
                                       df.scen$Market==market,"Value"]
  df.dD$Price.e.2DS       <- priceHere

  priceHere               <- rep(1,50)
  priceHere[4]            <- df.scen[df.scen$Year==yearHere & 
                                       df.scen$Case=="2DS" &
                                       df.scen$Var.type=="Price.c" &
                                       df.scen$Market=="Domestic","Value"]
  df.dD$Price.d.2DS      <- priceHere
    
  df.dD$Value.demandD.BAU    <- df.dD$Vol.demandD.BAU  * df.dD$Price.e.BAU
  df.dD$Value.demandD.2DS    <- df.dD$Vol.demandD.2DS  * df.dD$Price.e.2DS
  df.dD$Value.demandM.BAU    <- df.dD$Vol.demandM.BAU  * df.dD$Price.e.BAU
  df.dD$Value.demandM.2DS    <- df.dD$Vol.demandM.2DS  * df.dD$Price.e.2DS
  
  return(df.dD)
}

## put in handy format

domesticDemand.coal <- function(df.scen, market)
{
#  market              <- "Domestic"
#  fileName                <- paste("df.domesticDemand.coal", market,sep=".")
  df.domesticDemand.coal  <- domesticDemand.coal.zeros(df.scen, yearHere=2018, market=market )
  
  for (yearHere in yearsCPI[3:19])
    #for (yearHere in c(2025))
  {
    temp <- domesticDemand.coal.zeros(df.scen, yearHere=yearHere, market=market )
    df.domesticDemand.coal <- bind_rows(df.domesticDemand.coal,temp)
  }
  
  df.demand <- df.domesticDemand.coal %>% gather(Var, Value, -Industry, -SIC, -Year)
  split     <- strsplit(df.demand$Var,split='.', fixed=TRUE)
  part1    <- unlist(split)[3*(1:length(split))-2]
  part2    <- unlist(split)[3*(1:length(split))-1]
  part3    <- unlist(split)[3*(1:length(split))]
  df.demand$Case <- part3
  df.demand$Var  <-paste(part1,part2,sep=".")

#  results <- list(market,df.demand)
  return(df.demand)
  
}
  
############ demand scenarios with coal export demand from CPI, the others sectors 0

df.domesticDemand.coalExport <- domesticDemand.coal(df.scen, market="Export" )

cat("Demand scenario as input with zero demand all sectors except for coal export : df.domesticDemand.coalExport","\n","\n")


df.domesticDemand.coalDomestic <- domesticDemand.coal(df.scen, market="Domestic" )

cat("Demand scenario as input with zero demand all sector except for domestic coal use : df.domesticDemand.coalDomestic","\n","\n")











