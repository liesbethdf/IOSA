############################################
############ Construction demand input vector for difference 2Deg & BAU CPI coal export projections
############################################

## Coal scenarios from CPI

df.scen <- df.CPI %>% filter(!df.CPI$Unit=="mtce") %>% select(-Unit, -Variable, -Case) %>% spread(Var, Value)

df.scen <- df.scen[,c(1,5,4,3,2)]

scale.V <- df.IOT2014$Exports[4]/df.scen$V.BAU[1]
scale.P <- 1/df.scen$P.BAU[1]

df.scen$V.BAU.Cal     <- df.scen$V.BAU  * scale.V
df.scen$V.2Deg.Cal    <- df.scen$V.2Deg * scale.V
df.scen$P.BAU.Cal     <- df.scen$P.BAU  * scale.P
df.scen$P.2Deg.Cal    <- df.scen$P.2Deg * scale.P
df.scen$Value.BAU.Cal <- df.scen$P.BAU.Cal * df.scen$V.BAU.Cal
df.scen$Value.2Deg.Cal <- df.scen$P.2Deg.Cal * df.scen$V.2Deg.Cal

## Final demand from 2014 SA IOT

yearHere <- df.scen$Year[1]

vect.exp  <-  df.IOT2014$Exports[1:n] 
vect.cons <-  df.IOT2014$Household[1:n] 
vect.gov  <-  df.IOT2014$General.Government[1:n] 
vect.gfcf <-  df.IOT2014$Capital.formation[1:n]
vect.chi  <-  df.IOT2014$Changes.in.inventories[1:n]

demand.d <-  function(vect.exp, vect.cons, vect.gov, vect.gfcf, vect.chi)
{
  diag(s.d.exp)   %*%   vect.exp +
    diag(s.d.cons)  %*%   vect.cons +
    diag(s.d.gov)   %*%   vect.gov +
    diag(s.d.gfcf)  %*%   vect.gfcf +
    diag(s.d.chi)   %*%   vect.chi
}

demand.m <-  function(vect.exp, vect.cons, vect.gov, vect.gfcf, vect.chi)
{
  diag(s.m.exp)   %*%   vect.exp +
    diag(s.m.cons)  %*%   vect.cons +
    diag(s.m.gov)   %*%   vect.gov +
    diag(s.m.gfcf)  %*%   vect.gfcf +
    diag(s.m.chi)   %*%   vect.chi
}

############ below is the construction of domestic demand scenarios as in 2014 IOT and for coal the CPI projections

domesticDemand.input <- function(yearHere)
{
  df.dD              <- data.frame(Industry)
  colnames(df.dD)    <- "Industry"
  df.dD$SIC          <- SIC
  df.dD$Year         <- rep(yearHere,n)
  
  vect.expHere          <- vect.exp
  vect.expHere[4]       <- df.scen[df.scen$Year==yearHere,"V.BAU.Cal"]
  demandHereD           <- demand.d(vect.expHere, vect.cons, vect.gov, vect.gfcf, vect.chi) 
  demandHereM           <- demand.m(vect.expHere, vect.cons, vect.gov, vect.gfcf, vect.chi) 
  df.dD$Vol.demandD.BAU <- demandHereD
  df.dD$Vol.demandM.BAU <- demandHereM
  
  priceHere             <- rep(1,50)
  priceHere[4]          <- df.scen[df.scen$Year==yearHere,"P.BAU.Cal"]
  df.dD$Price.e.BAU     <- priceHere
  
  vect.expHere          <- vect.exp
  vect.expHere[4]       <- df.scen[df.scen$Year==yearHere,"V.2Deg.Cal"]
  demandHereD           <- demand.d(vect.expHere, vect.cons, vect.gov, vect.gfcf, vect.chi) 
  demandHereM           <- demand.m(vect.expHere, vect.cons, vect.gov, vect.gfcf, vect.chi) 
  df.dD$Vol.demandD.2Deg<- demandHereD
  df.dD$Vol.demandM.2Deg<- demandHereM
  
  priceHere             <- rep(1,50)
  priceHere[4]          <- df.scen[df.scen$Year==yearHere,"P.2Deg.Cal"]
  df.dD$Price.e.2Deg    <- priceHere
  
  df.dD$Value.demandD.BAU    <- df.dD$Vol.demandD.BAU  * df.dD$Price.e.BAU
  df.dD$Value.demandD.2Deg   <- df.dD$Vol.demandD.2Deg * df.dD$Price.e.2Deg
  df.dD$Value.demandM.BAU    <- df.dD$Vol.demandM.BAU  * df.dD$Price.e.BAU
  df.dD$Value.demandM.2Deg   <- df.dD$Vol.demandM.2Deg * df.dD$Price.e.2Deg
  
  return(df.dD)
}

df.domesticDemand.input <- domesticDemand.input(2018)

for (yearHere in df.scen$Year[2:18])
{
  temp <- domesticDemand.input(yearHere)
  df.domesticDemand.input <- bind_rows(df.domesticDemand.input,temp)
}

df.domesticDemand.input <- df.domesticDemand.input %>% gather(Var, Value, -Industry, -SIC, -Year)
split     <- strsplit(df.domesticDemand.input$Var,split='.', fixed=TRUE)
part1    <- unlist(split)[3*(1:length(split))-2]
part2    <- unlist(split)[3*(1:length(split))-1]
part3    <- unlist(split)[3*(1:length(split))]
df.domesticDemand.input$Case <- part3
df.domesticDemand.input$Var  <-paste(part1,part2,sep=".")

# this is the result : df.domesticDemand.input

############ demand scenarios with coal export only, the others 0

domesticDemand.coal.input <- function(yearHere)
{
  df.dD              <- data.frame(Industry)
  colnames(df.dD)    <- "Industry"
  df.dD$SIC          <- SIC
  df.dD$Year         <- rep(yearHere,n)
  
  demandHere              <- rep(0,50)
  demandHere[4]           <- df.scen[df.scen$Year==yearHere,"V.BAU.Cal"]
  df.dD$Vol.demandD.BAU   <- diag(s.d.exp)   %*% demandHere
  df.dD$Vol.demandM.BAU   <- diag(s.m.exp)   %*% demandHere
  
  priceHere               <- rep(1,50)
  priceHere[4]            <- df.scen[df.scen$Year==yearHere,"P.BAU.Cal"]
  df.dD$Price.e.BAU       <- priceHere
  
  demandHere              <- rep(0,50)
  demandHere[4]           <- df.scen[df.scen$Year==yearHere,"V.2Deg.Cal"]
  df.dD$Vol.demandD.2Deg  <- diag(s.d.exp)  %*% demandHere
  df.dD$Vol.demandM.2Deg  <- diag(s.m.exp)  %*% demandHere
  
  priceHere               <- rep(1,50)
  priceHere[4]            <- df.scen[df.scen$Year==yearHere,"P.2Deg.Cal"]
  df.dD$Price.e.2Deg      <- priceHere
  
  df.dD$Value.demandD.BAU    <- df.dD$Vol.demandD.BAU  * df.dD$Price.e.BAU
  df.dD$Value.demandD.2Deg   <- df.dD$Vol.demandD.2Deg * df.dD$Price.e.2Deg
  df.dD$Value.demandM.BAU    <- df.dD$Vol.demandM.BAU  * df.dD$Price.e.BAU
  df.dD$Value.demandM.2Deg   <- df.dD$Vol.demandM.2Deg * df.dD$Price.e.2Deg
  
  return(df.dD)
}

df.domesticDemand.coal.input <- domesticDemand.coal.input(2018)

for (yearHere in df.scen$Year[2:18])
{
  temp <- domesticDemand.coal.input(yearHere)
  df.domesticDemand.coal.input <- bind_rows(df.domesticDemand.coal.input,temp)
}

df.domDemand.coal.input.v2 <- df.domesticDemand.coal.input %>% gather(Var, Value, -Industry, -SIC, -Year)
split     <- strsplit(df.domDemand.coal.input.v2$Var,split='.', fixed=TRUE)
part1    <- unlist(split)[3*(1:length(split))-2]
part2    <- unlist(split)[3*(1:length(split))-1]
part3    <- unlist(split)[3*(1:length(split))]
df.domDemand.coal.input.v2$Case <- part3
df.domDemand.coal.input.v2$Var  <-paste(part1,part2,sep=".")

# this is the result : df.domDemand.coal.input.v2













