
############################################
############ Construction input vector for difference 2Deg & BAU CPI coal export projections
############################################

## Coal scenarios from CPI

df.scen <- df.CPI %>% filter(!df.CPI$Unit=="mtce") %>% 
                      select(-Unit, -Variable, -Case) %>%
                      spread(Var, Value)

df.scen <- df.scen[,c(1,5,4,3,2)]

scale.V <- df.IOT2014$Exports[4]/df.scen$V.BAU[1]
scale.P <- 1/df.scen$P.BAU[1]

df.scen$V.BAU.Cal   <- df.scen$V.BAU  * scale.V
df.scen$V.2Deg.Cal  <- df.scen$V.2Deg * scale.V
df.scen$P.BAU.Cal   <- df.scen$P.BAU  * scale.P
df.scen$P.2Deg.Cal  <- df.scen$P.2Deg * scale.P

## Final demand from 2014 SA IOT

yearHere <- df.scen$Year[1]

vect.exp  <-  df.IOT2014$Exports[1:n] 
vect.cons <-  df.IOT2014$Household[1:n] 
vect.gov  <-  df.IOT2014$General.Government[1:n] 
vect.gfcf <-  df.IOT2014$Capital.formation[1:n]
vect.chi  <-  df.IOT2014$Changes.in.inventories[1:n]

demand.d <-  function(vect.exp)
                  {
                    diag(s.d.exp)   %*%   vect.exp +
                    diag(s.d.cons)  %*%   vect.cons +
                    diag(s.d.gov)   %*%   vect.gov +
                    diag(s.d.gfcf)  %*%   vect.gfcf +
                    diag(s.d.chi)   %*%   vect.chi
                  }


# below is the scenario for yearHere, V.BAU.Cal and it is total , not diff 3 types, BAU, 2Deg, Diff + year

domesticDemand.input <- function(yearHere)
{
  df.dD              <- data.frame(Industry)
  colnames(df.dD)    <- "Industry"
  df.dD$SIC          <- SIC
  df.dD$Year         <- rep(yearHere,n)
  
  vect.expHere    <- vect.exp
  vect.expHere[4] <- df.scen[df.scen$Year==yearHere,"V.BAU.Cal"]
  demandHere <- demand.d(vect.expHere) 
  df.dD$Vol.BAU     <- demandHere
  
  vect.expHere    <- vect.exp
  vect.expHere[4] <- df.scen[df.scen$Year==yearHere,"V.2Deg.Cal"]
  demandHere <- demand.d(vect.expHere) 
  df.dD$Vol.2Deg    <- demandHere
  
  df.dD$Value.BAU    <- df.dD$Vol.BAU  * df.scen[df.scen$Year==yearHere,"P.BAU.Cal"]
  df.dD$Value.2Deg   <- df.dD$Vol.2Deg * df.scen[df.scen$Year==yearHere,"P.2Deg.Cal"]
  
  return(df.dD)
}

df.domesticDemand.input <- domesticDemand.input(2018)

for (yearHere in df.scen$Year[2:18])
{
  temp <- domesticDemand.input(yearHere)
  df.domesticDemand.input <- bind_rows(df.domesticDemand.input,temp)
}

# demand scenarios with coal only, the others 0


domesticDemand.coal.input <- function(yearHere)
{
  df.dD              <- data.frame(Industry)
  colnames(df.dD)    <- "Industry"
  df.dD$SIC          <- SIC
  df.dD$Year         <- rep(yearHere,n)
  
  demandHere    <- rep(0,50)
  demandHere[4] <- df.scen[df.scen$Year==yearHere,"V.BAU.Cal"]
  df.dD$Vol.BAU     <- demandHere
  
  demandHere    <- rep(0,50)
  demandHere[4] <- df.scen[df.scen$Year==yearHere,"V.2Deg.Cal"]
  df.dD$Vol.2Deg    <- demandHere
  
  df.dD$Value.BAU    <- df.dD$Vol.BAU  * df.scen[df.scen$Year==yearHere,"P.BAU.Cal"]
  df.dD$Value.2Deg   <- df.dD$Vol.2Deg * df.scen[df.scen$Year==yearHere,"P.2Deg.Cal"]
  
  return(df.dD)
}

df.domesticDemand.coal.input <- domesticDemand.coal.input(2018)

for (yearHere in df.scen$Year[2:18])
{
  temp <- domesticDemand.coal.input(yearHere)
  df.domesticDemand.coal.input <- bind_rows(df.domesticDemand.coal.input,temp)
}


############################################
############ Decomposition of output into imports, taxes, labour and capital share
############ + Employment effect
############################################

yearHere <- 2020

demand.d.scen <- df.domesticDemand.coal.input[df.domesticDemand.coal.input$Year==yearHere, "Vol.BAU"]

WS.di   <- diag(WS.per.production) %*% LI.d %*% diag(demand.d.scen)
VA.di   <- diag(VA.per.production) %*% LI.d %*% diag(demand.d.scen)
TO.di   <- diag(TO.per.production) %*% LI.d %*% diag(demand.d.scen)
TP.di   <- diag(TP.per.production) %*% LI.d %*% diag(demand.d.scen)
OS.di   <- diag(OS.per.production) %*% LI.d %*% diag(demand.d.scen)

output.di     <-  LI.d %*% demand.d

rowSums(wages.di)
rowSums(VA.di)
rowSums(taxesOther.di)
rowSums(taxesProd.di)
rowSums(GOSurplus.di)

colSums(wages.di)
colSums(VA.di)
colSums(taxesOther.di)
colSums(taxesProd.di)
colSums(GOSurplus.di)


