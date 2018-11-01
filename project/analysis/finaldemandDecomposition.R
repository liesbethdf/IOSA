
############################################
############ Construction demand input vector for difference 2Deg & BAU CPI coal export projections
############################################

## Coal scenarios from CPI

df.scen <- df.CPI %>% filter(!df.CPI$Unit=="mtce") %>% 
                      select(-Unit, -Variable, -Case) %>%
                      spread(Var, Value)

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

demand.d <-  function(vect.exp)
{
  diag(s.d.exp)   %*%   vect.exp +
  diag(s.d.cons)  %*%   vect.cons +
  diag(s.d.gov)   %*%   vect.gov +
  diag(s.d.gfcf)  %*%   vect.gfcf +
  diag(s.d.chi)   %*%   vect.chi
}

demand.m <-  function(vect.exp)
{
  diag(s.m.exp)   %*%   vect.exp +
  diag(s.m.cons)  %*%   vect.cons +
  diag(s.m.gov)   %*%   vect.gov +
  diag(s.m.gfcf)  %*%   vect.gfcf +
  diag(s.m.chi)   %*%   vect.chi
}

# below is the construction of domestic demand scenarios as in 2014 IOT and for coal the CPI projections

domesticDemand.input <- function(yearHere)
{
  df.dD              <- data.frame(Industry)
  colnames(df.dD)    <- "Industry"
  df.dD$SIC          <- SIC
  df.dD$Year         <- rep(yearHere,n)
  
  vect.expHere          <- vect.exp
  vect.expHere[4]       <- df.scen[df.scen$Year==yearHere,"V.BAU.Cal"]
  demandHereD           <- demand.d(vect.expHere) 
  demandHereM           <- demand.m(vect.expHere) 
  df.dD$Vol.demandD.BAU <- demandHereD
  df.dD$Vol.demandM.BAU <- demandHereM
  
  priceHere             <- rep(1,50)
  priceHere[4]          <- df.scen[df.scen$Year==yearHere,"P.BAU.Cal"]
  df.dD$Price.e.BAU     <- priceHere
  
  vect.expHere          <- vect.exp
  vect.expHere[4]       <- df.scen[df.scen$Year==yearHere,"V.2Deg.Cal"]
  demandHereD           <- demand.d(vect.expHere) 
  demandHereM           <- demand.m(vect.expHere) 
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

# demand scenarios with coal only, the others 0

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


############################################
############ Decomposition of output into imports, taxes, labour and capital share
############ + Employment effect
############################################

colnamesHere.di             <- c(Order,"Industry","Year","Var","Case")
colnamesHere.finDemand      <- c("Final.demand.imp","Industry","Year","Var","Case") 
colnamesHere.outputDecomp   <- c("Year", "Scenario","Industry","Vol.demand.dom","Value.demand.dom","Vol.demand.imp","Value.demand.imp","Price","Output.vol", "Output.value",
                                 "Dom.inputs","Imp.inputs", "Taxes.prod.vol", "VA.vol", "Wages", "Taxes.other.vol","Gross.op.surplus.vol",
                                 "Taxes.other.value", "Gross.op.surplus.value", "Taxes.prod.value", "VA.value")

X.di            <- data.frame(matrix(ncol = length(colnamesHere.di), nrow = 0))
colnames(X.di)  <- colnamesHere.di
WS.di           <- X.di
VA.di           <- X.di
TO.di           <- X.di
TP.di           <- X.di
OS.di           <- X.di
D.int           <- X.di # Domestic inputs (costs)
M.int           <- X.di # Imported inputs (costs)
MI.di           <- X.di # Imported inputs (costs) again, less detailed

M.finalDemand             <- data.frame(matrix(ncol = length(colnamesHere.finDemand), nrow = 0))
colnames(M.finalDemand)   <- colnamesHere.finDemand

df.outputDecomp           <- data.frame(matrix(ncol = length(colnamesHere.outputDecomp), nrow = 0))
initiate.outputDecomp     <- data.frame(matrix(rep(NA,length(colnamesHere.outputDecomp)), nrow = 1))
colnames(df.outputDecomp) <- colnamesHere.outputDecomp
colnames(initiate.outputDecomp) <- colnamesHere.outputDecomp

for (yearHere in 2018:2035)
{
#  for(scenarioHere in c("Vol.BAU"))
  for(scenarioHere in c("BAU","2Deg"))
  {
#   yearHere <- 2018
#   scenarioHere <- "BAU"
    
    vect.outputDecompHere <- initiate.outputDecomp
    
    demand.d.scen <- df.domDemand.coal.input.v2[df.domDemand.coal.input.v2$Year==yearHere &
                                                df.domDemand.coal.input.v2$Case==scenarioHere &
                                                df.domDemand.coal.input.v2$Var=="Vol.demandD", "Value" ]

    X.diHere    <-  LI.d %*% diag(demand.d.scen)
    WS.diHere   <-  diag(WS.per.production) %*% LI.d %*% diag(demand.d.scen)
    VA.diHere   <-  diag(VA.per.production) %*% LI.d %*% diag(demand.d.scen)
    TO.diHere   <-  diag(TO.per.production) %*% LI.d %*% diag(demand.d.scen)
    TP.diHere   <-  diag(TP.per.production) %*% LI.d %*% diag(demand.d.scen)
    OS.diHere   <-  diag(OS.per.production) %*% LI.d %*% diag(demand.d.scen)
    D.intHere   <-  diag(DI.per.production) %*% LI.d %*% diag(demand.d.scen)
    MI.diHere   <-  diag(MI.per.production) %*% LI.d %*% diag(demand.d.scen)
    
    df.list   <- list(X.diHere, WS.diHere, VA.diHere, TO.diHere, TP.diHere, OS.diHere, D.intHere, MI.diHere)
    names.var <- c("Output","Compensation employees", "Gross value added", "Other taxes", "Taxes on production", "Gross operating surplus", "Domestic inputs","Imported inputs") 
    for(i in 1:8)
    {
      df            <- data.frame(df.list[[i]])
      colnames(df)  <- Order
      df$Industry   <- Order
      df$Year       <- rep(yearHere,n)
      df$Var        <- rep(names.var[i],n)
      df$Case       <- rep(scenarioHere,n)
      df.list[[i]]  <- df
    }
    
    X.di    <- bind_rows(X.di,df.list[[1]]) 
    WS.di   <- bind_rows(WS.di,df.list[[2]]) 
    VA.di   <- bind_rows(VA.di,df.list[[3]]) 
    TO.di   <- bind_rows(TO.di,df.list[[4]]) 
    TP.di   <- bind_rows(TP.di,df.list[[5]]) 
    OS.di   <- bind_rows(OS.di,df.list[[6]]) 
    D.int   <- bind_rows(D.int,df.list[[7]]) 
    MI.di   <- bind_rows(MI.di,df.list[[8]]) 
    
    M.finalDemandHere           <-  diag(s.m.exp)   %*%   demand.d.scen # no this is incorrect, demand.d.scen is domestic demand not total, the following lines are incorrect
    M.finalDemandHere           <-  data.frame(M.finalDemandHere)
    colnames(M.finalDemandHere) <- "Final.demand.imp"
    M.finalDemandHere$Industry  <- Order
    M.finalDemandHere$Year      <- yearHere
    M.finalDemandHere$Var       <- rep("Imported final goods",n)
    M.finalDemandHere$Case      <- rep(scenarioHere,n)
    M.finalDemand               <- bind_rows(M.finalDemand, M.finalDemandHere)
    
    X1.di                       <- diag(X.diHere[,1])
    M.int.1                     <- A.m %*% X1.di
    M.intHere                   <- data.frame(M.int.1)
    colnames(M.intHere)         <- Order
    M.intHere$Industry          <- rep(Order[1],n)
    M.intHere$Year              <- rep(yearHere,n)
    M.intHere$Var               <- rep("Imported inputs",n)
    M.intHere$Case              <- rep(scenarioHere,n)
    
    for (k in 2:50)
    {
      Xk.di                 <- diag(X.diHere[,k])
      M.int.k               <- data.frame(round(A.m %*% Xk.di,6))
      colnames(M.int.k)     <- Order
      M.int.k$Industry      <- rep(Order[k],n)
      M.int.k$Year          <- rep(yearHere,n)
      M.int.k$Var         <- rep("Imported inputs",n)
      M.int.k$Case        <- rep(scenarioHere,n)
      M.intHere             <- bind_rows(M.intHere,M.int.k)
    }
    M.int     <-  bind_rows(M.int,M.intHere) 
    
    ## Fill the summary table
    ## Do it once for the coal sector, and once for others
    
    s <- 4 # production of sector
    
    for (s in 1:n)
    {
      
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Year"]          <- yearHere
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Scenario"]      <- scenarioHere
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Industry"]      <- Order[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Vol.demand.dom"]    <- df.domDemand.coal.input.v2[df.domDemand.coal.input.v2$Year==yearHere & 
                                                                                                                    df.domDemand.coal.input.v2$Var=="Vol.demandD" & 
                                                                                                                    df.domDemand.coal.input.v2$Case==scenarioHere &
                                                                                                                    df.domDemand.coal.input.v2$Industry==Industry[s]
                                                                                                                  ,"Value"]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Value.demand.dom"]  <- df.domDemand.coal.input.v2[df.domDemand.coal.input.v2$Year==yearHere & 
                                                                                                                    df.domDemand.coal.input.v2$Var=="Value.demandD" & 
                                                                                                                    df.domDemand.coal.input.v2$Case==scenarioHere &
                                                                                                                    df.domDemand.coal.input.v2$Industry==Industry[s]
                                                                                                                  ,"Value"]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Vol.demand.imp"]    <- df.domDemand.coal.input.v2[df.domDemand.coal.input.v2$Year==yearHere & 
                                                                                                                    df.domDemand.coal.input.v2$Var=="Vol.demandM" & 
                                                                                                                    df.domDemand.coal.input.v2$Case==scenarioHere &
                                                                                                                    df.domDemand.coal.input.v2$Industry==Industry[s]
                                                                                                                  ,"Value"]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Value.demand.imp"]  <- df.domDemand.coal.input.v2[df.domDemand.coal.input.v2$Year==yearHere & 
                                                                                                                    df.domDemand.coal.input.v2$Var=="Value.demandM" & 
                                                                                                                    df.domDemand.coal.input.v2$Case==scenarioHere &
                                                                                                                    df.domDemand.coal.input.v2$Industry==Industry[s]
                                                                                                                  ,"Value"]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Price"]         <- df.domDemand.coal.input.v2[df.domDemand.coal.input.v2$Year==yearHere & 
                                                                                                                df.domDemand.coal.input.v2$Var=="Price.e" & 
                                                                                                                df.domDemand.coal.input.v2$Case==scenarioHere &
                                                                                                                df.domDemand.coal.input.v2$Industry==Industry[s]
                                                                                                              ,"Value"]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Output.vol"]    <- rowSums(X.diHere)[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Output.value"]  <- vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Value.demand.dom"] + 
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Output.vol"]-
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Vol.demand.dom"]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Dom.inputs"]              <- rowSums(D.intHere)[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Imp.inputs"]              <- rowSums(MI.diHere)[s] #colSums(M.intHere[,1:50])[s] yields the same
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.prod.vol"]          <- rowSums(TP.diHere)[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="VA.vol"]                  <- rowSums(VA.diHere)[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Wages"]                   <- rowSums(WS.diHere)[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.other.vol"]         <- rowSums(TO.diHere)[s]
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Gross.op.surplus.vol"]    <- rowSums(OS.diHere)[s] 
      
      VAscaler <- (vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Output.value"] - rowSums(D.intHere)[s] - rowSums(MI.diHere)[s] - rowSums(WS.diHere)[s])/
        (rowSums(TO.diHere)[s] + rowSums(OS.diHere)[s] + rowSums(TP.diHere)[s])
      
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.other.value"]       <- rowSums(TO.diHere)[s] * VAscaler
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Gross.op.surplus.value"]  <- rowSums(OS.diHere)[s] * VAscaler
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.prod.value"]        <- rowSums(TP.diHere)[s] * VAscaler
      vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="VA.value"]                <- vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.other.value"] +
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Gross.op.surplus.value"] +
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Wages"]
      
      df.outputDecomp <- bind_rows(df.outputDecomp, vect.outputDecompHere)
      vect.outputDecompHere <- initiate.outputDecomp
      
    }
    
    
  }

}

setwd(dir.TABLES)
scenarioName    <- "ExportCoal_zeroDemandOther"
fileName.table  <- paste(scenarioName, "outputDecomp","allSectors",sep="_")
write.csv(df.outputDecomp,fileName.table)

setwd(dir.ANALYSIS)

#View(df.outputDecomp)

#results.coalExport  <- list(X.di, WS.di, VA.di, TO.di, TP.di, OS.di, M.int, M.finalDemand)


