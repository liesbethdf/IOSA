
############################################
############ Decomposition of output into imports, taxes, labour and capital share
############ + Employment effect
############################################

finaldemandDecomposition <- function(df.finalDemand.scenario)
{
  colnamesHere.di             <- c(Order,"Industry","Year","Var","Case")
  colnamesHere.finDemand      <- c("Final.demand.imp","Industry","Year","Var","Case") 
  colnamesHere.outputDecomp   <- c("Year", "Scenario","Industry","Vol.demand.dom","Value.demand.dom","Vol.demand.imp","Value.demand.imp","Price","Output.vol", "Output.value",
                                   "Dom.inputs","Imp.inputs", "Taxes.prod.vol", "VA.vol", "Wages", "Taxes.other.vol","Gross.op.surplus.vol",
                                   "Taxes.other.value", "Gross.op.surplus.value", "Taxes.prod.value", "VA.value")
  
  X.di            <- data.frame(matrix(ncol = length(colnamesHere.di), nrow = 0))
  colnames(X.di)  <- colnamesHere.di
  WS.di       <- X.di
  VA.di       <- X.di
  TO.di       <- X.di
  TP.di       <- X.di
  OS.di       <- X.di
  D.int       <- X.di # Domestic inputs (costs)
  M.int       <- X.di # Imported inputs (costs) again, more detailed
  MI.di       <- X.di # Imported inputs (costs)
  NE.di       <- X.di # Number of employees
  X.val.di    <- X.di
  TO.val.di   <- X.di
  TP.val.di   <- X.di
  OS.val.di   <- X.di
  VA.val.di   <- X.di
  
#  M.finalDemand             <- data.frame(matrix(ncol = length(colnamesHere.finDemand), nrow = 0))
#  colnames(M.finalDemand)   <- colnamesHere.finDemand
  
  df.outputDecomp           <- data.frame(matrix(ncol = length(colnamesHere.outputDecomp), nrow = 0))
  initiate.outputDecomp     <- data.frame(matrix(rep(NA,length(colnamesHere.outputDecomp)), nrow = 1))
  colnames(df.outputDecomp) <- colnamesHere.outputDecomp
  colnames(initiate.outputDecomp) <- colnamesHere.outputDecomp
  
  for (yearHere in 2018:2035)
  {
    #  for(scenarioHere in c("Vol.BAU"))
    for(scenarioHere in c("BAU","2Deg"))
    {
       #  yearHere <- 2025
       #  scenarioHere <- "BAU"
      
      vect.outputDecompHere <- initiate.outputDecomp
      
      demand.d.scen     <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere &
                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                   df.finalDemand.scenario$Var=="Vol.demandD", "Value" ]

      demand.d.scen.val <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere &
                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                   df.finalDemand.scenario$Var=="Value.demandD", "Value" ]
            
      price.e.scen      <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere &
                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                   df.finalDemand.scenario$Var=="Price.e", "Value" ]
      
      X.diHere    <-  LI.d %*% diag(demand.d.scen)
      WS.diHere   <-  diag(WS.per.production) %*% LI.d %*% diag(demand.d.scen)
      VA.diHere   <-  diag(VA.per.production) %*% LI.d %*% diag(demand.d.scen)
      TO.diHere   <-  diag(TO.per.production) %*% LI.d %*% diag(demand.d.scen)
      TP.diHere   <-  diag(TP.per.production) %*% LI.d %*% diag(demand.d.scen)
      OS.diHere   <-  diag(OS.per.production) %*% LI.d %*% diag(demand.d.scen)
      D.intHere   <-  diag(DI.per.production) %*% LI.d %*% diag(demand.d.scen)
      MI.diHere   <-  diag(MI.per.production) %*% LI.d %*% diag(demand.d.scen)
      NE.diHere   <-  diag(employment.per.production) %*% LI.d %*% diag(demand.d.scen)
      
      X.val.diHere<-  LI.d %*% diag(demand.d.scen) - diag(demand.d.scen) + diag(demand.d.scen.val)
      
      VAscaler <- (X.val.diHere - D.intHere - MI.diHere - WS.diHere)/ (TO.diHere + OS.diHere + TP.diHere)
      VAscaler[is.na(VAscaler)] <- 1
      
      TO.val.diHere   <-  TO.diHere * VAscaler
      TP.val.diHere   <-  TP.diHere * VAscaler
      OS.val.diHere   <-  OS.diHere * VAscaler
      
      VA.val.diHere   <-  WS.diHere + OS.val.diHere + TO.val.diHere
      
      df.list   <- list(X.diHere, WS.diHere, VA.diHere, TO.diHere, TP.diHere, OS.diHere, D.intHere, MI.diHere, NE.diHere, 
                        X.val.diHere, TO.val.diHere, TP.val.diHere, OS.val.diHere, VA.val.diHere)
      names.var <- c("Output","Compensation employees", "Gross value added", "Other taxes", "Taxes on production", "Gross operating surplus", 
                     "Domestic inputs","Imported inputs","Employment", "Output in value", "Other taxes, value", "Taxes on production, value", 
                     "Gross operating surplus, value", "Value added, value") 
      for(i in 1:length(df.list))
      {
        df            <- data.frame(df.list[[i]])
        colnames(df)  <- Order
        df$Industry   <- Order
        df$Year       <- rep(yearHere,n)
        df$Var        <- rep(names.var[i],n)
        df$Case       <- rep(scenarioHere,n)
        df.list[[i]]  <- df
      }
      
      X.di        <- bind_rows(X.di,df.list[[1]]) 
      WS.di       <- bind_rows(WS.di,df.list[[2]]) 
      VA.di       <- bind_rows(VA.di,df.list[[3]]) 
      TO.di       <- bind_rows(TO.di,df.list[[4]]) 
      TP.di       <- bind_rows(TP.di,df.list[[5]]) 
      OS.di       <- bind_rows(OS.di,df.list[[6]]) 
      D.int       <- bind_rows(D.int,df.list[[7]]) 
      MI.di       <- bind_rows(MI.di,df.list[[8]]) 
      NE.di       <- bind_rows(NE.di,df.list[[9]]) 
      X.val.di    <- bind_rows(X.val.di,df.list[[10]]) 
      TO.val.di   <- bind_rows(TO.val.di,df.list[[11]]) 
      TP.val.di   <- bind_rows(TP.val.di,df.list[[12]]) 
      OS.val.di   <- bind_rows(OS.val.di,df.list[[13]]) 
      VA.val.di   <- bind_rows(VA.val.di,df.list[[14]]) 
      
  #    M.finalDemandHere           <-  diag(s.m.exp)   %*%   demand.d.scen # no this is incorrect, demand.d.scen is domestic demand not total, the following lines are incorrect
  #    M.finalDemandHere           <-  data.frame(M.finalDemandHere)
  #    colnames(M.finalDemandHere) <- "Final.demand.imp"
  #    M.finalDemandHere$Industry  <- Order
  #    M.finalDemandHere$Year      <- yearHere
  #    M.finalDemandHere$Var       <- rep("Imported final goods",n)
  #    M.finalDemandHere$Case      <- rep(scenarioHere,n)
  #    M.finalDemand               <- bind_rows(M.finalDemand, M.finalDemandHere)
      
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
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Vol.demand.dom"]    <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                                   df.finalDemand.scenario$Var=="Vol.demandD" & 
                                                                                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                                                                                   df.finalDemand.scenario$Industry==Industry[s]
                                                                                                                 ,"Value"]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Value.demand.dom"]  <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                                   df.finalDemand.scenario$Var=="Value.demandD" & 
                                                                                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                                                                                   df.finalDemand.scenario$Industry==Industry[s]
                                                                                                                 ,"Value"]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Vol.demand.imp"]    <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                                   df.finalDemand.scenario$Var=="Vol.demandM" & 
                                                                                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                                                                                   df.finalDemand.scenario$Industry==Industry[s]
                                                                                                                 ,"Value"]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Value.demand.imp"]  <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                                   df.finalDemand.scenario$Var=="Value.demandM" & 
                                                                                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                                                                                   df.finalDemand.scenario$Industry==Industry[s]
                                                                                                                 ,"Value"]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Price"]         <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                               df.finalDemand.scenario$Var=="Price.e" & 
                                                                                                               df.finalDemand.scenario$Case==scenarioHere &
                                                                                                               df.finalDemand.scenario$Industry==Industry[s]
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
  results <- list(list("Output, direct and indirect, volume",X.di),        #1
                  list("Compensation of employees",WS.di),                 #2
                  list("Value added, volume",VA.di),                       #3
                  list("Other taxes, volume",TO.di),                       #4
                  list("Net taxes on production, volume",TP.di),           #5
                  list("Gross operating surplus, volume",OS.di),           #6
                  list("Domestically produced inputs, volume",D.int),      #7
                  list("Imported inputs, volume",MI.di),                   #8
                  list("Imported inputs, detailed, volume",M.int),         #9
                  list("Number of employees",NE.di),                      #10
                  list("Output, direct and indirect, value",X.val.di),    #11
                  list("Other taxes, value",TO.val.di),                   #12
                  list("Taxes on production, value",TP.val.di),           #13
                  list("Gross operating surplus, value",OS.val.di),       #14
                  list("Value added, value", VA.val.di),                  #15
                  list("Summary",df.outputDecomp))                        #16
  return(results)
}



