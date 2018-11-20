
############################################
############ Decomposition of output into imports, taxes, labour and capital share
############ + Employment effect
############################################
#df.finalDemand.scenario <- df.domDemand.coal.input.v2
#df.finalDemand.scenario <- df.domesticDemand.coal.export
finaldemandDecomposition.q <- function(df.finalDemand.scenario)
{
  colnamesHere.di             <- c(Order,"Industry","Year","Var","Case")
  colnamesHere.finDemand      <- c("Final.demand.imp","Industry","Year","Var","Case") 
  colnamesHere.outputDecomp   <- c("Year", "Scenario","Industry","Vol.demand.dom","Value.demand.dom","Vol.demand.imp","Value.demand.imp","Price.export",
                                   "Price.domestic","Output.value", "Dom.inputs","Imp.inputs", "Taxes.prod", "VA", "Wages", 
                                    "Taxes.other","Gross.op.surplus")
  
  Xq.di            <- data.frame(matrix(ncol = length(colnamesHere.di), nrow = 0))
  colnames(Xq.di)  <- colnamesHere.di
  Xpq.di      <- Xq.di
  WS.di       <- Xq.di
  VA.di       <- Xq.di
  TO.di       <- Xq.di
  TP.di       <- Xq.di
  OS.di       <- Xq.di
  DI.di       <- Xq.di # Domestic inputs (costs)
  M.int       <- Xq.di # Imported inputs (costs) again, more detailed
  MI.di       <- Xq.di # Imported inputs (costs)
  NE.di       <- Xq.di # Number of employees
  # X.val.di    <- Xq.di
  # TO.val.di   <- Xq.di
  # TP.val.di   <- Xq.di
  # OS.val.di   <- Xq.di
  # VA.val.di   <- Xq.di
  
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
      # yearHere <- 2018
      # scenarioHere <- "BAU"
      # market <- "Export"
      # 
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
      
      price.d.scen      <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere &
                                                   df.finalDemand.scenario$Case==scenarioHere &
                                                   df.finalDemand.scenario$Var=="Price.d", "Value" ]
      
      Xq.diHere   <-  LI.d.q %*% diag(demand.d.scen)
      Xpq.diHere  <-  diag(price.d.scen) %*% (Xq.diHere - diag(demand.d.scen)) + diag(price.e.scen) %*% diag(demand.d.scen) # value of output given that intermediate inputs are sold at different price than exports

      # need the year's price to determine DI.per.production and MI.per.production 
      # input costs
      output.vectHere           <- rowSums(Xq.diHere) # the total output needed for all sectors is volume terms 
      DI.vol.total              <- rowSums(Xq.diHere) - demand.d.scen
      DI.val.total              <- DI.vol.total * price.d.scen
      output.vectHere.val       <- DI.val.total + demand.d.scen.val
        
      DI.per.production.matrix  <-  diag(price.d.scen) %*% A.d.q %*%  diag(output.vectHere) # contains per column the intermediary inputs (domestic) in value
      MI.per.production.matrix  <-  diag(price.e.scen) %*% A.m.q %*%  diag(output.vectHere) # assumption is price imports is same as price exports (internat price)
      DI.per.production.q         <-  colSums(DI.per.production.matrix)/output.vectHere.val
      MI.per.production.q         <-  colSums(MI.per.production.matrix)/output.vectHere.val
      DI.per.production.q[is.na(DI.per.production.q)] <- 0
      MI.per.production.q[is.na(MI.per.production.q)] <- 0
 
      WS.diHere   <-  diag(WS.per.production.q) %*% LI.d.q %*% diag(demand.d.scen)
      
      WS.per.production.q <- WS.per.production.q * output.vectHere / output.vectHere.val 
      
      rest                <- rowSums(Xpq.diHere) - rowSums(WS.diHere) - colSums(DI.per.production.matrix) - colSums(MI.per.production.matrix)
      
      OS.per.production.q <- OS.per.production/(OS.per.production + TP.per.production + TO.per.production) * rest/output.vectHere.val 
      TP.per.production.q <- TP.per.production/(OS.per.production + TP.per.production + TO.per.production) * rest/output.vectHere.val 
      TO.per.production.q <- TO.per.production/(OS.per.production + TP.per.production + TO.per.production) * rest/output.vectHere.val 
      
      WS.per.production.q[is.na(WS.per.production.q)] <- 0
      OS.per.production.q[is.na(OS.per.production.q)] <- 0
      TP.per.production.q[is.na(TP.per.production.q)] <- 0
      TO.per.production.q[is.na(TO.per.production.q)] <- 0
      
      VA.per.production.q <- OS.per.production.q + TO.per.production.q + WS.per.production.q
      VA.per.production.q[is.na(VA.per.production.q)] <- 0
      
      VA.diHere   <-  diag(VA.per.production.q)   %*% Xpq.diHere 
      TO.diHere   <-  diag(TO.per.production.q)   %*% Xpq.diHere 
      TP.diHere   <-  diag(TP.per.production.q)   %*% Xpq.diHere 
      OS.diHere   <-  diag(OS.per.production.q)   %*% Xpq.diHere 
      DI.diHere   <-  diag(DI.per.production.q)   %*% Xpq.diHere 
      MI.diHere   <-  diag(MI.per.production.q)   %*% Xpq.diHere 
      NE.diHere   <-  diag(employment.per.production.q) %*% LI.d.q %*% diag(demand.d.scen)
      
#      X.val.diHere<-  LI.d.q %*% diag(demand.d.scen) - diag(demand.d.scen) + diag(demand.d.scen.val)
      
#      VAscaler <- (X.val.diHere - DI.diHere - MI.diHere - WS.diHere)/ (TO.diHere + OS.diHere + TP.diHere)
#      VAscaler[is.na(VAscaler)] <- 1
      
#      TO.val.diHere   <-  TO.diHere * VAscaler
#      TP.val.diHere   <-  TP.diHere * VAscaler
#      OS.val.diHere   <-  OS.diHere * VAscaler
      
#      VA.val.diHere   <-  WS.diHere + OS.val.diHere + TO.val.diHere
      
      df.list   <- list(Xq.diHere, Xpq.diHere, WS.diHere, VA.diHere, TO.diHere, TP.diHere, OS.diHere, DI.diHere, MI.diHere, NE.diHere) 
                        #X.val.diHere, TO.val.diHere, TP.val.diHere, OS.val.diHere, VA.val.diHere)
      names.var <- c("Output volume","Output value","Compensation employees", "Gross value added value", "Other taxes value", "Taxes on production value", 
                     "Gross operating surplus value", "Domestic inputs value","Imported inputs value","Employment") 
      
      #, "Output in value", "Other taxes, value", "Taxes on production, value", "Gross operating surplus, value", "Value added, value") 
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
      
      Xq.di       <- bind_rows(Xq.di,df.list[[1]]) 
      Xpq.di      <- bind_rows(Xpq.di,df.list[[2]]) 
      WS.di       <- bind_rows(WS.di,df.list[[3]]) 
      VA.di       <- bind_rows(VA.di,df.list[[4]]) 
      TO.di       <- bind_rows(TO.di,df.list[[5]]) 
      TP.di       <- bind_rows(TP.di,df.list[[6]]) 
      OS.di       <- bind_rows(OS.di,df.list[[7]]) 
      DI.di       <- bind_rows(DI.di,df.list[[8]]) 
      MI.di       <- bind_rows(MI.di,df.list[[9]]) 
      NE.di       <- bind_rows(NE.di,df.list[[10]]) 
      # X.val.di    <- bind_rows(X.val.di,df.list[[10]]) 
      # TO.val.di   <- bind_rows(TO.val.di,df.list[[11]]) 
      # TP.val.di   <- bind_rows(TP.val.di,df.list[[12]]) 
      # OS.val.di   <- bind_rows(OS.val.di,df.list[[13]]) 
      # VA.val.di   <- bind_rows(VA.val.di,df.list[[14]]) 
      
  #    M.finalDemandHere           <-  diag(s.m.exp)   %*%   demand.d.scen # no this is incorrect, demand.d.scen is domestic demand not total, the following lines are incorrect
  #    M.finalDemandHere           <-  data.frame(M.finalDemandHere)
  #    colnames(M.finalDemandHere) <- "Final.demand.imp"
  #    M.finalDemandHere$Industry  <- Order
  #    M.finalDemandHere$Year      <- yearHere
  #    M.finalDemandHere$Var       <- rep("Imported final goods",n)
  #    M.finalDemandHere$Case      <- rep(scenarioHere,n)
  #    M.finalDemand               <- bind_rows(M.finalDemand, M.finalDemandHere)
      
      X1.di                       <- diag(Xq.diHere[,1])
      M.int.1                     <- A.m %*% X1.di
      M.intHere                   <- data.frame(M.int.1)
      colnames(M.intHere)         <- Order
      M.intHere$Industry          <- rep(Order[1],n)
      M.intHere$Year              <- rep(yearHere,n)
      M.intHere$Var               <- rep("Imported inputs",n)
      M.intHere$Case              <- rep(scenarioHere,n)
      
      for (k in 2:50)
      {
        Xk.di                 <- diag(Xq.diHere[,k])
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
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Price.export"]      <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                               df.finalDemand.scenario$Var=="Price.e" & 
                                                                                                               df.finalDemand.scenario$Case==scenarioHere &
                                                                                                               df.finalDemand.scenario$Industry==Industry[s]
                                                                                                             ,"Value"]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Price.domestic"]    <- df.finalDemand.scenario[df.finalDemand.scenario$Year==yearHere & 
                                                                                                                      df.finalDemand.scenario$Var=="Price.d" & 
                                                                                                                      df.finalDemand.scenario$Case==scenarioHere &
                                                                                                                      df.finalDemand.scenario$Industry==Industry[s]
                                                                                                                    ,"Value"]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Output.value"]      <- rowSums(Xpq.diHere)[s]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Dom.inputs"]              <- rowSums(DI.diHere)[s]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Imp.inputs"]              <- rowSums(MI.diHere)[s] #colSums(M.intHere[,1:50])[s] yields the same
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.prod"]              <- rowSums(TP.diHere)[s]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="VA"]                      <- rowSums(VA.diHere)[s]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Wages"]                   <- rowSums(WS.diHere)[s]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Taxes.other"]             <- rowSums(TO.diHere)[s]
        vect.outputDecompHere[1,colnames(vect.outputDecompHere)=="Gross.op.surplus"]        <- rowSums(OS.diHere)[s] 
        
        df.outputDecomp <- bind_rows(df.outputDecomp, vect.outputDecompHere)
        vect.outputDecompHere <- initiate.outputDecomp
        
      }
      
    }
    
  }
  results <- list(list("Output, direct and indirect, volume",Xq.di),      #1
                  list("Output, direct and indirect, value",Xpq.di),      #2
                  list("Compensation of employees", WS.di),               #3
                  list("Value added",VA.di),                              #4
                  list("Other taxes",TO.di),                              #5
                  list("Net taxes on production",TP.di),                  #6
                  list("Gross operating surplus",OS.di),                  #7
                  list("Domestically produced inputs",DI.di),             #8
                  list("Imported inputs",MI.di),                          #9
              #    list("Imported inputs, detailed, volume",M.int),        #10
                  list("Number of employees",NE.di),                      #10
                  list("Summary",df.outputDecomp))                        #11
  return(results)
}



