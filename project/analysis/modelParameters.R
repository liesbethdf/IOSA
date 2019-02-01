
############################################
########## fixing of the parameter values for the demand-pull model, based on the IOT, domestic and import + employment data
#################### the parameter values that are fixed are : 
############################## the direct requirement matrices (m&d) (=the A matrices) 
############################## the total requirment matrices (d)     (=the Leontief inverse)
############################## the share importation vs. domestic production in the different components of final demand
############################## the share importation vs. domestic production in total deamnd (assuming weights of components of demand (eg. export, conso etc.) as in reference table)
############################## the ratios to decompose output in va etc :
################################        gross value added / output
################################       net taxes products / output
################################              other taxes / output
################################                    wages / output
################################  gross operating surplus / output
################################               employment / output (this is about number of employees)
########## definition of some much used vectors with industries descriptions and codes, etc.
############################################

#the direct requirement matrices (m&d) (=the A matrices) 

Z     <- as.matrix(df.IOT2014[1:n,(1:n)+2])  
Z.d   <- as.matrix(df.IOT2014.dom[1:n,(1:n)+3])
Z.m   <- as.matrix(df.IOT2014.imp[1:n,(1:n)+3])

A     <- t(t(Z)   / df.IOT2014$Output[1:n])
A.d   <- t(t(Z.d) / df.IOT2014$Output[1:n])
A.m   <- t(t(Z.m) / df.IOT2014$Output[1:n])

# the total requirment matrices (d)     (=the Leontief inverse)

L     <- diag(x = 1, nrow=n, ncol = n) - A
L.d   <- diag(x = 1, nrow=n, ncol = n) - A.d
LI    <- inv(as.matrix(L))
LI.d  <- inv(as.matrix(L.d))

# the share importation vs. domestic production in the different components of final demand

s.d.exp   <- rep(0,n)
s.d.cons  <- rep(0,n)
s.d.gov   <- rep(0,n)
s.d.gfcf  <- rep(0,n)
s.d.chi   <- rep(0,n)
s.m.exp   <- rep(0,n)
s.m.cons  <- rep(0,n)
s.m.gov   <- rep(0,n)
s.m.gfcf  <- rep(0,n)
s.m.chi   <- rep(0,n)

nonzero <- which(!df.IOT2014$Exports[1:n]==0)
s.d.exp[nonzero] <- df.IOT2014.dom$Exports[nonzero]/df.IOT2014$Exports[nonzero]

nonzero <- which(!df.IOT2014$Household[1:n]==0)
s.d.cons[nonzero] <- df.IOT2014.dom$Household[nonzero]/df.IOT2014$Household[nonzero]

nonzero <- which(!df.IOT2014$General.Government[1:n]==0)
s.d.gov[nonzero] <- df.IOT2014.dom$General.Government[nonzero]/df.IOT2014$General.Government[nonzero]

nonzero <- which(!df.IOT2014$Capital.formation[1:n]==0)
s.d.gfcf[nonzero] <- df.IOT2014.dom$Capital.formation[nonzero]/df.IOT2014$Capital.formation[nonzero]

nonzero <- which(!df.IOT2014$Changes.in.inventories[1:n]==0)
s.d.chi[nonzero] <- df.IOT2014.dom$Changes.in.inventories[nonzero]/df.IOT2014$Changes.in.inventories[nonzero]

nonzero <- which(!df.IOT2014$Exports[1:n]==0)
s.m.exp[nonzero] <- df.IOT2014.imp$Exports[nonzero]/df.IOT2014$Exports[nonzero]

nonzero <- which(!df.IOT2014$Household[1:n]==0)
s.m.cons[nonzero] <- df.IOT2014.imp$Household[nonzero]/df.IOT2014$Household[nonzero]

nonzero <- which(!df.IOT2014$General.Government[1:n]==0)
s.m.gov[nonzero] <- df.IOT2014.imp$General.Government[nonzero]/df.IOT2014$General.Government[nonzero]

nonzero <- which(!df.IOT2014$Capital.formation[1:n]==0)
s.m.gfcf[nonzero] <- df.IOT2014.imp$Capital.formation[nonzero]/df.IOT2014$Capital.formation[nonzero]

nonzero <- which(!df.IOT2014$Changes.in.inventories[1:n]==0)
s.m.chi[nonzero] <- df.IOT2014.imp$Changes.in.inventories[nonzero]/df.IOT2014$Changes.in.inventories[nonzero]

# the share importation vs. domestic production in total demand 
### (assuming weights of components of demand (eg. export, conso etc.) as in reference year)

fd    <- df.IOT2014$Exports[1:n] + df.IOT2014$Household[1:n] + df.IOT2014$General.Government[1:n] + df.IOT2014$Capital.formation[1:n] + df.IOT2014$Changes.in.inventories[1:n]
fd.d  <- df.IOT2014.dom$Exports[1:n] + df.IOT2014.dom$Household[1:n] + df.IOT2014.dom$General.Government[1:n] + df.IOT2014.dom$Capital.formation[1:n] + df.IOT2014.dom$Changes.in.inventories[1:n]

s.d   <- fd.d/fd        # share domestic    #fd : final demand #fd.d # final demand met with domestic goods

# the ratios to decompose output in va etc :

vect.decompOutput <- c("Compensation of employees","Gross value added","Other taxes less subsidies","Net taxes on products","Gross operating surplus","Total")

employment.per.production     <- df.NE.50I.QLFS$`201406`                                       /df.IOT2014$Output[1:n]
wages.per.production          <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[1],1:n+2]/df.IOT2014$Output[1:n]
VA.per.production             <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[2],1:n+2]/df.IOT2014$Output[1:n]
taxesOther.per.production     <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[3],1:n+2]/df.IOT2014$Output[1:n]
taxesProd.per.production      <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[4],1:n+2]/df.IOT2014$Output[1:n]
GOSurplus.per.production      <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[5],1:n+2]/df.IOT2014$Output[1:n]
totalInputs.per.production    <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[6],1:n+2]/df.IOT2014$Output[1:n] 
domesticInputs.per.production <- colSums(df.IOT2014.dom[1:50,1:n+3])/df.IOT2014$Output[1:n]
importedInputs.per.production <- colSums(df.IOT2014.imp[1:50,1:n+3])/df.IOT2014$Output[1:n]
  
NE.per.production <- employment.per.production
WS.per.production <- wages.per.production 
VA.per.production <- VA.per.production
TO.per.production <- taxesOther.per.production
TP.per.production <- taxesProd.per.production 
OS.per.production <- GOSurplus.per.production 
TI.per.production <- totalInputs.per.production
DI.per.production <- domesticInputs.per.production
MI.per.production <- importedInputs.per.production
# Some much used vectors with industries descriptions and codes, etc.

Order     <- df.IOT2014$Order[1:n]
Industry  <- df.NE.50I$Industry.description
SIC       <- df.NE.50I$SIC.code

# Some colours 

coloursDecomp.colour <- c("#5d6d7e","#f1c40f","#3498db","#e74c3c","#af601a")
#coloursDecomp.BW <- c("#5d6d7e","#f1c40f","#3498db","#e74c3c","#af601a")
coloursDecomp.BW  <- c("gray25","gray80","gray10","gray60","gray40")
coloursDecomp     <- coloursDecomp.BW
 
############################################
########## fixing of the parameter values for the demand-pull model, based on the IOT, domestic and import + employment data, version 2
#################### same as the above, except for that for coal, output/production is based on mt coal produced/used, not on monetary data
#################### this is necessary becasue the domestic price is so widely different from the international price
############################################
# source, facts and figures 2018, Minerals Council SA
# coal production in 10³ tonnes 2014 = 261949 
# coal domestically sold in 10³ tonnes 2014 = 184416
# exported coal in 10^3 tonnes 2014 = 75823, total salesrev 51452.471(Mc), 63581.9 (IOT) => price per tonne : 0.6785866 10^6 R (Mc) or 0.83856 (IOT) 
# imported coal in tonnes, assumed that price is equal to export price :
# 417.1633 mR => 497.5 10³ tonnes
# domestically sold coal, in value : 114230.414  - 63581.9 = 50649.41
# domestic price : 50649.41 / 184416 = 0.2746476

######## Construction of IOT with the coal row in volume

df.IOT2014.dom.q <- df.IOT2014.dom

df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Output"]                   <- 261.949
df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) =="I16"]                      <- 31
df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="I33"]                      <- 128
df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) =="Exports"]                  <- 75.823
#df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Imports"]    <- p.e^(-1) * df.IOT2014.dom[4,colnames(df.IOT2014.dom)=="Imports"]
df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Household"]                <- 2
df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="General.Government"]       <- 0
df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Capital.formation"]        <- 0
df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Changes.in.inventories"]   <- 2

p.e <- df.IOT2014.dom[4,colnames(df.IOT2014.dom) =="Exports"]   / df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) =="Exports"]  

Ind.notCTLelec      <- c("Order","SIC.code","Industry.description","Changes.in.inventories","Output","I16","I33","Exports","Household","General.Government","Capital.formation")
tons.ind.notCTLelec <- df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Output"] -
                       df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) =="I16"] -
                       df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="I33"]  -
                       df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) =="Exports"] -
                       df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Household"] -
                       df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q)=="Changes.in.inventories"]
                          
df.IOT2014.dom.q[4, !colnames(df.IOT2014.dom.q) %in% Ind.notCTLelec] <- df.IOT2014.dom.q[4, !colnames(df.IOT2014.dom.q) %in% Ind.notCTLelec] * tons.ind.notCTLelec/sum(df.IOT2014.dom.q[4, !colnames(df.IOT2014.dom.q) %in% Ind.notCTLelec])



# df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q) %in% c(Order,"Household","General.Government","Capital.formation","Changes.in.inventories")] <- 
#             p.d^(-1)   * df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) %in% c(Order,"Household","General.Government","Capital.formation","Changes.in.inventories")] 
# 
# df.IOT2014.dom.q[4, colnames(df.IOT2014.dom.q) %in% c("Exports","Imports")] <- 
#             p.e^(-1)   * df.IOT2014.dom.q[4,colnames(df.IOT2014.dom.q) %in% c("Exports","Imports")]
# 
# df.IOT2014.dom.q[4,"Output"]       <- sum(df.IOT2014.dom.q[4,c(Order,"Exports","Household","General.Government","Capital.formation","Changes.in.inventories")])
# 
df.IOT2014.imp.q <- df.IOT2014.imp
 
df.IOT2014.imp.q[4, colnames(df.IOT2014.imp.q) %in% c(Order,"Exports","Household","General.Government","Capital.formation","Changes.in.inventories","Imports")] <- 
             p.e^(-1)   * df.IOT2014.imp.q[4,colnames(df.IOT2014.imp.q) %in% c(Order,"Exports","Household","General.Government","Capital.formation","Changes.in.inventories","Imports")]

df.IOT2014.q                              <- df.IOT2014
df.IOT2014.q[4,Order]                     <- df.IOT2014.dom.q[4,Order] + df.IOT2014.imp.q[4,Order]
df.IOT2014.q[4,"Total.Industry"]          <- sum(df.IOT2014.q[4,Order])
df.IOT2014.q[4,"Household"]               <- df.IOT2014.dom.q[4,"Household"] + df.IOT2014.imp.q[4,"Household"] 
df.IOT2014.q[4,"Exports"]                 <- df.IOT2014.dom.q[4,"Exports"] + df.IOT2014.imp.q[4,"Exports"] 
df.IOT2014.q[4,"Imports"]                 <- - df.IOT2014.imp.q[4,"Imports"] 
df.IOT2014.q[4,"Changes.in.inventories"]  <- df.IOT2014.dom.q[4,"Changes.in.inventories"] + df.IOT2014.imp.q[4,"Changes.in.inventories"]
df.IOT2014.q[4,"Capital.formation"]       <- df.IOT2014.dom.q[4,"Capital.formation"] + df.IOT2014.imp.q[4,"Capital.formation"]
df.IOT2014.q[4,"General.Government"]      <- df.IOT2014.dom.q[4,"General.Government"] + df.IOT2014.imp.q[4,"General.Government"]
df.IOT2014.q[4,"Output"]                  <- sum(df.IOT2014.q[4,c(Order,"Exports","Household","General.Government","Capital.formation","Changes.in.inventories","Imports")])

#the direct requirement matrices (m&d) (=the A matrices) with tons for coal  

Z.q     <- as.matrix(df.IOT2014.q[1:n,(1:n)+2])  
Z.d.q   <- as.matrix(df.IOT2014.dom.q[1:n,(1:n)+3])
Z.m.q   <- as.matrix(df.IOT2014.imp.q[1:n,(1:n)+3])

A.q     <- t(t(Z.q)   / df.IOT2014.q$Output[1:n])
A.d.q   <- t(t(Z.d.q) / df.IOT2014.q$Output[1:n])
A.m.q   <- t(t(Z.m.q) / df.IOT2014.q$Output[1:n])

# the total requirement matrices (d)     (=the Leontief inverse)

L.q     <- diag(x = 1, nrow=n, ncol = n) - A.q
L.d.q   <- diag(x = 1, nrow=n, ncol = n) - A.d.q
LI.q    <- inv(as.matrix(L.q))
LI.d.q  <- inv(as.matrix(L.d.q))


fd.q    <- df.IOT2014.q$Exports[1:n] + df.IOT2014.q$Household[1:n] + df.IOT2014.q$General.Government[1:n] + df.IOT2014.q$Capital.formation[1:n] + df.IOT2014.q$Changes.in.inventories[1:n]
fd.d.q  <- df.IOT2014.dom.q$Exports[1:n] + df.IOT2014.dom.q$Household[1:n] + df.IOT2014.dom.q$General.Government[1:n] + df.IOT2014.dom.q$Capital.formation[1:n] + df.IOT2014.dom.q$Changes.in.inventories[1:n]

s.d.q   <- fd.d.q/fd.q        # share domestic    #fd : final demand #fd.d # final demand met with domestic goods

# the ratios to decompose output in va etc :

vect.decompOutput <- c("Compensation of employees","Gross value added","Other taxes less subsidies","Net taxes on products","Gross operating surplus","Total")

employment.per.production.q      <- df.NE.50I.QLFS$`201406`                                           /df.IOT2014.q$Output[1:n]
wages.per.production.q           <- df.IOT2014.q[df.IOT2014.q$Description==vect.decompOutput[1],1:n+2]/df.IOT2014.q$Output[1:n]
#VA.per.production             <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[2],1:n+2]/df.IOT2014.q$Output[1:n]
#taxesOther.per.production     <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[3],1:n+2]/df.IOT2014.q$Output[1:n]
#taxesProd.per.production      <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[4],1:n+2]/df.IOT2014.q$Output[1:n]
#GOSurplus.per.production      <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[5],1:n+2]/df.IOT2014.q$Output[1:n]
#totalInputs.per.production    <- df.IOT2014.q[df.IOT2014.q$Description==vect.decompOutput[6],1:n+2]/df.IOT2014.q$Output[1:n] 
#domesticInputs.per.production <- colSums(df.IOT2014.dom[1:50,1:n+3])/df.IOT2014.q$Output[1:n]
#importedInputs.per.production <- colSums(df.IOT2014.imp[1:50,1:n+3])/df.IOT2014.q$Output[1:n]

NE.per.production.q <- employment.per.production.q
WS.per.production.q <- wages.per.production.q 
# VA.per.production <- VA.per.production
# TO.per.production <- taxesOther.per.production
# TP.per.production <- taxesProd.per.production 
# OS.per.production <- GOSurplus.per.production 
# TI.per.production <- totalInputs.per.production
# DI.per.production <- domesticInputs.per.production
# MI.per.production <- importedInputs.per.production

#Average over last four years according to Minerals Council South Africa numbers (2014-2017) 
NE.per.production.q[4] <- 317












