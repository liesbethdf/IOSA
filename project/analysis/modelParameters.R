
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

L.d   <- diag(x = 1, nrow=n, ncol = n) - A.d
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

vect.decompOutput <- c("Compensation of employees","Gross value added","Other taxes less subsidies","Net taxes on products","Gross operating surplus")

employment.per.production <- df.NE.50I.QLFS$`201406`                                       /df.IOT2014$Output[1:n]
wages.per.production      <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[1],1:n+2]/df.IOT2014$Output[1:n]
VA.per.production         <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[2],1:n+2]/df.IOT2014$Output[1:n]
taxesOther.per.production <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[3],1:n+2]/df.IOT2014$Output[1:n]
taxesProd.per.production  <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[4],1:n+2]/df.IOT2014$Output[1:n]
GOSurplus.per.production  <- df.IOT2014[df.IOT2014$Description==vect.decompOutput[5],1:n+2]/df.IOT2014$Output[1:n]

NE.per.production <- employment.per.production
WS.per.production <- wages.per.production 
VA.per.production <- VA.per.production
TO.per.production <- taxesOther.per.production
TP.per.production <- taxesProd.per.production 
OS.per.production <- GOSurplus.per.production 


# Some much used vectors with industries descriptions and codes, etc.

Order     <- df.IOT2014$Order[1:n]
Industry  <- df.NE.50I$Industry.description
SIC       <- df.NE.50I$SIC.code







