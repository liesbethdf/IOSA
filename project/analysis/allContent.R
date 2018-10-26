
############################################
############ Decomposition of output into taxes, labour and capital share
############################################

vect.exp  <-  df.IOT2014$Exports[1:n] 
vect.cons <-  df.IOT2014$Household[1:n] 
vect.gov  <-  df.IOT2014$General.Government[1:n] 
vect.gfcf <-  df.IOT2014$Capital.formation[1:n]
vect.chi  <-  df.IOT2014$Changes.in.inventories[1:n]

demand.d  <-  diag(s.d.exp) %*% diag(vect.exp) +
              diag(s.d.cons) %*% diag(vect.cons) +
              diag(s.d.gov) %*% diag(vect.gov) +
              diag(s.d.gfcf) %*% diag(vect.gfcf) +
              diag(s.d.chi) %*% diag(vect.chi)

vect.exp.scen    <- vect.exp
vect.exp.scen[4] <- vect.exp[4] * 0.5

demand.d.scen  <-  diag(s.d.exp) %*% diag(vect.exp.scen) +
              diag(s.d.cons) %*% diag(vect.cons) +
              diag(s.d.gov) %*% diag(vect.gov) +
              diag(s.d.gfcf) %*% diag(vect.gfcf) +
              diag(s.d.chi) %*% diag(vect.chi)

demand.d.diff <- demand.d - demand.d.scen
sum(demand.diff)
#employment.per.production <- df.NE.50I.QLFS$`201406`/df.IOT2014$Output[1:n]
wages.per.production      <- df.IOT2014[df.IOT2014$Description=="Compensation of employees",1:n+2]/df.IOT2014$Output[1:n]
VA.per.production         <- df.IOT2014[df.IOT2014$Description=="Gross value added",1:n+2]/df.IOT2014$Output[1:n]
taxesOther.per.production <- df.IOT2014[df.IOT2014$Description=="Other taxes less subsidies",1:n+2]/df.IOT2014$Output[1:n]
taxesProd.per.production  <- df.IOT2014[df.IOT2014$Description=="Net taxes on products",1:n+2]/df.IOT2014$Output[1:n]
GOSurplus.per.production  <- df.IOT2014[df.IOT2014$Description=="Gross operating surplus",1:n+2]/df.IOT2014$Output[1:n]

wages.di      <- diag(wages.per.production)     %*% LI.d %*% demand.d
VA.di         <- diag(VA.per.production)        %*% LI.d %*% demand.d
taxesOther.di <- diag(taxesOther.per.production)%*% LI.d %*% demand.d
taxesProd.di  <- diag(taxesProd.per.production) %*% LI.d %*% demand.d
GOSurplus.di  <- diag(GOSurplus.per.production) %*% LI.d %*% demand.d

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

wages.di.diff      <- diag(wages.per.production)     %*% LI.d %*% demand.d.diff
VA.di.diff         <- diag(VA.per.production)        %*% LI.d %*% demand.d.diff
taxesOther.di.diff <- diag(taxesOther.per.production)%*% LI.d %*% demand.d.diff
taxesProd.di.diff  <- diag(taxesProd.per.production) %*% LI.d %*% demand.d.diff
GOSurplus.di.diff  <- diag(GOSurplus.per.production) %*% LI.d %*% demand.d.diff

output.d.di.diff     <-  LI.d %*% demand.d.diff

rowSums(wages.di.diff)
rowSums(VA.di.diff)
rowSums(taxesOther.di.diff)
rowSums(taxesProd.di.diff)
rowSums(GOSurplus.di.diff)

colSums(wages.di.diff)
colSums(VA.di.diff)
colSums(taxesOther.di.diff)
colSums(taxesProd.di.diff)
colSums(GOSurplus.di.diff)

############################################
############ Importations
############################################

Imports.M <- abs(df.IOT2014$Imports[1:n])
Imports.M[Imports.M==0] <- 0.01

MA.m   <- t(t(Z.m)/Imports.M)
ML.m   <- diag(x = 1, nrow=n, ncol = n) - MA.m
MLI.m  <- inv(as.matrix(ML.m))


demand.m  <-  diag(s.m.exp) %*% diag(vect.exp) +
              diag(s.m.cons) %*% diag(vect.cons) +
              diag(s.m.gov) %*% diag(vect.gov) +
              diag(s.m.gfcf) %*% diag(vect.gfcf) +
              diag(s.m.chi) %*% diag(vect.chi)

vect.exp.scen    <- vect.exp
vect.exp.scen[4] <- vect.exp[4] * 0.5

demand.m.scen  <- diag(s.m.exp) %*% diag(vect.exp.scen) +
                  diag(s.m.cons) %*% diag(vect.cons) +
                  diag(s.m.gov) %*% diag(vect.gov) +
                  diag(s.m.gfcf) %*% diag(vect.gfcf) +
                  diag(s.m.chi) %*% diag(vect.chi)

demand.m.diff     <- demand.m - demand.m.scen
output.m.di.diff  <- MLI.m %*% demand.m.diff

###################################################################################

A.all     <- t(t(Z)  /(df.IOT2014$Output[1:n]+abs(df.IOT2014$Imports[1:n])))
A.all.d   <- t(t(Z.d)/(df.IOT2014$Output[1:n]+abs(df.IOT2014$Imports[1:n])))
A.all.m   <- t(t(Z.m)/(df.IOT2014$Output[1:n]+abs(df.IOT2014$Imports[1:n])))

L.all     <- diag(x = 1, nrow=n, ncol = n) - A.all
L.all.d   <- diag(x = 1, nrow=n, ncol = n) - A.all.d
L.all.m   <- diag(x = 1, nrow=n, ncol = n) - A.all.m

LI.all    <- inv(as.matrix(L.all))
LI.all.d  <- inv(as.matrix(L.all.d))
LI.all.m  <- inv(as.matrix(L.all.m))

demand.scen  <- diag(vect.exp.scen) +
                diag(vect.cons) +
                diag(vect.gov) +
                diag(vect.gfcf) +
                diag(vect.chi)

demand.diff <- diag(demand) - demand.scen

output.di.diff.all    <-  LI.all  %*% demand.diff
output.d.di.diff.all  <-  LI.all.d  %*% demand.diff
output.m.di.diff.all  <-  LI.all.m  %*% demand.diff - demand.diff


rest <- output.di.diff.all - output.d.di.diff.all - output.m.di.diff.all

sum(rest) 
sum(output.d.di.diff.all)
sum(output.m.di.diff.all)

sum(output.di.diff.all-demand.diff)

sum(output.d.di.diff.all-demand.diff)

output.d.di.diff  <-  LI.d %*% demand.d.diff
output.di.diff  <-  LI.all   %*% demand.diff

output.m.di.diff.test <- output.di.diff.all - output.d.di.diff


