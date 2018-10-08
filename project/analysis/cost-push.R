
############################################
############ Evaluate how cost increase coal spreads throughout the economy by asuming it is translated in price increase
############ Cost-push model
############################################

d <- 2      # number of descriptive columns at beginning of IOT
exo.input <- c("Coal and lignite")

exo.n <- length(exo.input)

exo.ind <- which(df.IOT2014$Description == exo.input, arr.ind=TRUE)
#exo.col <- which(colnames(df.IOT2014) == df.IOT2014[exo.row,"Order"], arr.ind=TRUE)

# Write interindustry matrix without exo.input :

df.IOTHere  <- df.IOT2014[-exo.ind,-(exo.ind + d)]
Z           <- as.matrix(df.IOTHere[1:(n-exo.n), 1:(n-exo.n) + d])

A           <- t(t(Z)/df.IOTHere$Output[1:(n-exo.n)])

L           <- diag(x = 1, nrow=n-exo.n, ncol = n-exo.n) - A

LI          <- inv(as.matrix(L))

# output = total + gross value added + net taxes 
# gross value added = Compensation of Employees + Taxes + Gross operating surplus
# coal was taken out

vars.VA    <- c("Net taxes on products","Gross value added")

vector.exo <- df.IOT2014[exo.row,1:n + d]
vector.exo <- vector.exo[,-exo.row]

vector.VA  <- colSums(df.IOTHere[df.IOTHere$Description %in% vars.VA, 1:(n-exo.n) + d]) + vector.exo

VA.per.production <- vector.VA/df.IOTHere$Output[1:(n-exo.n)]

prices <-  as.matrix(VA.per.production) %*% LI

############ Exo price increase

p.increase <- 0.10

vector.VA.scen  <- colSums(df.IOTHere[df.IOTHere$Description %in% vars.VA, 1:(n-exo.n) + d]) + vector.exo * (1 + p.increase)

VA.per.production.scen <- vector.VA.scen/df.IOTHere$Output[1:(n-exo.n)]

prices <-  as.matrix(VA.per.production.scen) %*% LI




















