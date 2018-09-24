

## Coal, 50% less export
# The basic equation is ECT = E LI.d S.d (C + G + I + Inv + X)

# EC is obtained through setting (C + G + I + Inv + X) for each sector = 1
# If X' = 50% X, then impact on EC : EC' = EC [ E LI.d S.d (C + G + I + Inv + X')/(C + G + I + Inv + X) ]


final.uses.d <- c("Household","General.Government","Capital.formation","Changes.in.inventories")
final.uses.e <- c("Exports")
final.uses   <- c(final.uses.e,final.uses.d)
intermediate.uses <- c("Total.Industry")
total.supply <- c("Imports","Output")


fu.t        <- df.IOT2014[1:50,c("Description",final.uses)]
fu.t$total.demand <- rowSums(fu.t[,-1])


# total employment direct indirect (te.d.i) 
df.te.d.i           <- data.frame(ec.d.i %*% diag(fu.t$total.demand))
colnames(df.te.d.i) <- df.IOT2014$Description[1:50]
rownames(df.te.d.i) <- df.IOT2014$Description[1:50]

te.rs     <- data.frame(rowSums(te.d.i))
colnames(te.rs) <- "Number.Employed"
rownames(te.rs) <- df.IOT2014$Description[1:50]


## Demand vector of scenario 50% less exports of coal 

sector <- 'Coal and lignite'

fu.t.scen1          <- fu.t %>% select(-total.demand)
fu.t.scen1[fu.t.scen1$Description==sector,"Exports"] <- fu.t.scen1[fu.t.scen1$Description==sector,"Exports"] * 0.5
fu.t.scen1$total.demand <- rowSums(fu.t.scen1[,-1])

             # How many jobs does this support ? 
             # CPI mentioned 30.000 jobs by the end of the period, growth prospect for the country, necessary for internal demand ? 
             # demand for local production would grow ? 

# total employment direct indirect (te.d.i) for this 50% export scenario
df.te.scen1.d.i           <- data.frame(ec.d.i %*% diag(fu.t.scen1$total.demand))
colnames(df.te.scen1.d.i) <- df.IOT2014$Description[1:50]
rownames(df.te.scen1.d.i) <- df.IOT2014$Description[1:50]

te.scen1.rs     <- data.frame(rowSums(df.te.scen1.d.i))
colnames(te.scen1.rs) <- "Number.Employed"
rownames(te.scen1.rs) <- df.IOT2014$Description[1:50]



