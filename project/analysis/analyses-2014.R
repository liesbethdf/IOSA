############################################
############ analysis coal 2014 versus 50% less export
############################################
sector    <- c("Coal and lignite")
sector.nr <- c("4")

SA.2014.ec <- decomposition.direct.indirect(LI=LI.d, 
                                            O=df.IOT2014$Output[1:50], 
                                            VAR=df.NE.50I.QLFS$`201406`, 
                                            type="E", 
                                            n=50, 
                                            indList=df.NE.50I$Industry.description,
                                            indcodeList=df.NE.50I$SIC.code, 
                                            year=2014)

SA.2014.va <- decomposition.direct.indirect(LI=LI.d, 
                                            O=df.IOT2014$Output[1:50], 
                                            VAR=df.IOT2014[df.IOT2014$Description=="Gross value added",colnames(df.IOT2014) %in% paste0("I",seq(1:50))], 
                                            type="VA", 
                                            n=50, 
                                            indList=df.NE.50I$Industry.description,
                                            indcodeList=df.NE.50I$SIC.code, 
                                            year=2014)

df.SA.2014.ec.summary <- SA.2014.ec[[1]]
df.SA.2014.ec.matrix  <- SA.2014.ec[[2]]

df.SA.2014.va.summary <- SA.2014.va[[1]]
df.SA.2014.va.matrix  <- SA.2014.va[[2]]

############ decomposition va and employment in direct and indirect contributions in 2014

demand.2014  <- df.IOT2014$Exports[1:50] + 
              #df.IOT2014$Imports[1:50] + 
                df.IOT2014$Household[1:50] + 
                df.IOT2014$General.Government[1:50] + 
                df.IOT2014$Capital.formation[1:50] + 
                df.IOT2014$Changes.in.inventories[1:50]

output.ec <- top.indirect(SA.2014.ec , demand.2014, sector.nr, 7)
output.va <- top.indirect(SA.2014.va , demand.2014, sector.nr, 7)

table.ec <- output.ec[[1]]
table.va <- output.va[[1]]

fname.ec <- output.ec[[2]]
fname.va <- output.va[[2]]

fname.ec.split <- strsplit(fname.ec, "_")[[1]]
fname.va.split <- strsplit(fname.va, "_")[[1]]
elt            <- fname.ec.split[fname.ec.split != fname.va.split]
elt.index      <- match(elt,fname.ec.split)

fname          <- c(fname.va.split[1:(elt.index-1)], elt, fname.va.split[elt.index:length(fname.va.split)])
fileName.table <- paste(fname,collapse="_")

table          <- full_join(table.ec, table.va, by=c("Industry","Code"))

setwd(dir.TABLES)
writeout.table      <- xtable(table)
fileName.table      <- paste(fileName.table, "tex", sep=".")
print(writeout.table, file=fileName.table.scen)

############ decomposition va and employment in direct and indirect contributions in 2014, with 50% less export

demand.2014.50Exp  <- df.IOT2014$Exports[1:50] * 0.5 + 
                      #df.IOT2014$Imports[1:50] + 
                      df.IOT2014$Household[1:50] + 
                      df.IOT2014$General.Government[1:50] + 
                      df.IOT2014$Capital.formation[1:50] + 
                      df.IOT2014$Changes.in.inventories[1:50]

output.ec <- top.indirect(SA.2014.ec , demand.2014.50Exp, sector.nr, 7)
output.va <- top.indirect(SA.2014.va , demand.2014.50Exp, sector.nr, 7)

table.ec <- output.ec[[1]]
table.va <- output.va[[1]]

fname.ec <- output.ec[[2]]
fname.va <- output.va[[2]]

fname.ec.split <- strsplit(fname.ec, "_")[[1]]
fname.va.split <- strsplit(fname.va, "_")[[1]]
elt            <- fname.ec.split[fname.ec.split != fname.va.split]
elt.index      <- match(elt,fname.ec.split)

fname          <- c(fname.va.split[1:(elt.index-1)], elt, fname.va.split[elt.index:length(fname.va.split)])
fileName.table <- paste(fname,collapse="_")

table          <- full_join(table.ec, table.va, by=c("Industry","Code"))

setwd(dir.TABLES)
writeout.table      <- xtable(table)
fileName.table.scen <- paste(paste(fileName.table,"50Exp",sep="_"), "tex", sep=".")
print(writeout.table, file=fileName.table.scen)


Liesje <- data.frame(t(df.Empl.Exp[4,3:52]))
colnames(Liesje) = "E"
Liesje$Industry <- Industry
Liesje$SIC <- Code
Liesje[order(-Liesje$E),]











