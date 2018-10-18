
## Table with the top users of intermediate input

Intermediate.Input.usedby <- function(sector, depth)
{
  sector.char <- as.character(sector)
  sector.name <- df.IOT2014$Description[sector]
  
  df.sect <- df.IOT2014
  colnames(df.sect)[3:52] <- df.sect$Description[1:50]
  df.sect <- df.sect[sector,]
  
  df.sect <- as.data.frame(t(df.sect[,3:(dim(df.sect)[2]-7)]))
  df.sect <- df.sect/df.sect[51,1]*100
  
  sortVector <- order(-df.sect)
  
  df.sect$Industry <- rownames(df.sect)
  df.sect <- df.sect[sortVector,]
  df.sect <- df.sect
  df.sect$accumul <- 0
  
  for(i in 2:51){df.sect$accumul[i] = sum(df.sect[2:i,colnames(df.sect)==sector])}
  
  df.sect <- df.sect[-1,]
  
  colnames(df.sect) <- c("% intermediate inputs","Industry","%, accumulated")
  
  table <- round(df.sect[1:depth,c(1,3)],1)
  
  setwd(dir.DESCRIPT)
  writeout.table      <- xtable(table, digits=c(0,1,1))
  fileName.table      <- paste(paste(sector,gsub(" ","-",sector.name),"IIusedBy",sep="_"), "tex", sep=".")
  print(writeout.table, file=fileName.table)
  
}

for(i in c(4,33)){Intermediate.Input.usedby(i, 10)}

## The distribution of the cost of an input over the economy

sector <- 33

Intermediate.Input.HH.plot.usedby <- function(sector)
{

df.costShare <- data.frame(A[sector,]*100)
colnames(df.costShare) <- "Value"
df.costShare$Industry <- Industry
df.HH <- data.frame(t(c(2.56,"Households")))
colnames(df.HH) <- colnames(df.costShare)
df.HH$Value <- as.numeric(df.HH$Value)
df.HH$Value[1] <- 2.56
df.plot <- bind_rows(df.costShare, df.HH)
df.plot$Industry <- factor(df.plot$Industry, levels=c(Industry,"Households"))

p <- df.plot %>% ggplot(aes(x="", y=Value, fill=Industry))+
                  geom_bar(stat = "identity", colour="black") +
                  coord_polar("y", start=0) +
                  guides(fill=guide_legend(override.aes=list(colour=NA)))

# To condensate the legend : https://stackoverflow.com/questions/21801950/remove-legend-entries-for-some-factors-levels
print(p)
}