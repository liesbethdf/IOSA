
############################################
############ graphs number of jobs
############################################

resultsHereAll <- finaldemandDecomposition(df.domesticDemand.input)

resultsHereCoalExport <- finaldemandDecomposition(df.domDemand.coal.input.v2)


outputDecomp.All      <- resultsHereAll[[1]][[2]]
outputDecomp.CE       <- resultsHereAll[[1]][[2]]

resultsNemployees.All <- resultsHereAll[[10]][[2]]
resultsNemployees.CE  <- resultsHereCoalExport[[10]][[2]]

valueaddedDecomp.CE    <- resultsHereCoalExport[[3]][[2]]


############ graph for employment resulting from coal export :
plot.decompSectors <- function(scenarioReference, detail, df.resultsHere, colours)
{
#scenarioReference <- "BAU"
#  detail <- 10
#  df.resultsHere <- resultsNemployees.CE
  
  df.plot <- df.resultsHere  %>% select(I4, Industry, Year, Var, Case) %>%
    spread(Industry,I4)
  
  orderHere <- order(-df.plot[df.plot$Year=="2018" & df.plot$Case==scenarioReference,1:50 +3])
  
  df.plot <- df.plot[,c(1,2,3,orderHere+3)]
  nHere   <- n-detail
  df.plot$Other.sectors <- rowSums(df.plot[,1:nHere +detail+3])
  
  cut1 <- detail + 3
  cut2 <- n - cut1
  end1 <- n - detail
  
  reorder.all     <- c(seq(1:cut1),dim(df.plot)[2],seq(1:end1) + cut1)
  reorder.select  <- c(seq(1:cut1),dim(df.plot)[2])
  
  df.plot.all     <-  df.plot[,reorder.all]
  levels.Industry <- c(colnames(df.plot)[1:10+3],"Other.sectors")
  df.plot <-  df.plot[,reorder.select] %>% gather(Industry, Value, - Year, -Var, -Case, factor_key = TRUE)
  
  
  ###### graph with BAU and 2Deg
  
  
#  levels.Industry <- colnames(df.plot)[1:10+3]#c("I4", "I38", "I36",  "I46",  "I28",  "I40",  "I27",  "I42",  "I18",  "I50",  "Other.sectors")
  numselec        <- as.numeric(substr(levels.Industry[-length(levels.Industry)],2,3))
  labels.Industry <- c(Industry[numselec],"Rest of industry")
  
  df.plot$Industry <- factor(df.plot$Industry, levels=levels.Industry, labels=labels.Industry)
  df.plot          <- df.plot[order(df.plot$Industry),]
  
  df.plot$Case <- factor(df.plot$Case, levels=c("BAU","2Deg"))
  df.plot          <- df.plot[order(df.plot$Case),]
  
  
  #colours <-c("#b7950b","#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72", "#5d6d7e")    
  #colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
  
  
  p <- df.plot %>% ggplot(aes(x=Year, y=Value, fill=Industry)) +
    geom_bar(stat="identity") +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Year", y="number of jobs") +
    #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
    #scale_y_continuous(limits = c(0, 100000)) +
    #coord_flip() +
    scale_fill_manual(values=colours) +
    facet_wrap( ~ Case, scales="fixed", ncol=2) +
    labs(fill="Sector :") 
  
  return(p)
}

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=resultsNemployees.CE, colours=colours)

print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("NumberEmployees_Secor","Coal-Export","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)


############ graph for value added distributed over sectors

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=valueaddedDecomp.CE, colours=colours)
plot(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ValueAdded_Secor","Coal-Export","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)


############ graph for number of jobs in the coal industry, sustained by export and by domestic demand

resultsNemployees.All <- resultsHereAll[[10]][[2]]

scenarioReference <- "BAU"
detail <- 10
df.resultsHere <- resultsNemployees.All

df.plot <- df.resultsHere  %>% filter(df.resultsHere$Industry=="I4") 






