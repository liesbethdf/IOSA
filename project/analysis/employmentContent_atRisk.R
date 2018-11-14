

############################################
########## Graphs that give idea of decomposition of direct and indirect employment content, without summing over sectors.
############################################
yearHere      <- "2014"
sectorHere    <- 4
Industry.code <- paste(Industry, SIC,sep = " ")

df.resultsHere <- employment.content.direct.indirect[,4]

df.resultsHere                    <- data.frame(Industry.code)
colnames(df.resultsHere)          <- "Industry"
df.resultsHere$ec                 <- employment.content.direct.indirect[,sectorHere]
df.resultsHere$ec.diff            <- ec.d.i[,sectorHere] - ec.m.m[,sectorHere]
df.resultsHere$ec.dev.decomp.T    <-  T.dist[,sectorHere]
df.resultsHere$ec.dev.decomp.L    <-  L.dist[,sectorHere]
df.resultsHere$ec.dev.decomp.N    <-  N.dist[,sectorHere]
df.resultsHere$ec.dev.decomp.MF   <-  MF.dist[,sectorHere]
df.resultsHere$ec.dev.decomp.MI   <-  MI.dist[,sectorHere]

#orderHere         <- order(-df.resultsHere$ec)
#IndustryHere      <- df.resultsHere$Industry[orderHere]

orderHere         <- order(-df.resultsHere$ec.diff)
IndustryHere      <- df.resultsHere$Industry[orderHere]


df.plot           <- df.resultsHere
df.plot$Industry  <- factor(df.plot$Industry, levels=IndustryHere)
df.plot           <- df.plot[order(df.plot$Industry),]

df.plot$ec.diff.sign <- "neg"
df.plot[df.plot$ec.diff > 0,"ec.diff.sign"] <- "pos"
df.plot[rownames(df.plot)==sectorHere,"ec.diff.sign"] <- paste0("I",sectorHere)

df.sum <- setNames(data.frame(matrix(ncol=9,nrow = 3)), colnames(df.plot))
IndustriesHere <- c("Transport, Trade, \nComp., Gen. Machinery, \nStruct. Metal, Finance",
                    "Manufacturing, \nElec, Services, etc.",
                    "Coal and lignite")

signs <- unique(df.plot$ec.diff.sign)
for (i in 1:3) 
{
  df.sum[i,-c(1,dim(df.plot)[2])] <- colSums(df.plot[df.plot$ec.diff.sign==signs[i],-c(1,dim(df.plot)[2])])
  df.sum$ec.diff.sign[i] <- signs[i]
  df.sum$Industry[i] <- IndustriesHere[i]
}

df.plot <- df.sum[,-dim(df.sum)[2]]

df.plot <- df.plot %>% gather(Decomposition,Value, ec.dev.decomp.T, ec.dev.decomp.L, ec.dev.decomp.N, ec.dev.decomp.MF, ec.dev.decomp.MI, factor_key = TRUE)

#df.plot$Value <- df.plot$Value + 2.24

levels(df.plot$Decomposition) <- c("Taxes", "Labour \nshare","1/Wages", "Local demand \nfinal products","Local demand \ninputs")
df.plot$Decomposition <- factor(df.plot$Decomposition, levels=c("1/Wages","Labour \nshare","Taxes","Local demand \ninputs", "Local demand \nfinal products"))

#coloursDecomp <- c("#5d6d7e","#f1c40f","#3498db","#e74c3c","#af601a")

p <- df.plot %>% ggplot(aes(x=Industry, y=Value, fill=Decomposition)) +
                 geom_bar(stat="identity") +
                 ylab(paste0("Employment content of sector \"Coal and lignite\" against average")) +
                # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                #theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
                 labs(x = "") +
                 scale_x_discrete(limits = rev(levels(df.plot$Industry))) +
                 scale_y_continuous(labels = function(x) x + 2.24) +
                 coord_flip() +
                 labs(fill="Decomposition :") +
                 theme(legend.position="bottom") +
                 scale_fill_manual(values=coloursDecomp) 
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ECdecomposition-coal",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=20, height=6, units="cm", dpi=300)

setwd(dir.ANALYSIS)


