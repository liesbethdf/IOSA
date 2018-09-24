

df.NumberEmployees[df.NumberEmployees$Industry.description=="TOTAL",colnames(df.NumberEmployees)=="SIC.code"] <- "TOTAL"

df.plot <- df.NumberEmployees[df.NumberEmployees$SIC.code %in% c("2","3","4","5","6","7","8","9"),]
# df.plot <- df.NumberEmployees[df.NumberEmployees$SIC.code %in% c("2","3","4","5","6","7","8","9","TOTAL"),]

df.plot.temp <- df.plot %>% gather(Time, Value, -Industry.description, -SIC.code, -var)
df.plot.temp$Industry.description <- as.factor(df.plot.temp$Industry.description)
df.plot.temp$SIC.code <- as.factor(df.plot.temp$SIC.code)
df.plot.temp$Time <- as.numeric(df.plot.temp$Time)

p <- ggplot(df.plot.temp, aes(x=Time, y=Value, colour=Industry.description )) +
    geom_path(size=0.8)+
    scale_y_continuous()

#  facet_wrap(~ Variable, ncol=2, scales="free") +
#  scale_color_brewer(palette = "Spectral")
plot(p)


#####################################################################################""

## bar plots with the uses and resources per industry side by side

df.plot             <- df.IOT2014[1:50,c("Description","Total.Industry","Exports","Imports","Household","General.Government","Capital.formation","Changes.in.inventories","Output")]
df.plot$Imports     <- abs(df.plot$Imports)
df.plot$`Capital formation`   <- df.plot$Capital.formation + df.plot$Changes.in.inventories
df.plot$Consumption <- df.plot$Household + df.plot$General.Government
df.plot$SIC         <- df.NE.50I$SIC.code
df.plot$Industry.code <- paste(df.plot$Description, df.plot$SIC,sep = " ")

#colnames(df.plot)[which(names(df.plot) == "Total.Industry")] <- "Intermediate inputs"
colnames(df.plot)[colnames(df.plot) == "Total.Industry"] <- "Intermediate inputs"

Industry.code  <- df.plot$Industry.code

df.plot$Industry.code <- factor(df.plot$Industry.code,levels=Industry.code)


df.plot       <- df.plot %>% gather(Aggregate, Value, -Description, -SIC, -Industry.code)
side          <- c("Uses","Resources")
df.plot$side  <- side[1]
df.plot[df.plot$Aggregate %in% c("Imports","Output"),"side"]  <- side[2]
df.plot$side <- factor(df.plot$side)
df.plot$side <- relevel(df.plot$side,"Uses")

df.plot$Aggregate <- factor(df.plot$Aggregate, levels=c("Exports","Consumption","General.Government","Household",
                                                        "Capital formation","Changes.in.inventories","Capital.formation",
                                                        "Intermediate inputs","Imports","Output"))

# unique(df.plot[df.plot$side %in% c("Resources"),"Aggregate"]) #check
# unique(df.plot[df.plot$side %in% c("Uses"),"Aggregate"]) #check

df.plot.total <- df.plot

df.plot <- df.plot[df.plot$Aggregate %in% c("Exports", "Consumption", "Capital formation", "Intermediate inputs","Imports","Output"),]

#Ind <- "Coal and lignite"
#Ind <- "General machinery"
Ind <- df.plot$Industry.code

col <- c("#B2182B","#D1E5F0","#FDDBC7","#EF8A62","#2166AC","#67A9CF")



p <- df.plot[df.plot$Industry.code==Ind,] %>% ggplot(aes(x=side, y=Value, fill=Aggregate)) +
                                          geom_bar(stat="identity") +
                                          ylab("million Rand") +
                                          #xlab("") +
                                          facet_wrap( ~ Industry.code, ncol=10, scales="free") +
                                          theme(legend.position="bottom") +
                                          theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
                                          #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#E69F00", "#999999"))
                                          #scale_fill_brewer(palette="PuOr")
                                          scale_fill_manual(values=col)

print(p)

p <- df.plot[df.plot$Industry==Ind,] %>% ggplot(aes(x=side, y=Value, fill=Aggregate)) +
  geom_bar(stat="identity") +
  ylab("million Rand") +
  #xlab("") +
  facet_wrap( ~ Industry, ncol=10) +
  theme(legend.position="bottom") +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#E69F00", "#999999"))
  scale_fill_brewer(palette="BrBG")

print(p)


setwd(dir.PLOTS)
fileName.graph <- paste("Uses_Resources",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=36, height=28, units="cm", dpi=300)
