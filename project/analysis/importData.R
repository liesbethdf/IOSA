
############################################
############ Import IOT STAT SA
############################################

fileName.list <- c("Final_Input_Output_tables_for_South_Africa_2013.xlsx", "Final_Input_Output_tables_for_South_Africa_2014.xlsx")

n <- 50 # number of sectors

############ 2013
i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.IOT2013 <- data.frame(read_excel(data, 
                                    sheet=2, 
                                    range = cell_limits(ul = c(2,1), lr = c(60, 60))))


############ 2014
i <- 2
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.IOT2014 <- data.frame(read_excel(data, 
                                    sheet=2, 
                                    range = cell_limits(ul = c(2,1), lr = c(60, 60))))

colnames(df.IOT2014)[1:n +2] <-df.IOT2014[1,1:n +2]

df.IOT2014 <- df.IOT2014[-1,]
rownames(df.IOT2014) <- seq(1:dim(df.IOT2014)[1])

# for (i in 3:dim(df.IOT2014)[2]) # get the commas out of the numbers
# {
#   for (j in 1:dim(df.IOT2014)[1])
#   {
#     df.IOT2014[j,i] <- gsub(",","",df.IOT2014[j,i])
#   }
# }

df.IOT2014[,3:dim(df.IOT2014)[2]] <- as.data.frame(sapply(df.IOT2014[,3:dim(df.IOT2014)[2]], as.numeric)) 

Industry.description.50 <- df.IOT2014$Description[1:50]

############################################
############ Import Employment data OECD
############################################

fileName.list <- c("Employment_SA_OECD_20180911.xls")
data <- paste(dir.DATA,fileName.list[1], sep = sep)

# df.Employment.OECD <- readWorksheetFromFile(data, 
#                                      sheet=1, 
#                                      startRow = 5,
#                                      startCol = 1)

df.Employment.OECD <- data.frame(read_excel(data, 
                                            sheet = 1, 
                                            range = cell_limits(ul = c(5,1), lr = c(36, 24))))

colnames(df.Employment.OECD) <- df.Employment.OECD[1,]
df.Employment.OECD <- df.Employment.OECD[-c(1,2),]

df.Employment.OECD[,grep("20", colnames(df.Employment.OECD))] <- df.Employment.OECD[,grep("20", colnames(df.Employment.OECD))+1]

cols.toDelete <- c(4,grep("20", colnames(df.Employment.OECD))+1)

df.Employment.OECD <- df.Employment.OECD[,-cols.toDelete]

for (i in 1:dim(df.Employment.OECD)[1])
{
  if(!is.na(df.Employment.OECD[i,3])==TRUE)
  {
    df.Employment.OECD[i,2] <- df.Employment.OECD[i,3] 
  }
}

df.Employment.OECD <- df.Employment.OECD[,-c(3)]
colnames(df.Employment.OECD)[1:2] <- c("Description1","Description2")
industries.ISIC <- c("A-B","C-E","D","F","G-I","J-K","L-P")
df.Employment.OECD$ISIC <- c(NA,NA,NA,NA,NA,industries.ISIC,NA,industries.ISIC,NA,industries.ISIC,NA)

df.Employment.OECD <- df.Employment.OECD[,c("Description1","Description2","ISIC",colnames(df.Employment.OECD)[-c(1,2,13)])]


############################################
############ Import QES data STAT SA | QES = survey from enterprises data
############################################
fileName.list <- c("QES_Details_BreakDown_200909_201803.xlsx")
data <- paste(dir.DATA,fileName.list[1], sep = sep)

# df.SICnotes <- readWorksheetFromFile(data, 
#                                      sheet=1, 
#                                      startRow = 115,
#                                      endRow = 125,
#                                      startCol = 1,
#                                      endCol = 4)

df.SICnotes <- read_excel(data, 
                          sheet = 1,
                          range = cell_limits(ul = c(115,1), lr = c(125, 1)))

# df.NE <- readWorksheetFromFile(data, 
#                                     sheet=1, 
#                                     startRow = 1,
#                                     endRow = 114,
#                                     startCol = 1,
#                                     endCol = 38)

df.NE <- data.frame(read_excel(data, 
                    sheet = 1, 
                    range = cell_limits(ul = c(1,1), lr = c(114, 37))))


# df.GrossEarnings <- readWorksheetFromFile(data, 
#                                             sheet=1, 
#                                             startRow = 1,
#                                             endRow = 114,
#                                             startCol = 39)

df.GrossEarnings <- data.frame(read_excel(data, 
                               sheet = 1, 
                               range = cell_limits(ul = c(1,39), lr = c(114, 73))))


varName <- colnames(df.NE)[3]
colNumber <- ncol(df.NE)
colnames(df.NE)[3:colNumber] <- df.NE[1,3:colNumber]
df.NE$var <- varName
colNumber <- ncol(df.NE)
df.NE <- df.NE[-c(1),]
df.NE <- df.NE %>% select(Industry.description,SIC.code,var,`200909`:`201803`)
rownames(df.NE) <- seq(1:112)

# for (i in 4:colNumber ) # get the commas out of the numbers
# {
#     for (j in 1:112)
#     {
#       df.NE[j,i] <- gsub(",","",df.NE[j,i])
#     }
# }

df.NE[,4:colNumber] <- as.data.frame(sapply(df.NE[,4:colNumber], as.numeric)) 

varName <- colnames(df.GrossEarnings)[1]
colnames(df.GrossEarnings) <- df.GrossEarnings[1,]
df.GrossEarnings$var <- varName
colNumber <- ncol(df.GrossEarnings)
df.GrossEarnings <- df.GrossEarnings[-c(1),]
df.GrossEarnings$Industry.description <- df.NE$Industry.description
df.GrossEarnings$SIC.code <- df.NE$SIC.code
df.GrossEarnings<- df.GrossEarnings %>% select(Industry.description,SIC.code,var,`200909`:`201803`)
rownames(df.GrossEarnings) <- seq(1:112)

# for (i in 4:colNumber ) # get the commas out of the numbers
# {
#   for (j in 1:112)
#   {
#     df.GrossEarnings[j,i] <- gsub(",","",df.GrossEarnings[j,i])
#   }
# }
df.GrossEarnings[,4:colNumber] <- as.data.frame(sapply(df.GrossEarnings[,4:colNumber], as.numeric)) 


# df.Data <- dplyr::bind_rows(df.NE, df.GrossEarnings)
# df.Data <- df.Data %>% gather(quarter, value, -Industry.description, -SIC.code, -var)
# df.Data <- df.Data %>% select(Industry.description , SIC.code, var, quarter, value)

############################################
############ Import QLFS data STAT SA | QLFS = survey from households data
############################################
fileName.list <- c("QLFS_Trends_2008-2018_Q2.xlsx")
data <- paste(dir.DATA,fileName.list[1], sep = sep)

# df.QLFS <- readWorksheetFromFile(data, 
#                                  sheet=10, 
#                                  startRow = 2,
#                                  startCol = 1)

df.QLFS <- data.frame(read_excel(data, 
                                 sheet = 10, 
                                 range = cell_limits(ul = c(2,1), lr = c(44, 43))))

df.QLFS <- df.QLFS[-c(1,2),]

df.QLFS <- df.QLFS[1:12,]

# for (i in 2:dim(df.QLFS)[2] ) # get the commas out of the numbers
# {
#   for (j in 1:dim(df.QLFS)[1])
#   {
#     df.QLFS[j,i] <- gsub(",","",df.QLFS[j,i])
#   }
# }

df.QLFS[,2:dim(df.QLFS)[2]] <- as.data.frame(sapply(df.QLFS[,2:dim(df.QLFS)[2]], as.numeric)) 
df.QLFS[,2:dim(df.QLFS)[2]] <- df.QLFS[,2:dim(df.QLFS)[2]] *1000


############################################
############ Compare employment data STAT SA and OECD
############################################

## Aggregate STAT SA data to OECD aggregation, 2008-2014

df.Employment.STATSA.temp <- df.NE[, colnames(df.NE) %in% c("SIC.code","var","201006","201106","201206","201306","201406")]

SIClevels.toRetain <- c("2","3","4","5","6","7","8","9")

df.Employment.STATSA.temp <- df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% SIClevels.toRetain,]

df.Employment.OECD.STATSA <- df.Employment.STATSA.temp[1:7,]
colnames(df.Employment.OECD.STATSA)[1] <- "ISIC"
df.Employment.OECD.STATSA$ISIC <- industries.ISIC

years.quart <-  c("201006","201106","201206","201306","201406")
years <-  c("2010","2011","2012","2013","2014")

colnames(df.Employment.OECD.STATSA)[which(names(df.Employment.OECD.STATSA) %in% years.quart)] <- years
colnames(df.Employment.STATSA.temp)[which(names(df.Employment.STATSA.temp) %in% years.quart)] <- years

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="A-B",colnames(df.Employment.OECD.STATSA) %in% years] <- rep(NA,5)

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="C-E",colnames(df.Employment.OECD.STATSA) %in% years] <- 
          colSums(df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% c("2","3","4"),colnames(df.Employment.STATSA.temp) %in% years])

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="D",colnames(df.Employment.OECD.STATSA) %in% years] <- 
  colSums(df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% c("3"),colnames(df.Employment.STATSA.temp) %in% years])

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="F",colnames(df.Employment.OECD.STATSA) %in% years] <- 
  colSums(df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% c("5"),colnames(df.Employment.STATSA.temp) %in% years])

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="G-I",colnames(df.Employment.OECD.STATSA) %in% years] <- 
  colSums(df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% c("6","7"),colnames(df.Employment.STATSA.temp) %in% years])

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="J-K",colnames(df.Employment.OECD.STATSA) %in% years] <- 
  colSums(df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% c("8"),colnames(df.Employment.STATSA.temp) %in% years])

df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$ISIC=="L-P",colnames(df.Employment.OECD.STATSA) %in% years] <- 
  colSums(df.Employment.STATSA.temp[df.Employment.STATSA.temp$SIC.code %in% c("9"),colnames(df.Employment.STATSA.temp) %in% years])

df.Employment.OECD.STATSA$Source <- "QES"

df.Employment.OECD.STATSA <- df.Employment.OECD.STATSA[,c("ISIC","var","Source",years)]

temp        <- df.Employment.OECD[6:12 ,colnames(df.Employment.OECD) %in% c("ISIC",years)]
temp$Source <- "OECD"
temp$var    <- "Number.of.Employees"

temp <- temp[,c("ISIC","var","Source",years)]

temp[,colnames(temp) %in% years] <- temp[,colnames(temp) %in% years] * 1000

df.Employment.OECD.STATSA <- bind_rows(df.Employment.OECD.STATSA,temp)

#############################"
####################### Transformation of QLFS data to compare QLFS, QES data and OECD

df.QLFS[,2:dim(df.QLFS)[2]] <- as.data.frame(sapply(df.QLFS[,2:dim(df.QLFS)[2]], as.numeric)) 

df.QLFS.temp  <- df.QLFS[,grepl("Jun", colnames(df.QLFS))]
quarters      <- colnames(df.QLFS.temp)
years         <- substr(quarters, nchar(quarters)-3, nchar(quarters))
colnames(df.QLFS.temp) <- years

df.QLFS.temp$Industry <- df.QLFS$X__1
df.QLFS <- df.QLFS.temp
rm(df.QLFS.temp)
df.QLFS <- df.QLFS[1:15,]

df.QLFS$ISIC <- c("A-P","A-B","C","D","E","F","G-H","I","J-K","L-O","P","Other",unique(df.Employment.OECD.STATSA$ISIC)[-c(1,3,4,6)])
df.QLFS$Source <- "QLFS"
df.QLFS$var <- unique(df.Employment.OECD.STATSA$var)
df.QLFS <- df.QLFS[,c("Industry","ISIC","var","Source",years)]

df.QLFS[df.QLFS$ISIC=="C-E",colnames(df.QLFS) %in% years] <- df.QLFS[df.QLFS$ISIC=="C",colnames(df.QLFS) %in% years] + df.QLFS[df.QLFS$ISIC=="D",colnames(df.QLFS) %in% years] + df.QLFS[df.QLFS$ISIC=="E",colnames(df.QLFS) %in% years]

df.QLFS[df.QLFS$ISIC=="G-I",colnames(df.QLFS) %in% years] <- df.QLFS[df.QLFS$ISIC=="G-H",colnames(df.QLFS) %in% years] + df.QLFS[df.QLFS$ISIC=="I",colnames(df.QLFS) %in% years] 

df.QLFS[df.QLFS$ISIC=="L-P",colnames(df.QLFS) %in% years] <- df.QLFS[df.QLFS$ISIC=="L-O",colnames(df.QLFS) %in% years] + df.QLFS[df.QLFS$ISIC=="P",colnames(df.QLFS) %in% years] 

df.QLFS <- df.QLFS[,-c(1)]
#############################




df.QLFS.temp <- df.QLFS[df.QLFS$ISIC %in% unique(df.Employment.OECD.STATSA$ISIC),c("ISIC","var", "Source", "2010","2011","2012","2013","2014")]

df.Employment.OECD.STATSA <- bind_rows(df.Employment.OECD.STATSA, df.QLFS.temp)
                                       
df.plot <- df.Employment.OECD.STATSA %>% gather(Year, Value, -ISIC, -var, -Source)
df.plot$Source <- factor(df.plot$Source, levels=c("QES","QLFS","OECD"), labels=c("Stat SA, QES","Stat SA, QLFS","OECD"))

## plot

colours.col <-c("#f5b041", "#27ae60",  "#2e86c1")
colours.grey <-c("grey40", "grey60",  "grey30")
colours <- colours.grey
for(yearHere in c("2014"))
{
  p <- df.plot[df.plot$Year==yearHere,] %>% ggplot(aes(x=ISIC, y=Value, fill=Source)) +
    geom_bar(stat="identity", position=position_dodge()) +
    ylab(paste0("Number of persons employed, ",yearHere)) +
    scale_fill_manual(values=colours)
  # Use position=position_dodge() ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  
  print(p)
  
  setwd(dir.PLOTS)
  fileName.graph <- paste("ComparisonOECD-STATSA-Empldata",yearHere, sep="_")
  ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=16, height=8, units="cm", dpi=300)
  
}
rm(colours)
############################################
############ Aggregate data into 50 sector format matching the IOTs
############################################

df.NE.50I <- df.NE[1,]
df.NE.50I[1:50,4:ncol(df.NE.50I)] <- "NA"
df.NE.50I[,colnames(df.NE.50I)=="Industry.description"] <- df.IOT2013[2:51,colnames(df.IOT2013)=="Description"]
#df.NE.50I$order <- df.IOT2013[2:51,colnames(df.IOT2013)=="Order"]
rownames(df.NE.50I) <- paste0("I",seq(1,50))

SIC.code.50 <- c("11","12","13","21","23-24","25","301-4","305-6","311-2","313-5","316", "317", "321-2", "323", 
                "324-6", "331-2", "333-4", "335-6", "337", "338", "341", "342", "391", "392_395", "351_353",
                "352", "354-5", "356-9", "36", "371-3", "374-6", "381-387", "41", "42", "5", "61-63", "64", 
                "71-74", "75", "81", "82", "83", "84", "85", "87", "86_88","91_94","92","93","95_96_99")

df.NE.50I[,colnames(df.NE.50I)=="SIC.code"] <- SIC.code.50
df.NE.50I$var <- df.NE.50I[1,colnames(df.NE.50I)=="var"]
colNumber <- ncol(df.NE)
df.NE.50I[7,4:colNumber]  <- colSums(df.NE[6:9,4:colNumber])
df.NE.50I[8,4:colNumber]  <- colSums(df.NE[10,4:colNumber])
df.NE.50I[9,4:colNumber]  <- colSums(df.NE[12:13,4:colNumber])
df.NE.50I[10,4:colNumber] <- colSums(df.NE[14:15,4:colNumber])
df.NE.50I[11,4:colNumber] <- colSums(df.NE[16,4:colNumber])
df.NE.50I[12,4:colNumber] <- colSums(df.NE[17,4:colNumber])
df.NE.50I[13,4:colNumber] <- colSums(df.NE[19:20,4:colNumber])
df.NE.50I[14,4:colNumber] <- colSums(df.NE[21,4:colNumber])
df.NE.50I[15,4:colNumber] <- colSums(df.NE[22:23,4:colNumber])
#df.NE.50I[16,4:colNumber] <- colSums(df.NE[25,4:colNumber])
#df.NE.50I[17,4:colNumber] <- colSums(df.NE[,4:colNumber])
df.NE.50I[18,4:colNumber] <- colSums(df.NE[27,4:colNumber])
df.NE.50I[19,4:colNumber] <- colSums(df.NE[28,4:colNumber])
df.NE.50I[20,4:colNumber] <- colSums(df.NE[29,4:colNumber])
df.NE.50I[21,4:colNumber] <- colSums(df.NE[31,4:colNumber])
df.NE.50I[22,4:colNumber] <- colSums(df.NE[32,4:colNumber])
df.NE.50I[23,4:colNumber] <- colSums(df.NE[63,4:colNumber])
df.NE.50I[24,4:colNumber] <- colSums(df.NE[64:65,4:colNumber])
df.NE.50I[25,4:colNumber] <- colSums(df.NE[c(34,36),4:colNumber]) #,df.NE[36,4:colNumber])
df.NE.50I[26,4:colNumber] <- colSums(df.NE[35,4:colNumber])
df.NE.50I[27,4:colNumber] <- colSums(df.NE[37:38,4:colNumber])
df.NE.50I[28,4:colNumber] <- colSums(df.NE[39:42,4:colNumber])
df.NE.50I[29,4:colNumber] <- colSums(df.NE[43,4:colNumber])
df.NE.50I[30,4:colNumber] <- colSums(df.NE[51:52,4:colNumber])
df.NE.50I[31,4:colNumber] <- colSums(df.NE[53:54,4:colNumber])
df.NE.50I[32,4:colNumber] <- colSums(df.NE[55,4:colNumber])
df.NE.50I[33,4:colNumber] <- colSums(df.NE[67,4:colNumber])
df.NE.50I[34,4:colNumber] <- colSums(df.NE[68,4:colNumber])
df.NE.50I[35,4:colNumber] <- colSums(df.NE[69,4:colNumber])
df.NE.50I[36,4:colNumber] <- colSums(df.NE[76:78,4:colNumber])
df.NE.50I[37,4:colNumber] <- colSums(df.NE[79,4:colNumber])
df.NE.50I[38,4:colNumber] <- colSums(df.NE[81:84,4:colNumber])
df.NE.50I[39,4:colNumber] <- colSums(df.NE[85,4:colNumber])
df.NE.50I[40,4:colNumber] <- colSums(df.NE[87,4:colNumber])
df.NE.50I[41,4:colNumber] <- colSums(df.NE[88,4:colNumber])
df.NE.50I[42,4:colNumber] <- colSums(df.NE[89,4:colNumber])
df.NE.50I[43,4:colNumber] <- colSums(df.NE[90,4:colNumber])
df.NE.50I[44,4:colNumber] <- colSums(df.NE[91,4:colNumber])
df.NE.50I[45,4:colNumber] <- colSums(df.NE[93,4:colNumber])
df.NE.50I[46,4:colNumber] <- colSums(df.NE[c(92,94,95,96,97),4:colNumber])
df.NE.50I[47,4:colNumber] <- colSums(df.NE[c(100,101,102,103,108),4:colNumber])
df.NE.50I[48,4:colNumber] <- colSums(df.NE[c(104,106),4:colNumber])
df.NE.50I[49,4:colNumber] <- colSums(df.NE[107,4:colNumber])
df.NE.50I[50,4:colNumber] <- colSums(df.NE[c(109,110,111),4:colNumber])

df.NE.50I[,4:colNumber] <- as.data.frame(sapply(df.NE.50I[,4:colNumber], as.numeric)) 


#################### Employment data in the mining industry, disaggregation, from document http://www.statssa.gov.za/publications/Report-20-01-02/Report-20-01-022015.pdf
#################### p20, 2015
mining_SIC.code <- c(2, 21, 23, 24, 25, 29)
mining_NE2015 <- c(490146, 97952, 104369, 24524 +16571 +7279 +198951, 1801+2616+10619+15386+1749+483+640, 7206)
NE2015.nongold <- sum(mining_NE2015[mining_SIC.code %in% c("23","24","25")])

#sum(Empl.mining_NumberEmployees_2015[-c(1)]) == Empl.mining_NumberEmployees2015[1] # is TRUE, ok.

# to obtain the number of employees in the mining sector for 2014 & 2015, assume the same shares per subsector. Leave out sector 29.
NE.nongold <- df.NE[df.NE$Industry.description=="Non-gold", colnames(df.NE)%in%c(201412,201512)]
NE.gold <- df.NE[df.NE$Industry.description=="Gold", colnames(df.NE)%in%c(201412,201512)]

df.NE.50I[df.NE.50I$SIC.code=="21",colnames(df.NE.50I) %in% c("201406","201506")] <- 
  NE.nongold/NE2015.nongold * mining_NE2015[mining_SIC.code==21]

df.NE.50I[df.NE.50I$SIC.code=="23-24",colnames(df.NE.50I) %in% c("201406","201506")] <- 
  NE.nongold/NE2015.nongold * mining_NE2015[mining_SIC.code %in% c(24)] + NE.gold

df.NE.50I[df.NE.50I$SIC.code=="25",colnames(df.NE.50I) %in% c("201406","201506")] <- 
  NE.nongold/NE2015.nongold * mining_NE2015[mining_SIC.code==25]

#################### Employment data in Agriculture, from OECD

df.NE.50I[df.NE.50I$SIC.code=="11",colnames(df.NE.50I) %in% years.quart] <- 
  df.IOT2014$Output[1]/sum(df.IOT2014$Output[1:3])  * df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$Source=="OECD" & df.Employment.OECD.STATSA$ISIC=="A-B" ,colnames(df.Employment.OECD.STATSA) %in% years]

df.NE.50I[df.NE.50I$SIC.code=="12",colnames(df.NE.50I) %in% years.quart] <- 
  df.IOT2014$Output[2]/sum(df.IOT2014$Output[1:3])  * df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$Source=="OECD" & df.Employment.OECD.STATSA$ISIC=="A-B" ,colnames(df.Employment.OECD.STATSA) %in% years]

df.NE.50I[df.NE.50I$SIC.code=="13",colnames(df.NE.50I) %in% years.quart] <- 
  df.IOT2014$Output[3]/sum(df.IOT2014$Output[1:3])  * df.Employment.OECD.STATSA[df.Employment.OECD.STATSA$Source=="OECD" & df.Employment.OECD.STATSA$ISIC=="A-B" ,colnames(df.Employment.OECD.STATSA) %in% years]

#################### Employment data 331-4, disaggregation

df.NE.50I[df.NE.50I$SIC.code=="331-2",4:colNumber] <- 
  df.IOT2014$Output[16]/sum(df.IOT2014$Output[16:17]) * colSums(df.NE[25:26,4:colNumber])

df.NE.50I[df.NE.50I$SIC.code=="333-4",4:colNumber] <- 
  colSums(df.NE[25:26,4:colNumber]) - df.NE.50I[df.NE.50I$SIC.code=="331-2",4:colNumber]

#df.NE.50I[df.NE.50I$SIC.code=="331-2",4:colNumber] + df.NE.50I[df.NE.50I$SIC.code=="333-4",4:colNumber]

############################################
############ Match proportions to the proportions of the QLFS data at the 10 sector level
############################################

df.NE.50I.QLFS <- df.NE.50I

yearHere <- "2014"
quarterHere <- paste0(yearHere,"06")

#agri
sectorset <- c("11","12","13") 
scaler    <- df.QLFS[df.QLFS$ISIC=="A-B",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#mining
sectorset <- c("21","23-24","25") 
scaler    <- df.QLFS[df.QLFS$ISIC=="C",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#manufacture
sectorset <- c("301-4","305-6","311-2","313-5","316","317","321-2","323","324-6","331-2","333-4","335-6","337","338","341","342","391","392_395","351_353","352","354-5","356-9","36","371-3","374-6","381-387")
scaler    <- df.QLFS[df.QLFS$ISIC=="D",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#utilities
sectorset <- c("41","42")
scaler    <- df.QLFS[df.QLFS$ISIC=="E",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#construction
sectorset <- c("5")
scaler    <- df.QLFS[df.QLFS$ISIC=="F",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#retail, trade, restaurants
sectorset <- c("61-63","64")
scaler    <- df.QLFS[df.QLFS$ISIC=="G-H",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#transport
sectorset <- c("71-74","75")
scaler    <- df.QLFS[df.QLFS$ISIC=="I",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#finance
sectorset <- c( "81","82","83","84","85","87","86_88")
scaler    <- df.QLFS[df.QLFS$ISIC=="J-K",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#Community and social services
sectorset <- c( "91_94","92","93","95_96_99")
scaler    <- df.QLFS[df.QLFS$ISIC=="L-O",yearHere] / sum(df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, quarterHere]) 

df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)] <-
  scaler * df.NE.50I.QLFS[df.NE.50I.QLFS$SIC.code %in% sectorset, -c(1,2,3)]

#Private households
setwd(dir.ANALYSIS)