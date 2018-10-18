
fileName <- "IOTS_17102018203858128ZAF_DomImp.csv"
data <- paste(dir.DATA, "oecd",fileName, sep = sep)
df.IOT.oecd = read.csv(data)

na.columns  <- list.NAcolumns(df.IOT.oecd)
qualifiers.list  <- qualifiers(df.IOT.oecd)
df.IOT.oecd <- df.IOT.oecd %>% select(-na.columns,-TIME)
#df.IOT.oecd <- df.IOT.oecd %>% select(-one_of(colnames(qualifiers)))

years.IOT.oecd <- as.character(unique(df.IOT.oecd$Time))
ny <- length(years.IOT.oecd)

df.IOT.temp <- df.IOT.oecd
df.IOT.temp <- df.IOT.temp %>% spread(Time, Value)
df.IOT.temp <- select(df.IOT.temp , -one_of(years.IOT.oecd[-ny]))

#y <- c("IMP_C85","TTL_C29","TTL_INT")
#f <- function(s) {strsplit(s, "_")[[1]]}
#w <- sapply(y,f)
#x <- sapply(y, f, USE.NAMES=FALSE)
#x[1,]

nLevels                 <- length(levels(df.IOT.temp$ROW))
df.IOT.temp$ROW.Var     <- df.IOT.temp$ROW
df.IOT.temp$ROW.Sector  <- df.IOT.temp$ROW
Var.Sect                <- levels(df.IOT.temp$ROW)
temp                    <- Var.Sect
temp2                   <- Var.Sect

for (i in 1:nLevels){if(TRUE %in% (seq(0,9) %in% strsplit(Var.Sect[i],"")[[1]])) {temp[i]<- strsplit(Var.Sect[i],"_")[[1]][1]}}
for (i in 1:112){if(TRUE %in% (seq(0,9) %in% strsplit(Var.Sect[i],"")[[1]])) {temp2[i]<- strsplit(Var.Sect[i],"_")[[1]][2]} else {temp2[i] <- NA}}

levels(df.IOT.temp$ROW.Var)     <- temp
levels(df.IOT.temp$ROW.Sector)  <- temp2

cols  <- dim(df.IOT.temp)[2]
r     <- which(colnames(df.IOT.temp)=="ROW")
df.IOT.temp <- df.IOT.temp[,c(seq(1:r),cols-1,cols,seq(r:(cols-3))+r)]

df.IOT.temp2 <- df.IOT.temp  %>% select(-ROW, -Variable)
df.IOT.temp2 <- df.IOT.temp2 %>% filter(df.IOT.temp2$VAR==c("DOMIMP","TTL"))
df.IOT.temp2 <- df.IOT.temp2  %>% select(-VAR)
df.IOT.temp2 <- df.IOT.temp2 %>% filter(df.IOT.temp2$ROW.Var %in% c("DOM","IMP","TTL"))
df.IOT.temp2 <- df.IOT.temp2 %>% spread(ROW.Var,'2011')
  



df.correspondence <- df.NE.50I.QLFS[1:50,c(1,2)]
df.correspondence$ISIC3 <-c()

df.correspondence$Industry.Code.oecd <- c(rep(sectors.oecd[1],3),  # Agri, 
                                          rep(sectors.oecd[2],3),  # Mining
                                          rep(sectors.oecd[3],2),  # Food
                                          rep(sectors.oecd[4],4),  # Textiles
                                          rep(sectors.oecd[5],4),  # Wood
                                          rep(sectors.oecd[6],2),  # Paper
                                          rep(sectors.oecd[7],2),  # Coke
                                          rep(sectors.oecd[8],1),  # Chem
                                          rep(sectors.oecd[9],2),  # Rubber
                                          rep(sectors.oecd[10],2),  # Other non-metal minerals
                                          rep(sectors.oecd[11],),  # Basic metals
                                          rep(sectors.oecd[12],),  # Fabricated metals
                                          rep(sectors.oecd[13],),  # Machinery and eq nec
                                          rep(sectors.oecd[14],),  # Comp, elec opt equip
                                          rep(sectors.oecd[15],),  # Electr mach
                                          rep(sectors.oecd[16],),  # Motor veh
                                          rep(sectors.oecd[17],),  # Other transp
                                          rep(sectors.oecd[18],),  # Manufact nec, recycling
                                          rep(sectors.oecd[19],2),  # Elec
                                          rep(sectors.oecd[20],1),  # Construction
                                          rep(sectors.oecd[21],1),  # Trade
                                          rep(sectors.oecd[22],1),  # Hotels
                                          rep(sectors.oecd[23],1),  # Transport
                                          rep(sectors.oecd[24],1),  # Telecom
                                          rep(sectors.oecd[25],3),  # Fin
                                          rep(sectors.oecd[26],1),  # Real estate act
                                          rep(sectors.oecd[27],1),  # Renting mach and equip
                                          rep(sectors.oecd[29],),  # R&D
                                          rep(sectors.oecd[28],),  # Computer activities
                                          rep(sectors.oecd[30],),  # Public admin. and defence; compulsory social security
                                          rep(sectors.oecd[31],),  # Education
                                          rep(sectors.oecd[32],),  # Health and social work
                                          rep(sectors.oecd[33],),  # Other community, social and personal services
                                          rep(sectors.oecd[34],),  # Private households with employed persons
                                          )
