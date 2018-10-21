
#df.IOT.temp3

df.IOT.temp5 <- df.IOT.temp3 %>% gather(COL, 2011, C01T05:lastcol)
df.IOT.temp5          <- df.IOT.temp5  %>% select(-Row.sector..from..)
df.IOT.temp5          <- df.IOT.temp5 %>% spread(ROW.Sector,'2011')
df.IOT.temp5$C29T33X  <- df.IOT.temp5$C29 + df.IOT.temp5$C30T33X
df.IOT.temp5$C34T35   <- df.IOT.temp5$C34 + df.IOT.temp5$C35
df.IOT.temp5$C72T74   <- df.IOT.temp5$C72 + df.IOT.temp5$C73T74
df.IOT.temp5          <- df.IOT.temp5 %>% gather(ROW.Sector,'2011', C01T05:C72T74 )

df.IOT.temp5 <- df.IOT.temp5 %>% spread(ROW.Var,'2011')
df.IOT.temp5[df.IOT.temp5$COL=="OUTPUT","TTL"] <- df.IOT.temp5[df.IOT.temp5$COL=="OUTPUT","DOM"] + df.IOT.temp5[df.IOT.temp5$COL=="OUTPUT","IMP"]

df.IOT.temp5$TOT <- df.IOT.temp5$DOM + df.IOT.temp5$IMP
df.IOT.temp5[!df.IOT.temp5$TTL==0,] <- df.IOT.temp5[!df.IOT.temp5$TTL==0,] %>% mutate(DOM=DOM/TOT, IMP=IMP/TOT)
df.IOT.temp5 <- df.IOT.temp5  %>% select(-TTL, -IMP, -TOT)

df.IOT.temp5 <- df.IOT.temp5 %>% spread(COL,DOM)

df.IOT.temp5 <- df.IOT.temp5  %>% select(-CONS_ABR, -CONS_NONRES, -EXPO, -IMPO, -IMPO.T, -NPISH)

code.oecd.l <- unique(df.IOT.temp5$ROW.Sector)
#code.oecd <- as.vector(code.oecd)

sector.matching <- c(rep(code.oecd.l[1],3), rep(code.oecd.l[2],3),
                     rep(code.oecd.l[3],2), rep(code.oecd.l[4],4),
                     rep(code.oecd.l[5],1), rep(code.oecd.l[6],2),
                     rep(code.oecd.l[7],2), rep(code.oecd.l[8],1),
                     rep(code.oecd.l[9],2), rep(code.oecd.l[10],2),
                     rep(code.oecd.l[20],2), rep(code.oecd.l[11],2), 
                     rep(code.oecd.l[12],1), 
                     rep(code.oecd.long[14],1), 
                     rep(code.oecd.l[16],1), rep(code.oecd.l[15],2),
                     rep(code.oecd.long[18],1), 
                     rep(code.oecd.l[21],2), rep(code.oecd.l[22],1),
                     rep(code.oecd.l[23],1), rep(code.oecd.l[24],1),
                     rep(code.oecd.l[25],1), rep(code.oecd.l[26],1),
                     rep(code.oecd.l[27],3), rep(code.oecd.l[28],1),
                     rep(code.oecd.l[29],1), rep(code.oecd.l[32],1), 
                     rep(code.oecd.long[31],1), 
                     rep(code.oecd.l[33],1), rep(code.oecd.l[34],1), 
                     rep(code.oecd.l[35],1), rep(code.oecd.l[36],1)
                     )
pos   <- c(1,2,3,4,5,6,7,8,9,10,20,11,12,14,16,15,18,21,22,23,24,25,26,27,28,29,32,31,33,34,35,36)
freq  <- c(3,3,2,4,1,2,2,1,2,2,2,2,1,1,1,2,1,2,rep(1,5),3,rep(1,8))
#sum(freq) test is ok, =50
#length(freq) == length(pos) #OK

temp1 <- df.IOT.temp5[,1:7]

for(i in 1:length(pos))
  {
  temp1$temp <- df.IOT.temp5[,7+pos[i]]
  colnames(temp1)[dim(temp1)[2]] <- colnames(df.IOT.temp5)[7+pos[i]]
  if (freq[i]>1)
    {
    for (j in 1:(freq[i]-1))
      { 
      temp1$temp <- temp1[,dim(temp1)[2]] ; colnames(temp1)[dim(temp1)[2]] <- colnames(temp1)[dim(temp1)[2]-1] 
      }
    }
  }


#temp1$temp <- df.IOT.temp5[,9]


df.domestic.shares.oecd <- data.frame(matrix(rep(0,2750), nrow=50))
dom.shares <- df.domestic.shares.oecd
colnames(dom.shares) <- c(sector.matching,"Exports", "Households", "General.Government", "Capital.formation", "Changes.in.inventories")
names <- colnames(dom.shares)
dom.shares$Row.Sector <- sector.matching
dom.shares <- dom.shares[,c("Row.Sector",names)]

dom.shares[1,] <- df.IOT.temp5
