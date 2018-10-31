
############################################
########### construct seperate input-output tables for domestic production and importations
############################################

df.IOT.temp5 <- df.IOT.temp3 %>% gather(COL, 2011, C01T05:C72T74)
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

colnames(temp1)[1:50+7] <- paste(paste0(rep("I",50),seq(1:50)), colnames(temp1)[1:50+7], sep="_")

temp1$Total.Industry          <- df.IOT.temp5$Total.Industry
temp1$Exports                 <- df.IOT.temp5$EXPO.T
temp1$Household               <- df.IOT.temp5$HFCE
temp1$General.Government      <- df.IOT.temp5$GGFC
temp1$Capital.formation       <- df.IOT.temp5$GFCF
temp1$Changes.in.inventories  <- df.IOT.temp5$INVNT

temp2 <- temp1[1,]
temp2 <- temp2[-1,]

for(i in 1:length(pos))
{
    for (j in 1:freq[i])
    { 
      temp2 <- bind_rows(temp2, temp1[pos[i],])   
    }
}
rm(temp1)

temp2$ROW.Sector <- colnames(temp2)[1:50+7]
temp2$ROW.Sector <- ordered(temp2$ROW.Sector, levels=colnames(temp2)[1:50+7])

############################################
############ matrices for optisolve
############################################

# Run optisolve per row i 

### Inputs

S <- as.matrix(temp2[1:50, c(seq(1:50), 52,53,54,55,56)+7])
B <- as.matrix(df.IOT2014[1:50, c(seq(1:50), 52,54,55,56,57)+2])
#dim(S)==dim(B)

Q.obj <- diag(rep(1,max(dim(S))))

a.obj <- - 2 * S * B

d.obj <- rowSums(S * S * B * B)

SA.IO.codes <- paste0(rep("I",50),seq(1:50))
ids         <- c(SA.IO.codes, "X","C", "G", "I", "Inv")

A.constr.i   <- t(matrix(c(rep(1,55), as.vector(diag(rep(1,55))),as.vector(diag(rep(1,55)))),nrow=55)) # 1st row for sum uses = output, others for uses=0 or > 0
A.constr.i   <- A.constr.i[-56,]

d.constr.i   <- matrix(rep(0,110),nrow=110)

val.constr.i <- matrix(c(as.vector(df.IOT2014$Output[1:50]),rep(0,54*50), as.vector(as.matrix(df.IOT2014[1:50,c(seq(1:50), 52,54,55,56,57)+2]))),nrow=50) 

log.i        <- c("==",rep(">=",54), rep("<=",55))

### Storage space for results

df.IOT2014.dom            <- data.frame(matrix(seq(0,55),nrow=1))
colnames(df.IOT2014.dom)  <- ids  
df.IOT2014.dom <- df.IOT2014.dom[-1,]

### Solving

for(i in 1:nrow(S))
  {
  log   <- log.i
  d.constr  <- d.constr.i
  A.constr  <- A.constr.i
  val.constr<- val.constr.i
  zeros <- which(B[i,]==0)
  
  ##
  log       <- log[-(zeros+55)]
  d.constr  <- d.constr[-(zeros+55)]
  A.constr  <- A.constr[-(zeros+55),]
  val.constr<- val.constr[,-(zeros+55)]
  
  ##
  
    for(z in 1:length(zeros)){ log[1+zeros[z]] <- "==" }
  
  obj     <- quadfun(Q.obj, a=a.obj[i,], d=d.obj[i], id=ids, name="quad.fun")
  constr  <- lincon(A.constr, d=d.constr, dir=log, val=val.constr[i,], 
                  id=ids, use=rep(TRUE,length(log)), name=c("Output",paste0(rep("zero",54),seq(1:54)), paste0(rep("max",length(log)-55),seq(1:(length(log)-55))) ))
  op      <- cop(obj, max=FALSE, lb=NULL, ub=NULL, lc=constr)
  
  results <- solvecop(op, solver="default", make.definite=FALSE, X=NULL, quiet=FALSE)
  
  df.IOT2014.dom <- bind_rows(df.IOT2014.dom , results$x)   
}
# Three lines below, check whether constraint of not exceeding total inputs respected.
#df.IOT2014.dom.rounded <- round(df.IOT2014.dom,10)
#k <- 49
#df.IOT2014.dom.rounded[k,1:55] <= df.IOT2014[k, c(seq(1:50), 52,54,55,56,57)+2]


## Solved, next give names to columns and rows

ncols          <- dim(df.IOT2014.dom)[2]
df.IOT2014.dom <- df.IOT2014.dom[,-ncols]
colnames(df.IOT2014.dom)     <- c(SA.IO.codes,"Exports","Household","General.Government","Capital.formation","Changes.in.inventories")

df.IOT2014.dom$Order                <- SA.IO.codes
df.IOT2014.dom$SIC.code             <- SIC.code.50
df.IOT2014.dom$Industry.description <- Industry.description.50

ncols          <- dim(df.IOT2014.dom)[2]

df.IOT2014.dom <- df.IOT2014.dom[,c(ncols-2,ncols-1,ncols,rep(1:55))]

df.IOT2014.dom$Output <- rowSums(df.IOT2014.dom[,-c(1,2,3)])

############################################
############ Input-output table for imports
############################################

df.IOT2014.imp <- df.IOT2014.dom
df.IOT2014.imp <- df.IOT2014.imp %>% select(-Output)
df.IOT2014.imp[1:50,4:53] <- df.IOT2014[1:50,3:52] - df.IOT2014.dom[1:50,4:53]
df.IOT2014.imp$Exports                <- df.IOT2014$Exports[1:50] - df.IOT2014.dom$Exports[1:50]
df.IOT2014.imp$Household              <- df.IOT2014$Household[1:50] - df.IOT2014.dom$Household[1:50]
df.IOT2014.imp$General.Government     <- df.IOT2014$General.Government[1:50] - df.IOT2014.dom$General.Government[1:50]
df.IOT2014.imp$Capital.formation      <- df.IOT2014$Capital.formation[1:50] - df.IOT2014.dom$Capital.formation[1:50]
df.IOT2014.imp$Changes.in.inventories <- df.IOT2014$Changes.in.inventories[1:50] - df.IOT2014.dom$Changes.in.inventories[1:50]
df.IOT2014.imp$Imports                 <- rowSums(df.IOT2014.imp[,-c(1,2,3)])

#round(df.IOT2014.imp$Imports[1:50],2)==-round(df.IOT2014$Imports[1:50],2) # Check, is OK

# The output of this file is df.IOT2014.dom and df.IOT2014.imp

