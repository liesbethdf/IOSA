
############################################
############ Check Leontief inverse ; ok, same as provided by SA Stat
############################################

fd    <- df.IOT2014$Exports[1:n] + df.IOT2014$Household[1:n] + df.IOT2014$General.Government[1:n] + df.IOT2014$Capital.formation[1:n] + df.IOT2014$Changes.in.inventories[1:n]
fd.d  <- df.IOT2014.dom$Exports[1:n] + df.IOT2014.dom$Household[1:n] + df.IOT2014.dom$General.Government[1:n] + df.IOT2014.dom$Capital.formation[1:n] + df.IOT2014.dom$Changes.in.inventories[1:n]

s.d   <- fd.d/fd        # share domestic    #fd : final demand #fd.d # final demand met with domestic goods

# Z     <- as.matrix(df.IOT2014[1:n,(1:n)+2])  
# Z.d   <- as.matrix(df.IOT2014.dom[1:n,(1:n)+3])
# Z.m   <- as.matrix(df.IOT2014.imp[1:n,(1:n)+3])
# 
# A     <- t(t(Z)/df.IOT2014$Output[1:n])
# A.d   <- t(t(Z.d)/df.IOT2014$Output[1:n])
# A.m   <- t(t(Z.m)/df.IOT2014$Output[1:n])
# 
# L     <- diag(x = 1, nrow=n, ncol = n) - A
# L.d   <- diag(x = 1, nrow=n, ncol = n) - A.d
# L.m   <- diag(x = 1, nrow=n, ncol = n) - A.m
# 
# LI    <- inv(as.matrix(L))
# LI.d  <- inv(as.matrix(L.d))
# LI.m  <- inv(as.matrix(L.m))

# s.d.exp   <- rep(0,n)
# s.d.cons  <- rep(0,n)
# s.d.gov   <- rep(0,n)
# s.d.gfcf  <- rep(0,n)
# s.d.chi   <- rep(0,n)
# s.m.exp   <- rep(0,n)
# s.m.cons  <- rep(0,n)
# s.m.gov   <- rep(0,n)
# s.m.gfcf  <- rep(0,n)
# s.m.chi   <- rep(0,n)
# 
# nonzero <- which(!df.IOT2014$Exports[1:50]==0)
# s.d.exp[nonzero] <- df.IOT2014.dom$Exports[nonzero]/df.IOT2014$Exports[nonzero]
# 
# nonzero <- which(!df.IOT2014$Household[1:50]==0)
# s.d.cons[nonzero] <- df.IOT2014.dom$Household[nonzero]/df.IOT2014$Household[nonzero]
# 
# nonzero <- which(!df.IOT2014$General.Government[1:50]==0)
# s.d.gov[nonzero] <- df.IOT2014.dom$General.Government[nonzero]/df.IOT2014$General.Government[nonzero]
# 
# nonzero <- which(!df.IOT2014$Capital.formation[1:50]==0)
# s.d.gfcf[nonzero] <- df.IOT2014.dom$Capital.formation[nonzero]/df.IOT2014$Capital.formation[nonzero]
# 
# nonzero <- which(!df.IOT2014$Changes.in.inventories[1:50]==0)
# s.d.chi[nonzero] <- df.IOT2014.dom$Changes.in.inventories[nonzero]/df.IOT2014$Changes.in.inventories[nonzero]
# 
# nonzero <- which(!df.IOT2014$Exports[1:50]==0)
# s.m.exp[nonzero] <- df.IOT2014.imp$Exports[nonzero]/df.IOT2014$Exports[nonzero]
# 
# nonzero <- which(!df.IOT2014$Household[1:50]==0)
# s.m.cons[nonzero] <- df.IOT2014.imp$Household[nonzero]/df.IOT2014$Household[nonzero]
# 
# nonzero <- which(!df.IOT2014$General.Government[1:50]==0)
# s.m.gov[nonzero] <- df.IOT2014.imp$General.Government[nonzero]/df.IOT2014$General.Government[nonzero]
# 
# nonzero <- which(!df.IOT2014$Capital.formation[1:50]==0)
# s.m.gfcf[nonzero] <- df.IOT2014.imp$Capital.formation[nonzero]/df.IOT2014$Capital.formation[nonzero]
# 
# nonzero <- which(!df.IOT2014$Changes.in.inventories[1:50]==0)
# s.m.chi[nonzero] <- df.IOT2014.imp$Changes.in.inventories[nonzero]/df.IOT2014$Changes.in.inventories[nonzero]
# 


############################################
############ Contenu en emploie / employment content
############################################

demand <- df.IOT2014$Exports[1:n] + 
          df.IOT2014$Household[1:n] + 
          df.IOT2014$General.Government[1:n] + 
          df.IOT2014$Capital.formation[1:n] + 
          df.IOT2014$Changes.in.inventories[1:n]

employment.per.production <- df.NE.50I.QLFS$`201406`/df.IOT2014$Output[1:n]

#employment.content.direct.indirect <- round(diag(employment.per.production) %*% LI.d,3)
employment.content.direct.indirect <- diag(employment.per.production) %*% LI.d %*% diag(s.d)

df.ec.di <- data.frame(employment.content.direct.indirect)
colnames(df.ec.di) <- SA.IO.codes
rownames(df.ec.di) <- SA.IO.codes

df.e.di <- data.frame(employment.content.direct.indirect %*% diag(demand))
colnames(df.e.di) <- SA.IO.codes

df.output.di <- LI.d %*% diag(s.d) %*% diag(demand)
df.output.di <- add.sectorNames(df.output.di)

employment.content.indirect <- colSums(employment.content.direct.indirect - diag(diag(employment.content.direct.indirect)))

employment.content.direct   <- (diag(employment.content.direct.indirect))

############ Decomposition employment content

VA    <- df.IOT2014[df.IOT2014$Description=="Gross value added",(1:n)+2] + df.IOT2014[df.IOT2014$Description=="Net taxes on products",(1:n)+2]
VA.HT <- df.IOT2014[df.IOT2014$Description=="Gross value added",(1:n)+2] - df.IOT2014[df.IOT2014$Description=="Other taxes less subsidies",(1:n)+2]
COMP  <- df.IOT2014[df.IOT2014$Description=="Compensation of employees",(1:n)+2]
NE    <- df.NE.50I$`201406`

T     <- diag(VA.HT/VA)
L     <- diag(COMP/VA.HT)
N     <- diag(NE/COMP)
MF    <- diag(s.d)
MI    <- LI.d/LI
V     <- diag(VA/df.IOT2014$Output) %*% LI

#TestCompE <- T %*% L %*% N %*% V * MI %*% MF

Tbar     <- matrix(rep(as.numeric(VA.HT/VA),n),nrow=n)
Lbar     <- matrix(rep(as.numeric(COMP/VA.HT),n),nrow=n)
Nbar     <- matrix(rep(as.numeric(NE/COMP),n),nrow=n)
MFbar    <- t(matrix(rep(s.d,n),nrow=n)) 
MIbar    <- MI
Vbar     <- V

TestCompE.bar <- Tbar * Lbar * Nbar * Vbar * MIbar * MFbar

#### Check, is the same matrix obtained through the decomposition ?

## Average over the economy
s.d.m   <-  sum(fd.d)/sum(fd) 
#s.d.m   <-  abs(sum(df.IOT2014$Output[1:n]))/sum(df.IOT2014$Output[1:n] + abs(df.IOT2014$Imports[1:n]))  
T.m     <-  sum(VA.HT)/sum(VA)
L.m     <-  sum(COMP)/sum(VA.HT)
N.m     <-  sum(NE)/sum(COMP)
#MF.m    <-  diag(s.d.m)
MI.m.diag   <-  sum(diag(LI.d))/sum(diag(LI))
MI.m.ofdiag <-  sum(LI.d - diag(diag(LI.d)))/sum(LI - diag(diag(LI)))
MI.m        <-  diag(MI.m.diag, nrow=n) + matrix(MI.m.ofdiag, nrow=n, ncol=n) - diag(MI.m.ofdiag, nrow=n)
V.m.diag    <-  sum(VA)/sum(df.IOT2014$Output[1:n]) * mean(diag(LI))
V.m.ofdiag  <-  sum(VA)/sum(df.IOT2014$Output[1:n]) * sum(LI - diag(diag(LI)))/(dim(LI)[1]^2 - dim(LI)[1])
V.m         <-  diag(V.m.diag, nrow=n) + matrix(V.m.ofdiag, nrow=n, ncol=n) - diag(V.m.ofdiag, nrow=n)

employment.content.m.matrix  <- MI.m*V.m * T.m * L.m * N.m * s.d.m
employment.content.m  <- colSums(employment.content.m.matrix)

employment.content.m.test <- mean(colSums(employment.content.direct.indirect))

## Ventilation

#T.s   <-  Vbar^(1/4) * Tbar
#L.s   <-  Vbar^(1/4) * Lbar
#N.s   <-  Vbar^(1/4) * Nbar
#MI.s  <-  Vbar^(1/4) * MIbar

#TestCompE.s <- T.s * L.s * N.s  * MI.s * MFbar #OK

## LMDI decomposition
ec.d.i <- employment.content.direct.indirect
ec.m.m <- employment.content.m.matrix

w <- (ec.d.i - ec.m.m)/(log(ec.d.i) - log(ec.m.m))
k <- (Vbar/V.m)^(1/4)
  
## Deviations from mean, total economy

T.dist  <-  w * log(k * Tbar / T.m)
L.dist  <-  w * log(k * Lbar / L.m)
N.dist  <-  w * log(k * Nbar / N.m)
MF.dist <-  w * log(MFbar/s.d.m)
MI.dist <-  w * log(k * MI/MI.m)
MI.dist[is.na(MI.dist)] <- 0

## Deviations from mean, direct

T.dist.direct  <-  diag(diag(T.dist))
L.dist.direct  <-  diag(diag(L.dist))
N.dist.direct  <-  diag(diag(N.dist))
MF.dist.direct <-  diag(diag(MF.dist))
MI.dist.direct <-  diag(diag(MI.dist))

## Deviations from mean, indirect

T.dist.indirect  <-  T.dist - diag(diag(T.dist))
L.dist.indirect  <-  L.dist - diag(diag(L.dist))
N.dist.indirect  <-  N.dist - diag(diag(N.dist))
MF.dist.indirect <-  MF.dist - diag(diag(MF.dist))
MI.dist.indirect <-  MI.dist - diag(diag(MI.dist))

#Check 
#round(colSums(Tbar * Lbar * Nbar * Vbar * MIbar * MFbar - employment.content.m.matrix),5) == round(colSums(T.dist) + colSums(L.dist) + colSums(N.dist) + colSums(MF.dist) + colSums(MI.dist),5)

Order     <- df.IOT2014$Order[1:n]
Industry  <- df.NE.50I$Industry.description
SIC       <- df.NE.50I$SIC.code

df.results                    <-  data.frame(Industry)
df.results$SIC                <-  df.NE.50I$SIC.code
df.results$Order              <-  Order
df.results$ec                 <-  colSums(employment.content.direct.indirect)
df.results$ec.direct          <-  employment.content.direct
df.results$ec.indirect        <-  employment.content.indirect
df.results$ec.mean            <-  colSums(employment.content.m.matrix)
df.results$ec.dev             <-  colSums(employment.content.direct.indirect) - colSums(employment.content.m.matrix)
df.results$ec.dev.decomp.T    <-  colSums(T.dist)
df.results$ec.dev.decomp.L    <-  colSums(L.dist)
df.results$ec.dev.decomp.N    <-  colSums(N.dist)
df.results$ec.dev.decomp.MF   <-  colSums(MF.dist)
df.results$ec.dev.decomp.MI   <-  colSums(MI.dist)
df.results$ec.dev.decomp.T.d    <-  colSums(T.dist.direct)
df.results$ec.dev.decomp.L.d    <-  colSums(L.dist.direct)
df.results$ec.dev.decomp.N.d    <-  colSums(N.dist.direct)
df.results$ec.dev.decomp.MF.d   <-  colSums(MF.dist.direct)
df.results$ec.dev.decomp.MI.d   <-  colSums(MI.dist.direct)
df.results$ec.dev.decomp.T.i    <-  colSums(T.dist.indirect)
df.results$ec.dev.decomp.L.i    <-  colSums(L.dist.indirect)
df.results$ec.dev.decomp.N.i    <-  colSums(N.dist.indirect)
df.results$ec.dev.decomp.MF.i   <-  colSums(MF.dist.indirect)
df.results$ec.dev.decomp.MI.i   <-  colSums(MI.dist.indirect)

df.results.ec.matrix            <- data.frame(employment.content.direct.indirect)
colnames(df.results.ec.matrix)  <- Order
rownames(df.results.ec.matrix)  <- Order
df.results.ec.matrix$Industry   <- df.NE.50I$Industry.description
df.results.ec.matrix$SIC        <- df.NE.50I$SIC.code
df.results.ec.matrix            <- df.results.ec.matrix[,c("Industry","SIC",Order)]

results <- list(df.results,df.results.ec.matrix)

############################################
############ Graphs of employment content, total economy
############################################

######################## Decomposition of difference with average, direct and indirect together

df.plot <- results[[1]]
Industry.code <- paste(df.plot$Industry, df.plot$SIC,sep = " ")

df.plot$Industry <- factor(df.plot$Industry,levels=Industry)
df.plot$Industry.code <- Industry.code
df.plot$Industry.code <- factor(df.plot$Industry.code,levels=Industry.code)

df.plot.temp <- df.plot

df.plot <- df.plot %>% gather(Decomposition,Value, ec.dev.decomp.T, ec.dev.decomp.L, ec.dev.decomp.N, ec.dev.decomp.MF, ec.dev.decomp.MI, factor_key = TRUE)

levels(df.plot$Decomposition) <- c("Taxes", "Labour \nshare","Wages", "Local \nfinal demand","Intermediate \nimports")
df.plot$Decomposition <- factor(df.plot$Decomposition, levels=c("Wages","Labour \nshare","Taxes", "Local \nfinal demand","Intermediate \nimports"))

p <- df.plot %>% ggplot(aes(x=Industry.code, y=Value, fill=Decomposition)) +
                 geom_bar(stat="identity") +
                 ylab(paste0("Employment content against average, ",yearHere)) +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                 labs(x = "Industry, SIC code") +
                 scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                 scale_y_continuous(limits = c(-2.2, 3.5)) +
                 coord_flip() +
                 labs(fill="Decomposition :") +
                 theme(legend.position="bottom")
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ECdecomposition-avg-PQ",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=20, height=25, units="cm", dpi=300)

######################## Employment content per sector decomposed in direct, indirect.
## Two graphs : 1. horizontal bargraph, employment content per sector, decomposed into direct/indirect
##              2. horizontal bargraph, decomposition of difference with average, into direct/indirect

# plot 1

df.plot <- df.plot.temp

df.plot <- df.plot %>% select(Industry.code, ec.direct, ec.indirect)

df.plot <- df.plot %>% gather(Decomposition,Value, -Industry.code, factor_key = TRUE)
levels(df.plot$Decomposition) <- c("Direct","Indirect")


p <- df.plot %>% ggplot(aes(x=Industry.code, y=Value, fill=Decomposition)) +
                  geom_bar(stat="identity") +
                  ylab(paste0("Employment content, #jobs/million Rand, ",yearHere)) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Industry, SIC code") +
                  scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                  coord_flip() +
                  labs(fill="Decomposition :") +
                  geom_hline(yintercept = c(1.09), linetype="longdash", colour ="#00BFC4", size=0.8) +
                  geom_hline(yintercept = c(2.4), linetype="longdash", colour = "#999999",size=0.8) +
                  scale_y_continuous(breaks = sort(c(seq(round(min(df.plot$Value),0), round(max(df.plot$Value),0), length.out=3), 1.09,2.4))) +
                    theme(legend.position="bottom")
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ECdecomposition-direct-indirect",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=20, height=25, units="cm", dpi=300)

#plot 2

df.plot <- df.plot.temp
df.plot$ec.dev.decomp.d   <- df.plot$ec.direct - mean(df.plot$ec.direct)
df.plot$ec.dev.decomp.id  <- df.plot$ec.indirect - mean(df.plot$ec.indirect)

df.plot <- df.plot %>% select(Industry.code, ec.dev.decomp.d, ec.dev.decomp.id)

df.plot <- df.plot %>% gather(Decomposition,Value, -Industry.code, factor_key = TRUE)
levels(df.plot$Decomposition) <- c("Direct","Indirect")

p <- df.plot %>% ggplot(aes(x=Industry.code, y=Value, fill=Decomposition)) +
                  geom_bar(stat="identity") +
                  ylab(paste0("Employment content against average, ",yearHere)) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Industry, SIC code") +
                  scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                  coord_flip() +
                  labs(fill="Decomposition :") +
                  theme(legend.position="bottom")
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ECdecomposition-avg-direct-indirect",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=20, height=25, units="cm", dpi=300)


############################################
############ Graphs of employment content, seperated direct and indirect
############################################

######################## Decomposition of difference with average, direct

df.plot <- results[[1]]
Industry.code <- paste(df.plot$Industry, df.plot$SIC,sep = " ")

df.plot$Industry <- factor(df.plot$Industry,levels=Industry)
df.plot$Industry.code <- Industry.code
df.plot$Industry.code <- factor(df.plot$Industry.code,levels=Industry.code)

df.plot.temp <- df.plot ; rm(df.plot.temp)

df.plot <- df.plot %>% gather(Decomposition,Value, ec.dev.decomp.T.d, ec.dev.decomp.L.d, ec.dev.decomp.N.d, ec.dev.decomp.MF.d, ec.dev.decomp.MI.d, factor_key = TRUE)

levels(df.plot$Decomposition) <- c("Taxes", "Labour \nshare","Wages", "Local \nfinal demand","Intermediate \nimports")
df.plot$Decomposition <- factor(df.plot$Decomposition, levels=c("Wages","Labour \nshare","Taxes", "Local \nfinal demand","Intermediate \nimports"))

p <- df.plot %>% ggplot(aes(x=Industry.code, y=Value, fill=Decomposition)) +
                geom_bar(stat="identity") +
                ylab(paste0("Direct employment content against average, ",yearHere)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                labs(x = "Industry, SIC code") +
                scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                scale_y_continuous(limits = c(-2.2, 3.5)) +
                coord_flip() +
                labs(fill="Decomposition :") +
                theme(legend.position="bottom")
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ECdecomposition-avg-PQ-direct",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=20, height=25, units="cm", dpi=300)

######################## Decomposition of difference with average, indirect

df.plot <- results[[1]]
Industry.code <- paste(df.plot$Industry, df.plot$SIC,sep = " ")

df.plot$Industry <- factor(df.plot$Industry,levels=Industry)
df.plot$Industry.code <- Industry.code
df.plot$Industry.code <- factor(df.plot$Industry.code,levels=Industry.code)

df.plot.temp <- df.plot ; rm(df.plot.temp)

df.plot <- df.plot %>% gather(Decomposition,Value, ec.dev.decomp.T.i, ec.dev.decomp.L.i, ec.dev.decomp.N.i, ec.dev.decomp.MF.i, ec.dev.decomp.MI.i, factor_key = TRUE)

levels(df.plot$Decomposition) <- c("Taxes", "Labour \nshare","Wages", "Local \nfinal demand","Intermediate \nimports")
df.plot$Decomposition <- factor(df.plot$Decomposition, levels=c("Wages","Labour \nshare","Taxes", "Local \nfinal demand","Intermediate \nimports"))

p <- df.plot %>% ggplot(aes(x=Industry.code, y=Value, fill=Decomposition)) +
                  geom_bar(stat="identity") +
                  ylab(paste0("Indirect employment content against average, ",yearHere)) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Industry, SIC code") +
                  scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                  scale_y_continuous(limits = c(-2.2, 3.5)) +
                  coord_flip() +
                  labs(fill="Decomposition :") +
                  theme(legend.position="bottom")
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ECdecomposition-avg-PQ-indirect",yearHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=20, height=25, units="cm", dpi=300)
















