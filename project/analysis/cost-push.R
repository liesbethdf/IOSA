
############################################
############ Evaluate how cost increase coal spreads throughout the economy by asuming it is translated in price increase
############ Cost-push model
############################################

d <- 2      # number of descriptive columns at beginning of IOT
exo.input <- c("Coal and lignite")

exo.n <- length(exo.input)

exo.ind <- which(df.IOT2014$Description == exo.input, arr.ind=TRUE)
#exo.col <- which(colnames(df.IOT2014) == df.IOT2014[exo.ind,"Order"], arr.ind=TRUE)

# Write interindustry matrix without exo.input :

df.IOTHere  <- df.IOT2014[-exo.ind,-(exo.ind + d)]
Z           <- as.matrix(df.IOTHere[1:(n-exo.n), 1:(n-exo.n) + d])

A           <- t(t(Z)/df.IOTHere$Output[1:(n-exo.n)])

L           <- diag(x = 1, nrow=n-exo.n, ncol = n-exo.n) - A

LI          <- inv(as.matrix(L))

# output = total + gross value added + net taxes 
# gross value added = Compensation of Employees + Taxes + Gross operating surplus
# coal was taken out

vars.VA    <- c("Net taxes on products","Gross value added")

vector.exo <- df.IOT2014[exo.ind,1:n + d]
vector.exo <- vector.exo[,-exo.ind]

vector.VA  <- colSums(df.IOTHere[df.IOTHere$Description %in% vars.VA, 1:(n-exo.n) + d]) + vector.exo

VA.per.production <- vector.VA/df.IOTHere$Output[1:(n-exo.n)]

prices <-  as.matrix(VA.per.production) %*% LI

############ Exo price increase

p.increase <- 0.10

vector.VA.scen  <- colSums(df.IOTHere[df.IOTHere$Description %in% vars.VA, 1:(n-exo.n) + d]) + vector.exo * (1 + p.increase)

VA.per.production.scen <- vector.VA.scen/df.IOTHere$Output[1:(n-exo.n)]

prices.scen1 <-  as.matrix(VA.per.production.scen) %*% LI
#prices.scen1 <- c(prices.scen1[1:(exo.ind -1)], 1 , prices.scen1[exo.ind:(n-1)] )

prices.scen1.gr <- (prices.scen1 - rep(1,n-1))

df.prices.scen1 <- data.frame(t(prices.scen1.gr))
colnames(df.prices.scen1) <- "price"
sector.listHere <- c(sector.list[1:(exo.ind - 1)], sector.list[(exo.ind + 1):n])
df.prices.scen1$Description <- factor(sector.listHere, levels=sector.listHere)

df.plot <- df.prices.scen1

p <- df.plot %>% ggplot(aes(x=Description, y=price)) +
                  geom_bar(stat="identity") +
                  ylab(paste0("Cost transfer after ",p.increase*100, "% coal price increase")) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Industry, SIC code") +
                  scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                  scale_y_continuous(labels = scales::percent)
                  #coord_flip() +
                  #labs(fill="Decomposition :") +
                 # theme(legend=none)
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("costPush", paste0("I",exo.ind), p.increase*100,sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=24, height=12, units="cm", dpi=300)


















