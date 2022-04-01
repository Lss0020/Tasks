setwd('C:\\Users\\sians\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_10')
library(phytools)
#Question 1-3
trees <- vector("list")
births <- c()
fractions <- c()
?pbtree
netdiversification <- c()
speciationrate <- c()
Avgbranchlength <- c()
ABL <- c()
for(i in 1:100) {
  births[i]<- runif(1,0,1)
  fractions[i]<- runif(1,0,1)
  trees[[i]] <- pbtree(b=births[i], d=(fractions[i]*births[i]),n=100) 
  ABL[[i]] <- mean(trees[[i]]$edge.length)
}
plot(trees[[i]])
#Question 4- There is a positive correlation of 0.205184 compared between the net diversification and the log of the total number of tips.
TipLog <- log( sapply(trees, Ntip))
BRate <- births
DRate <- births * fractions
NetDiv <- BRate - DRate
plot(NetDiv, TipLog, xlab="net diversification rate", ylab="tips")
abline(lm(TipLog ~ NetDiv))
cor(NetDiv, TipLog)
#Question 5-6- There is a negative correlation of -0.329518 compared between the speciation rate and the average branch length.
ABL <- unlist(ABL)
plot(BRate, ABL, xlab="speciation rate", ylab="average branch length")
abline(lm(ABL ~ BRate))
cor(BRate, ABL)
#Question 7-
tips <- sapply(trees, Ntip)
which.max(tips)
# tips= 77
LargeTree<- trees[[which.max(tips)]]
plot(LargeTree, type="radial")
rates <- c()
MeanTraits <- c()
VarTraits <- c()
traits <- vector(mode="list", length=1)
for (i in 1:100) {
  rates[i] <- runif(1,0,1)
  traits[[i]] <- fastBM(tree= LargeTree, sig2= rates[i])
  MeanTraits[[i]] <- mean(traits[[i]])
  VarTraits[[i]] <- var(traits[[i]])
}
#Question 8- There is a positive correlation of 0.2376105 compared between mean and the rates.
MeanTraits <- unlist(MeanTraits)
plot(MeanTraits, rates, xlab="mean of traits", ylab="rates")
abline(lm(rates ~ MeanTraits))
cor(MeanTraits, rates)
#Question 9- There is a positive correlation of 0.8425549 compared between variance of traits and rates.
VarTraits <- unlist(VarTraits)
plot(VarTraits, rates, xlab="variance of traits", ylab="rates")
abline(lm(rates ~ VarTraits))
cor(VarTraits, rates)
#Question 10- There is a positive correlation of 0.2034047 compared between the first and second elements. No significance.
cor(traits[[1]], traits[[2]])
plot(traits[[1]], traits[[2]], xlab="first element of traits", ylab="second element of traits")
traitMat <- cbind(traits[[1]], traits[[2]])
traitMat

#Extra Credit 
?phylomorphospace
phylomorphospace(LargeTree, traitMat, xlab="first element of traits", ylab="second element of traits")
