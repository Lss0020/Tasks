trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
head(population1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
head(population2)
size <- 50
Sample1 <- sample(population1, Size <- 50)
Sample2 <- sample(population2, Size <- 50)
head(Sample1)
head(Sample2)
boxplot(Sample1, Sample2)
# The samples were different in the sizes of the sample. 
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
head(MatGrandma)
nrow(MatGrandma)
MatGrandpa <- makeFounder("grandma_mom")
head(MatGrandpa)
nrow(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(PatGrandma)
nrow(PatGrandma)
nrow(PatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
nrow(Alan)
Brenda <- makeBaby(PatGrandma, PatGrandpa)
head(Brenda)
nrow(Brenda)
Focus <- makeBaby(Brenda, Alan)
head(Focus)
nrow(Focus)
#  
ToMom <- length( grep("mom", Focus)) / length( Focus )
head(ToMom)
ToMom
ToMomMom <- length( grep( "grandma_mom", Focus )) / length( Focus )
ToMomMom
nrow(ToMomMom)
ToMomDad <- length ( grep( "grandpa_mom", Focus )) / length( Focus )
#Focus isn't equally related to grandpa and grandma 
Sibling_01 <- makeBaby(Brenda, Alan)
# Focus won't have the same DNA as Sibling_01, they will only have some similar traits but the DNa will be their own. 
ToSib <- length( intersect( Focus, Sibling_01 )) / length( Focus )
head(ToSib)
#I thought Focus would have 25% share with her siblings atleast
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan))) / length( Focus ))
head(ManySiblings)
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#
HWE <- function(p) 
{ aa <- p^2 
  ab <- 2 * p * (1-p) 
  bb <- (1-p)^2 
  return(c(aa=aa, ab=ab, bb=bb)) } 
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
# This plot means that the population is constantly going up in the rate of which it's moving 
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#
install.packages("learnPopGen")
library(learnPopGen)
library
head(learnPopGen)
library(learnPopGen)
x <-genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep("Popsizes", 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
install.packages("learnPopGen")
library(learnPopGen)
Samples <- rep(Popsizes <- 5:50, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm( tExt~Samples + 0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)
#Line 1 is a bit above Line 2
# +0 means that there is no intercept 
#Population size is getting larger and the points are spreading further from the line 
install.packages("caret")
library(caret)
tExt2 <- BoxCoxTrans(Samples, tExt)
Samples2 <- cbind(Samples, tExt_ = predict(tExt2, Samples, tExt))
head(Samples2)
tExt3 <- lm(Samples, tExt= Samples2)
