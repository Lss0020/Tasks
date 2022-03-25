setwd('C:\\Users\\sians\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_09')
library(phytools)
library(ape)
library(maps)
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
tree$tip.label
tree$edge.length
# Question 1- There are 82 tip labels and 162 branches 
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
?data
data
# Question 2- Data is showing the different species of lizards and also showing their svl (how long the lizards are in length).
svl <- setNames(data$svl, rownames(data))
rownames(data)
Ancestors <-fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
fastAnc
#Question 3- {} CI95 has a 95% confidence interval. 
#Question 4- 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
#Question 5- 
fossilData
?fastAnc
fossilNodes <- c()
nodeN <- c()
fossilData

  for(i in 1:6) {
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN[i] <- Node }
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes[i], CI=TRUE, var=TRUE)
#Question 7- 
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_withoutFossils
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace, xlab='with fossils', ylab='without fossils', pch=10, cex=1, col='red')
#Question7- The with fossil data showed an increase is size
#Question 8-10- 
install.packages('geiger')
library('geiger')
??fitContinuous
fitContinuous(tree, svl, model="BM")
#AIC= -6.512019
#frequency of best fit= 1.00
fitContinuous(tree, svl, model="EB")
#AIC= -7.235181
#freq. of best fit= 0.29
fitContinuous(tree, svl, model="rate_trend")
#AIC= -6.981431
#freq. of best fit= 0.04
fitContinuous(tree, svl, model="delta")
#AIC= -6.106546
#freq. of best fit= 0.24

#2) The model with best fit model would be "EB" because it has the smallest AIC.

?fastAnc
#3) We can find different model by using the fitContinuous from the 'geiger' package and according to the help page of fastAnc, it uses BM to find the best fit. 


i


 
                                                       