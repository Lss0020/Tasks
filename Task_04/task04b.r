setwd('C://Desktop//Evolution//Tasks//Task_04b')
Results <-read.csv("https://jonsmitchell.com/data/biol112labresults.csv
                   ", stringsAsFactors=F)
write.csv(Data, 'rawresults.csv', quote=F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#When the chi squared value is high, the  bars are at zero except for the "red" column
#When the x^2 value is lower, the bars become more even.
# 
Avg <- mean(Chisqs)
#
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
#Values are different for each background
propSig <- length( which( Chisqs > 11.70 )) / length(Chisqs)
pereSig <- round(100 * propSig)

pereSig
#I was expecting a percentage between 30-50%, but the actual percentage of 90 is way different. 
#There could possibly other things that play into it rather than just natural selection.
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter +1
}
abline( v = 11.70, lty=2, lwd = 2, col='black')
#
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
#Not alot of difference
#
Simulation
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit <- c(0.1,0.1,0.1,0.1,0.1,1)
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
#
#
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
#The mixture is similar to the student-generated data.
#It doesn't look like there is a strong correlation with selection in the student group.
#I would say that there is natural selection in the evolutionary process. 
#Natural selection was involved based on the data. 
#Based on the graph and looking at the increasing chi-squared value, it looks like mutation will also increase.
#We can basically see that the data we analyze from the students is going to be more of an accurate number than what would actually happen in nature. 

#Extra Credit
simPop(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu=0, twoway = TRUE, w = NULL)
simPop
n.sims <- 1000
n.color <- rbinom(n.sims, 1000, 0.25)
hist(n.color)
Simulation8 <- simDraws(1e4, mu=0)


