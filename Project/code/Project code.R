setwd('C:\\Users\\sians\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\data')
toadcalldata <- read.csv('ss2.csv')

head(toadcalldata)




par(mar=c(4,4,1,1), las=1)
plot(1:nrow(toadcalldata), toadcalldata[,2], type="l", col="blue", lwd=2, ylim=c(0,50), xlab="individual toad", ylab="")
lines(1:nrow(toadcalldata), toadcalldata[,3], col="orange", lwd=2)
lines(1:nrow(toadcalldata), toadcalldata[,4], col="gray", lwd=2)
legend("topright", legend=c("Call Time", "Latitude", "Mass"), text.col=c("orange", "gray", "blue"), bty="n")


avgMass <- tapply(toadcalldata[,2], toadcalldata[,1], mean, na.rm=T)
barplot(avgMass)

isSB <- which(toadcalldata[,1] == "SB")
isSM <- which(toadcalldata[,1] == "SM")

par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(toadcalldata[isSB,2], toadcalldata[isSB,4], xlab="mass (g)", ylab="latitude", pch=16, col='orange', xlim=c(5, 25), ylim=c(30, 41))
abline(lm(toadcalldata[isSB,4]~toadcalldata[isSB,2]), col='orange', lwd=2, lty=2)
points(toadcalldata[isSM,2], toadcalldata[isSM,4], pch=15, col='blue')
abline(lm(toadcalldata[isSM,4]~toadcalldata[isSM,2]), col='blue', lwd=2, lty=2)

plot(toadcalldata[isSB,2], toadcalldata[isSB,3], xlab="mass (g)", ylab="call time (s)", pch=16, col='orange', xlim=c(5, 25), ylim=c(5, 45))
abline(lm(toadcalldata[isSB,3]~toadcalldata[isSB,2]), col='orange', lwd=2, lty=2)
points(toadcalldata[isSM,2], toadcalldata[isSM,3], pch=15, col='blue')
abline(lm(toadcalldata[isSM,3]~toadcalldata[isSM,2]), col='blue', lwd=2, lty=2)

plot(toadcalldata[isSB,4], toadcalldata[isSB,3], xlab="latitude", ylab="call time (s)", pch=16, col='orange', xlim=c(30, 41), ylim=c(5, 45))
abline(lm(toadcalldata[isSB,3]~toadcalldata[isSB,4]), col='orange', lwd=2, lty=2)
points(toadcalldata[isSM,4], toadcalldata[isSM,3], pch=15, col='blue')
abline(lm(toadcalldata[isSM,3]~toadcalldata[isSM,4]), col='blue', lwd=2, lty=2)
legend("topleft", legend=c("SB","SM"), text.col=c("orange","blue"), bty="n")




