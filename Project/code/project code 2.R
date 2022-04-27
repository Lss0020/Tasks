setwd('C:\\Users\\sians\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\data')
headmorphdata <- read.csv('Head morph.csv')
?headmorphdata
head(headmorphdata)
data(headmorphdata)
cor.test(headmorphdata$TEETH.NUMBER, headmorphdata$HEAD.WIDTH)
ggscatter(headmorphdata, x = "TEETH.NUMBER", y = "HEAD.WIDTH",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "teeth number", ylab= "head width(mm)")


Spec <- unique(headmorphdata[,1])

par(mar=c(4,4,1,1))
plot(headmorphdata[which(headmorphdata[,1]==Spec[1]),2], headmorphdata[which(headmorphdata[,1]==Spec[1]),3], col='orange', xlab="Teeth number", ylab="Head width(mm)")
points(headmorphdata[which(headmorphdata[,1]==Spec[2]),2], headmorphdata[which(headmorphdata[,1]==Spec[2]),3], col='blue', pch=16, ylab="Head width", xlab="")
legend("topright", legend=c("T. Mel No eat crayfish", "T. Mel eat crayfish"), text.col=c("orange", "blue"), bty="n")
linek <- lm(y~x)
abline(linek, col="red")

abline(Model1, col="red")
title("Correlation Between T. Mel According to Teeth Number vs. Head Width(mm)")
abline(lm())

install.packages("ggpubr")
library(phytools)
trees <- read.nexus("headmorphdata")
data(headmorphdata)

