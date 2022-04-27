setwd('C:\\Users\\sians\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_11')
#1- Extra Credit
x <- rnorm(100, mean=5, sd=4)
var(x)
mean(x)
y <- ((x*5)+2)+runif(100,0,0.1)
pdf('plot1.pdf', height=4, width=4)
plot(x,y)
abline(lm(y~x), col='red')
dev.off()
coef(lm(y~x))
#Question- The y-intercept is 2.049901 and the x-intercept is 4.999605, they are restricted to the mean and variance because of their similarity.
z <- c()
x <- rnorm(100, mean=5, sd=4)
for (i in 1:100) { 
  z[i] <- runif(1)
  y <- (x * z[i]) + 2 + (rnorm(100, 0:0.1))
  l <- coef(lm(z[1:100]~y))
}
pdf('plot2.pdf', height=4, width=4)
plot(z[1:100], y)
abline(lm(y~z[1:100]))
dev.off()
coef(lm(y~z))
#Question- The y-intercept is 4.268663 and the z is 0.146802, there is a stable linear correlation with z and slope.
pdf('plot3.pdf', height=4, width=4)
plot(c(z, -0.3754))
dev.off()

#2- Extra Credit
install.packages("ggplot")
library(ggplot2)
iter<-10000
doors <- c("horse", "car", "rubber duck")
monty_hall <- function(iteration) {
  contestant_door <- sample(doors, size=iteration,replace=TRUE)
  i=1:iteration
  stick_win <- ifelse(contestant_door== 'car', 1, 0)
  switch_win <- ifelse(contestant_door== 'car', 0, 1)
  stick_prob <- cumsum(stick_win)/i 
  switch_prob <- cumsum(switch_win)/i
  results <- data.frame(i=i, contest_door=contestant_door, stick_win=stick_win, switch_win=switch_win, stick_prob=stick_prob, switch_prob=switch_prob)
  return(results)
}
monty_hall_results <- monty_hall(iter)
summary <- table(monty_hall_results$contestant_door)
df_summary <- data.frame(label=names(summary), count=matrix(summary))
print(df_summary)
barplot(c(sum(as.numeric(monty_hall_results$stick_win)),sum(as.numeric(monty_hall_results$switch_win))), beside=TRUE, xlab='switch', ylab='stick')
head(monty_hall_results)



#3- Extra Credit 
install.packages('meme')
library(meme)
if (.Platform$OS.type == "windows") {
  windowsFonts(
    Impact = windowsFont("Impact"),
    Courier = windowsFont("Courier")
  )
}
u <- 'https://i.imgflip.com/6dzg1r.jpg'
my_meme <- meme(u, upper= "When Dr.Mitchell says to only ask a specific question for R", "But I don't even know the specifics of R", color="white", size="1.5")
plot(my_meme)
meme
dev.off()

