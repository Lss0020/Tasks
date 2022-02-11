set('C:\\users\\Sian\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_05')
library(learnPopGen)
?coalescent.plot
?learnPopGen
pdf('r05-plot1.pdf', height=6, width=6)
coalescent.plot(n=5, ngen=10, colors=NULL)
dev.off()
pdf('r05-plot2.pdf', height=6, width=6)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()
pdf('r05-plot3.pdf', height=6, width=6)
coalescent.plot(n=10, ngen=10, colors=NULL)
dev.off()
#
#
#
#
#
#install.packages("rehh",dep=T)
#install.packages("assertthat",dep=T)
#install.packages("RcppArmadillo",dep=T)
#install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")

