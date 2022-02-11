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
# In comparison, the first stimulation has only 5 alleles and the others have 10. In order to change we use the coalescent.plot() function. 
# Depending on the alleles is what is going to determine fixation. In this case, it doesn't take the first generation of alleles to go through fixation.
# One or two offspring 
# Fitness plays a role in fixation because this is a big deterrence of how many offspring comes from the fitness of something or a population.
#Yes
install.packages("rehh",dep=T)
install.packages("assertthat",dep=T)
install.packages("RcppArmadillo",dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
library(coala)
install.packages("phytools")
library("phytools")
install.packages("ape")
library("ape")
install.packages("maps")
library("maps")
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 
     500, ploidy = 2) +
     feat_mutation(10) +
     feat_recombination(10) +
     sumstat_trees() +
     sumstat_nucleotide_div() 
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
Diversity
#The numbers are different, this is because mutations and recombination 
Nloci <- length(stats$trees)
Nloci
t1 <- read.tree(text=stats$trees [[1]][1])
plot (t1)
axisPhylo()
#They each have 2 alleles 
Agel <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot (t2)
axisPhylo()
#The graphs don't match
par (mfrow=c(1,2))
plot (t1)
axisPhylo()
plot (t2)
axisPhylo()
compare.chronograms(t1 , t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) { 
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo (outPhy, read.tree (text=stats$trees [[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 
                      500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(30) +
  sumstat_trees() +
  sumstat_nucleotide_div() 
#More distribution of alleles 
model3 <- coal_model(10, 50) +
  feat_mutation(par_prior("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi)
plot(theta)
plot(mean_pi, theta, xlabn='Diversity', ylab='Mutation Rate', pch=10, cex=1.8, col='red', main='Diversity from Mutation')
abline(mean_pi ~ theta), col='black')
abline(lm(mean_pi ~ theta), col='black')
dev.off()
#Showed a clear positive correlation
#Extra credit
modelk <- coal_model(c(10,15), 7) +
  feat_size_change(.8, population=2, time= "1") +
  feat_mutation(par_prior("theta", sample.int(2,1))) +
  feat_migration(0.3, 1, 2) +
  feat_migration(1, 2, 1) +
  feat_growth(10, time=0) +
  feat_growth(7, time=1) +
  feat_selection(strength_A = 0.5, population = 2, time=1, locus_group = "all") +
  feat_selection(strength_A = 1.8, population=1, time=0, locus_group = "all") +
  sumstat_nucleotide_div()
modelk


check_model(modelk)
Output <- stimulate (modelk, nsim=2)
  




