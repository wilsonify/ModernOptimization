library(genalg) # get rba.bin

intbin <- function(x) sum(2^(which(rev(x==1))-1))
maxsin <- function(x) -sin(pi*(intbin(x))/(2^D))
D <- 16 # number of dimensions

# genetic algorithm:
GA <- rbga.bin(size=D, popSize=20, iters=100, zeroToOneRatio=1,
               evalFunc=maxsin, elitism=1)
b <- which.min(GA$evaluations) # best individual
cat("best:",GA$population[b,],"f:",-GA$evaluations[b],"\n")
