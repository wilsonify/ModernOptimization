source("hill.R") # load the hill climbing methods

intbin <- function(x) sum(2^(which(rev(x == 1)) - 1))
maxsin <- function(x) sin(pi * (intbin(x)) / (2^D))
D <- 16 # number of dimensions
s <- rep(0, D) # initial search point

# hill climbing:
maxit <- 20
C <- list(maxit = maxit, REPORT = 0) # maximum of 10 iterations
ichange <- function(par, lower, upper) # integer change
{ hchange(par, lower, upper, rnorm, mean = 0, sd = 1) }

b <- hclimbing(s, maxsin, change = ichange, lower = rep(0, D), upper = rep(1, D),
               control = C, type = "max")
cat("hill b:", b$sol, "f:", b$eval, "\n")

# simulated annealing: 
eval <- function(x) -maxsin(x)

ichange2 <- function(par) # integer change
{ D <- length(par); hchange(par, lower = rep(0, D), upper = rep(1, D), rnorm, mean = 0, sd = 1) }

C <- list(maxit = maxit)
b <- optim(s, eval, method = "SANN", gr = ichange2, control = C)
cat("sann b:", b$par, "f:", abs(b$value), "\n")

# tabu search:
b <- tabuSearch(size = D, iters = maxit, objFunc = maxsin, config = s, neigh = 4, listSize = 8)
ib <- which.max(b$eUtilityKeep) # best index
cat("tabu b:", b$configKeep[ib,], "f:", b$eUtilityKeep[ib], "\n")
