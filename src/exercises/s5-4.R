library(rgp) # load rgp

# auxiliary functions:
eggholder <- function(x) # length of x is 2
  f <- (-(x[2]+47)*sin(sqrt(abs(x[2]+x[1]/2+47)))
     -x[1]*sin(sqrt(abs(x[1]-(x[2]+47))))
    )
fwrapper <- function(x, f)
{ res <- suppressWarnings(f(x[1], x[2]))
  # if NaN is generated (e.g. sqrt(-1)) then
  if(is.nan(res)) res <- Inf # replace by Inf
  return(res)
}

# configuration of the genetic programming:
ST <- inputVariableSet("x1", "x2")
cF1 <- constantFactorySet(function() sample(c(2, 47), 1) )
FS <- functionSet("+", "-", "/", "sin", "sqrt", "abs")
# set the input samples:
samples <- 500
domain <- matrix(ncol=2, nrow=samples)
domain[] <- runif(samples, -512, 512)
eval <- function(f) # evaluation function
  mse(apply(domain,1,eggholder),apply(domain,1,fwrapper,f))

# run the genetic programming:
gp <- geneticProgramming(functionSet=FS, inputVariables=ST,
                         constantSet=cF1, populationSize=100,
                         fitnessFunction=eval,
                         stopCondition=makeTimeStopCondition(20),
                         verbose=TRUE)
# show the results:
b <- gp$population[[which.min(gp$fitnessValues)]]
cat("best solution (f=",eval(b),"):\n")
print(b)
L1 <- apply(domain, 1, eggholder)
L2 <- apply(domain, 1, fwrapper, b)
# sort L1 and L2 (according to L1 indexes)
# for an easier comparison of both curves:
L1 <- sort.int(L1, index.return=TRUE)
L2 <- L2[L1$ix]
L1 <- L1$x
MIN <- min(L1, L2);MAX <- max(L1, L2)
plot(L1,ylim=c(MIN,MAX),type="l",lwd=2,lty=1,
     xlab="points",ylab="function values")
lines(L2,type="l",lwd=2,lty=2)
legend("bottomright",leg=c("eggholder","GP function"),lwd=2,lty=1:2)
# note: the fit is not perfect, but the search space is
#       too large
