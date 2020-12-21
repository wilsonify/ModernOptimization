library(pso)
library(copulaedas)
source("blind.R") # get fsearch
source("montecarlo.R") # get mcsearch

# evaluation function: -------------------------------------
eggholder <- function(x) # length of x is 2
{ x <- ifelse(x<lower[1], lower[1], x) # (only due to EDA):
  x <- ifelse(x>upper[1], upper[1], x) # bound if needed
  f <- (-(x[2]+47)*sin(sqrt(abs(x[2]+x[1]/2+47)))
     -x[1]*sin(sqrt(abs(x[1]-(x[2]+47))))
    )
  # global assignment code: <<- 
  EV<<-EV+1 # increase evaluations
  if(f<BEST) BEST<<-f # minimum value
  if(EV<=MAXFN) F[EV]<<-BEST
  return(f)
}

# auxiliary functions: ------------------------------------
crun2 <- function(method, f, lower, upper, LP, maxit, MAXFN) # run a method
{ if(method=="MC") 
   {
    s <- runif(D, lower[1], upper[1]) # initial search point
    mcsearch(MAXFN,lower=lower,upper=upper,FUN=eggholder)
   }
  else if(method=="PSO") 
     { C <- list(maxit=maxit, s=LP, type="SPSO2011")
       psoptim(rep(NA,length(lower)),fn=f,
               lower=lower,upper=upper,control=C)
     }
  else if(method=="EDA") 
     { setMethod("edaTerminate","EDA",edaTerminateMaxGen)
       DVEDA <- VEDA(vine="DVine", indepTestSigLevel=0.01,
                     copulas = "normal", margin = "norm")
       DVEDA@name <- "DVEDA"
       edaRun(DVEDA,f,lower,upper)
     }
}

successes <- function(x, LIM, type="min") # number of successes
{ if(type=="min") return(sum(x<LIM)) else return(sum(x>LIM)) }

ctest2 <- function(Methods, f, lower, upper, type="min", Runs, # test
               D, MAXFN, maxit, LP, pdf, main, LIM) # all methods:
{ RES <- vector("list", length(Methods)) # all results
  VAL <- matrix(nrow=Runs, ncol=length(Methods)) # best values
  for(m in seq_along(Methods)) # initialize RES object
   RES[[m]] <- matrix(nrow=MAXFN, ncol=Runs)
 
  for(R in 1:Runs) # cycle all runs
    for(m in seq_along(Methods))
      { EV<<-0; F<<-rep(NA,MAXFN) # reset EV and F
        if(type=="min") BEST<<-Inf else BEST<<- -Inf # reset BEST
        suppressWarnings(crun2(Methods[m],f,lower,upper,LP,maxit,MAXFN))
        RES[[m]][,R] <- F # store all best values
        VAL[R,m] <- F[MAXFN] # store best value at MAXFN
      }
  # compute average F result per method:
  AV <- matrix(nrow=MAXFN, ncol=length(Methods))
  for(m in seq_along(Methods))
    for(i in 1:MAXFN)
      AV[i,m] <- mean(RES[[m]][i,])
  # show results:
  cat(main,"\n",Methods,"\n")
  cat(round(apply(VAL,2,mean),digits=0)," (average best)\n")
  cat(round(100*apply(VAL,2,successes,LIM,type)/Runs,
            digits=0)," (%successes)\n")

  # create pdf file:
  pdf(paste0(pdf, ".pdf"), width=5, height=5, paper="special")
  par(mar=c(4.0,4.0,1.8,0.6)) # reduce default plot margin
  MIN <- min(AV);MAX <- max(AV)
  # use a grid to improve clarity:
  g1 <- seq(1, MAXFN, length.out=500) # grid for lines
  plot(g1,AV[g1,1],ylim=c(MIN,MAX),type="l",lwd=2,main=main,
       ylab="average best",xlab="number of evaluations")
  for(i in 2:length(Methods)) lines(g1,AV[g1,i],lwd=2,lty=i)
  if(type=="min") position <- "topright" else position <- "bottomright"
  legend(position,legend=Methods,lwd=2,lty= seq_along(Methods))
  dev.off() # close the PDF device
}

# define EV, BEST and F:
MAXFN <- 1000
EV <- 0;BEST <- Inf;F <- rep(NA, MAXFN)
# define method labels:
Methods <- c("MC", "PSO", "EDA")
# eggholder comparison: -----------------------------------
Runs <- 10; D <- 2; LP <- 20; maxit <- 50
lower <- rep(-512, D);upper <- rep(512, D)
ctest2(Methods,eggholder,lower,upper,"min",Runs,D,MAXFN,maxit,LP,
      "comp-eggholder","eggholder (D=2)",-950)
