source("functions.R") # bag prices functions
library(copulaedas) # EDA

# auxiliary functions: ------------------------------------

# returns TRUE if prices are sorted in descending order
prices_ord=function(x) 
{ d=diff(x) # d lagged differences x(i+1)-x(i)
  if(sum(d>=0)) return (FALSE) else return (TRUE) 
}
ord_prices=function(x)
{ x=sort(x,decreasing=TRUE) # sort x
  # x is sorted but there can be ties:
  k=2          # remove ties by removing $1
  while(!prices_ord(x)) # at each iteration
    { if(x[k]==x[k-1]) x[k]=x[k]-1
      k=k+1
    }
  return(x)
}

# evaluation function: ------------------------------------
cprofit3=function(x) # bag prices with death penalty
{ x=round(x,digits=0) # convert x into integer
  x=ifelse(x<1,1,x)        # assure that x is within 
  x=ifelse(x>1000,1000,x)  # the [1,1000] bounds
  if(!prices_ord(x)) res=Inf # if needed, death penalty!!!
  else
    {
     s=sales(x);c=cost(s);profit=sum(s*x-c)
     # if needed, store best value
     if(profit>BEST) { BEST<<-profit; B<<-x}
     res=-profit # minimization task!
    }
  EV<<-EV+1 # increase evaluations
  if(EV<=MAXFN) F[EV]<<-BEST
  return(res)
}
# example of a very simple and fast repair of a solution:
# sort the solution values!
localRepair2=function(eda, gen, pop, popEval, f, lower, upper)
{
 for(i in 1:nrow(pop))
 { x=pop[i,]
   x=round(x,digits=0) # convert x into integer
   x=ifelse(x<lower[1],lower[1],x) # assure x within 
   x=ifelse(x>upper[1],upper[1],x) # bounds
   if(!prices_ord(x)) x=ord_prices(x) # order x
   pop[i,]=x;popEval[i]=f(x) # replace x in population
 } 
 return(list(pop=pop,popEval=popEval))
}

# experiment: ----------------------------------------------
MAXFN=5000 
Runs=50; D=5; LP=50; maxit=100
lower=rep(1,D);upper=rep(1000,D)
Methods=c("Death","Repair")
setMethod("edaTerminate","EDA",edaTerminateMaxGen)
UMDA=CEDA(copula="indep",margin="norm"); UMDA@name="UMDA"

RES=vector("list",length(Methods)) # all results
VAL=matrix(nrow=Runs,ncol=length(Methods)) # best values
for(m in 1:length(Methods)) # initialize RES object
   RES[[m]]=matrix(nrow=MAXFN,ncol=Runs) 
for(R in 1:Runs) # cycle all runs
  { 
    B=NA;EV=0; F=rep(NA,MAXFN); BEST= -Inf # reset vars. 
    setMethod("edaOptimize","EDA",edaOptimizeDisabled)
    setMethod("edaTerminate","EDA",edaTerminateMaxGen)
    suppressWarnings(edaRun(UMDA,cprofit3,lower,upper))
    RES[[1]][,R]=F # store all best values
    VAL[R,1]=F[MAXFN] # store best value at MAXFN

    B=NA;EV=0; F=rep(NA,MAXFN); BEST= -Inf # reset vars. 
    # set local repair search method:
    setMethod("edaOptimize","EDA",localRepair2)
    # set additional termination criterion:
    setMethod("edaTerminate","EDA",
              edaTerminateCombined(edaTerminateMaxGen,edaTerminateEvalStdDev))
    # this edaRun might produces warnings or errors:
    suppressWarnings(try(edaRun(UMDA,cprofit3,lower,upper),silent=TRUE))
    if(EV<MAXFN) # if stopped due to EvalStdDev
       F[(EV+1):MAXFN]=rep(F[EV],MAXFN-EV) # replace NAs
    RES[[2]][,R]=F # store all best values
    VAL[R,2]=F[MAXFN] # store best value at MAXFN
  }

# compute average F result per method:
MIN=Inf
AV=matrix(nrow=MAXFN,ncol=length(Methods))
for(m in 1:length(Methods))
  for(i in 1:MAXFN)
   {
    AV[i,m]=mean(RES[[m]][i,])
    # update MIN for plot (different than -Inf):
    if(AV[i,m]!=-Inf && AV[i,m]<MIN) MIN=AV[i,m]
   }
# show results:
cat(Methods,"\n")
cat(round(apply(VAL,2,mean),digits=0)," (average best)\n")
# Mann-Whitney non-parametric test:
p=wilcox.test(VAL[,1],VAL[,2],paired=TRUE)$p.value
cat("p-value:",round(p,digits=2),"(<0.05)\n")

# create PDF file:
pdf("comp-bagprices-constr2.pdf",width=5,height=5,
    paper="special")
par(mar=c(4.0,4.0,1.8,0.6)) # reduce default plot margin
# use a grid to improve clarity:
g1=seq(1,MAXFN,length.out=500) # grid for lines
MAX=max(AV)
plot(g1,AV[g1,2],ylim=c(MIN,MAX),type="l",lwd=2,
     main="bag prices with constraint 2",
     ylab="average best",xlab="number of evaluations")
lines(g1,AV[g1,1],lwd=2,lty=2)
legend("bottomright",legend=rev(Methods),lwd=2,lty=1:4)
dev.off() # close the PDF device
