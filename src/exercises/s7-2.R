# this solution assumes that file "tsp.R" has already been executed
source("oea.R") # load ordered evolutionary algorithm
source("s7-1.R") # get the cycle operator

# random mutation
randomm=function(s)
{ return(switch(sample(1:3,1),exchange(s),insertion(s),displacement(s))) }

# random crossover
randomx=function(m)
{ return(switch(sample(1:3,1),pmx(m),ox(m),cx(m))) }

Methods=c("new SANN","new EA")
# new SANN:
cat("new SANN run:\n")
set.seed(12345) # for replicability
s=sample(1:N,N) # initial solution
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
C=list(maxit=MAXIT,temp=2000,trace=TRUE,REPORT=MAXIT)
PTM=proc.time() # start clock
SANN=optim(s,fn=tour,gr=randomm,method="SANN",control=C)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")
RES[,1]=F
cat("tour distance:",tour(SANN$par),"\n")

# new EA:
cat("new EA run:\n")
set.seed(12345) # for replicability
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
pSize=30;iters=ceiling((MAXIT-pSize)/(pSize-1))
PTM=proc.time() # start clock
OEA=oea(size=N,popSize=pSize,iters=iters,evalFunc=tour,crossfunc=randomx,mutfunc=randomm,REPORT=iters,elitism=1)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")
RES[,2]=F
cat("tour distance:",tour(OEA$population[which.min(OEA$evaluations),]),"\n")

# there is no improvement when compared with "tsp.R" file
