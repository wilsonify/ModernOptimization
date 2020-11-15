source("hill.R") # load the blind search methods
source("mo-tasks.R") # load MO bag prices task
source("lg-ga.R") # load tournament function

# lexicographic hill climbing, assumes minimization goal:
lhclimbing=function(par,fn,change,lower,upper,control,
                    ...)
{ 
  for(i in 1:control$maxit) 
     { 
      par1=change(par,lower,upper) 
      if(control$REPORT>0 &&(i==1||i%%control$REPORT==0)) 
         cat("i:",i,"s:",par,"f:",eval(par),"s'",par1,"f:",
             eval(par1),"\n")
      pop=rbind(par,par1) # population with 2 solutions
      I=tournament(pop,fn,k=2,n=1,m=2) 
      par=pop[I,]
     }
  if(control$REPORT>=1) cat("best:",par,"f:",eval(par),"\n")
  return(list(sol=par,eval=eval(par)))
}

# lexico. hill climbing for all bag prices, one run:
D=5; C=list(maxit=10000,REPORT=10000) # 10000 iterations
s=sample(1:1000,D,replace=TRUE) # initial search
ichange=function(par,lower,upper) # integer value change
{ hchange(par,lower,upper,rnorm,mean=0,sd=1) }
LEXI=c(0.1,0.1) # explicitly defined lexico. tolerances
eval=function(x) c(-profit(x),produced(x))
b=lhclimbing(s,fn=eval,change=ichange,lower=rep(1,D),
          upper=rep(1000,D),control=C)
cat("final ",b$sol,"f(",profit(b$sol),",",produced(b$sol),")\n")
