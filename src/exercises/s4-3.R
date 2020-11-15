library(tabuSearch) # get tabuSearch

rastrigin=function(x) f=10*length(x)+sum(x^2-10*cos(2*pi*x))
intbin=function(x) # convert binary to integer
{ sum(2^(which(rev(x==1))-1)) } # explained in Chapter 3
breal=function(x) # convert binary to D real values
{ # note: D and bits need to be set outside this function
  s=vector(length=D) 
  for(i in 1:D) # convert x into s:
  { ini=(i-1)*bits+1;end=ini+bits-1
    n=intbin(x[ini:end])
    s[i]=lower+n*drange/2^bits 
  }
  return(s)
}
# note: tabuSearch does not work well with negative evaluations
# to solve this drawback, a MAXIMUM constant is defined
MAXIMUM=10000 
brastrigin=function(x) MAXIMUM-rastrigin(breal(x)) # max. goal

D=8;MAXIT=500
bits=8 # per dimension
size=D*bits
lower=-5.2;upper=5.2;drange=upper-lower
s=sample(0:1,size=size,replace=TRUE)
b=tabuSearch(size=size,iters=MAXIT,objFunc=brastrigin,config=s,neigh=bits,listSize=bits,nRestarts=1)
ib=which.max(b$eUtilityKeep) # best index
cat("b:",b$configKeep[ib,],"f:",MAXIMUM-b$eUtilityKeep[ib],"\n")
