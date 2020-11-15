source("blind.R") # load the blind search methods

binint=function(x,D) 
{ x=rev(intToBits(x)[1:D]) # get D bits
  # remove extra 0s from raw type:
  as.numeric(unlist(strsplit(as.character(x),""))[(1:D)*2])
}
intbin=function(x) sum(2^(which(rev(x==1))-1))
maxsin=function(x,Dim) sin(pi*(intbin(x))/(2^Dim))

D=16 # number of dimensions

# blind search:
PTM=proc.time() # start clock
x=0:(2^D-1) # integer search space
search=t(sapply(x,binint,D=D))
S=fsearch(search,maxsin,"max",D) # full search
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("fsearch s:",S$sol,"f:",S$eval,"time:",sec,"s\n")

# adapted grid search:
N=1000
PTM=proc.time() # start clock
x=seq(0,2^D-1,length.out=N)
search=t(sapply(x,binint,D=D))
S=fsearch(search,maxsin,"max",D) # grid
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("gsearch s:",S$sol,"f:",S$eval,"time:",sec,"s\n")

# adapted monte carlo search:
PTM=proc.time() # start clock
x=sample(0:2^D-1,N)
search=t(sapply(x,binint,D=D))
S=fsearch(search,maxsin,"max",D) # grid
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("mcsearch s:",S$sol,"f:",S$eval,"time:",sec,"s\n")

