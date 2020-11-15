source("blind.R") # load the blind search methods
source("montecarlo.R") # load the monte carlo method 

rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))

# experiment setup parameters:
D=30
Runs=30
N=10^c(2,3,4) # number of samples

# perform all monte carlo searches:
S=matrix(nrow=Runs,ncol=length(N))
for(j in 1:length(N)) # cycle all number of samples
for(i in 1:Runs) # cycle all runs
   S[i,j]=mcsearch(N[j],rep(-5.2,D),rep(5.2,D),
                   rastrigin,"min")$eval
# compare average results:
p21=t.test(S[,2],S[,1])$p.value
p31=t.test(S[,3],S[,2])$p.value
cat("N=",N,"\n")
cat("average f:",apply(S,2,mean),"\n")
cat("p-value (N=",N[2],"vs N=",N[1],")=",
    round(p21,digits=2),"\n")
cat("p-value (N=",N[3],"vs N=",N[2],")=",
    round(p31,digits=2),"\n")
boxplot(S[,1],S[,2],S[,3],names=paste("N=",N,sep=""))
