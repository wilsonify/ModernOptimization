source("blind.R") # load the blind search methods
source("grid.R") # load the grid search methods
source("functions.R") # load the profit function

D=5 # number of dimensions
# grid search code:
S1=gsearch(rep(11,D),rep(350,D),rep(450,D),profit,"max") 
cat("gsearch s:",round(S$sol),"f:",S$eval,"\n")

# dfsearch code:
domain=vector("list",D) 
for(i in 1:D) domain[[i]]=seq(350,450,by=11)
S=dfsearch(domain=domain,FUN=profit,type="max")
cat("dfsearch s:",round(S$sol),"f:",S$eval,"\n")
