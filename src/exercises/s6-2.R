library(genalg) # load rbga function
library(mco)  # load nsga2 function

set.seed(12345) # set for replicability

# real value FES2 benchmark:
fes2 <- function(x)
{ D <- length(x);f <- rep(0, 3)
  for(i in 1:D)
     {
      f[1] <- f[1]+(x[i]-0.5*cos(10*pi/D)-0.5)^2
      f[2] <- f[2]+abs(x[i]-(sin(i-1))^2*(cos(i-1)^2))^0.5
      f[3] <- f[3]+abs(x[i]-0.25*cos(i-1)*cos(2*i-2)-0.5)^0.5
     }
  return(f) 
}

D <- 8;m <- 3

# WBGA execution:
# evaluation function for WBGA 
# (also used to print and get last population fes2 values:
# WBGA chromosome used: x=(w1,w2,w3,v1,v2,v3,...,vD)
#   where w_i are the weights and v_j the values
eval <- function(x, REPORT=FALSE)
{ D <- length(x)/2
  # normalize weights, such that sum(w)=1
  w <- x[1:m]/sum(x[1:m]);v <- x[(m+1):length(x)];f <- fes2(v)
  if(REPORT)
    { cat("w:",round(w,2),"v:",round(v,2),"f:",round(f,2),"\n")
      return(f)
    }
  else return(sum(w*f))
}
WBGA <- rbga(evalFunc=eval,
             stringMin=rep(0,D*2), stringMax=rep(1,D*2),
             popSize=20, iters=100)
print("WBGA last population:")
# S1 contains last population fes2 values in individuals x objectives
S1 <- t(apply(WBGA$population, 1, eval, REPORT=TRUE))
LS1 <- nrow(S1)

# NSGA-II execution:
NSGA2 <- nsga2(fn=fes2, idim=D, odim=m,
               lower.bounds=rep(0,D), upper.bounds=rep(1,D),
               popsize=20, generations=100)
S2 <- NSGA2$value[NSGA2$pareto.optimal,]
print("NSGA2 last Pareto front:")
print(S2)
LS2 <- nrow(S2)

# Comparison of results:
library(scatterplot3d)
S <- data.frame(rbind(S1, S2))
names(S) <- c("f1", "f2", "f3")
col <- c(rep("gray", LS1), rep("black", LS2))
# nice scatterplot3d
#  WBGA points are in gray
#  NSGA2 points are in black
#  NSGA2 produces a more disperse and interesting
#    Pareto front when compared with WBGA
scatterplot3d(S,pch=16,color=col)


