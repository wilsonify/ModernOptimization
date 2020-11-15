# cycle crossover (CX) operator:
# m is a matrix with 2 parent x ordered solutions
cx=function(m)
{
 N=ncol(m)
 c=matrix(rep(NA,N*2),ncol=N)
 stop=FALSE
 k=1
 ALL=1:N
 while(length(ALL)>0)
 {
  i=ALL[1]
  # perform a cycle:
  base=m[1,i];vi=m[2,i]
  I=i
  while(vi!=base)
   {
    i=which(m[1,]==m[2,i])
    vi=m[2,i]
    I=c(I,i)
   } 
  ALL=setdiff(ALL,I)
  if(k%%2==1) c[,I]=m[,I] else c[,I]=m[2:1,I]
  k=k+1
 }
 return(c)
}

# example of CX operator:
m=matrix(ncol=9,nrow=2)
m[1,]=1:9
m[2,]=c(9,8,1,2,3,4,5,6,7)
print(m)
print("---")
print(cx(m))
