m=matrix(nrow=3,ncol=4)
m[1,]=1:4
m[2,]=sqrt(m[1,])
m[3,]=sqrt(m[2,])
m[,4]=m[,3]^2 # m[,3]*m[,3]
print(round(m,digits=2))
cat("sums of rows:",round(apply(m,1,sum),digits=2),"\n")
cat("sums of columns:",round(apply(m,2,sum),digits=2),"\n")
