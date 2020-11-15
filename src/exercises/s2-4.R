# 1 - use of for ... if
counteven1=function(x) 
{ r=0
  for(i in 1:length(x))
     { if(x[i]%%2==0) r=r+1 }
  return(r)
}

# 2 - use of sapply
# auxiliary function
ifeven=function(x) # x is a number
{ if(x%%2) return(TRUE) else return(FALSE)}

counteven2=function(x)
{ return(sum(sapply(x,ifeven))) }

# 3 - use of direct condition (easiest way)
counteven3=function(x)
{ return(sum(x%%2==0)) }

x=1:10
cat("counteven1:",counteven1(x),"\n")
cat("counteven2:",counteven2(x),"\n")
cat("counteven3:",counteven3(x),"\n")
