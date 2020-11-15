DIR="" # change to other directory if needed
pdf(paste(DIR,"maxsin.pdf",sep=""),width=5,height=5) # create PDF
D=8 # number of binary digits, the dimension
x=0:(2^D-1);y=sin(pi*x/2^D)
plot(x,y,type="l",ylab="evaluation function",
     xlab="search space",lwd=2)
pmax=c(x[which.max(y)],max(y)) # set the maximum point
points(pmax[1],pmax[2],pch=19,lwd=2) # plot the maximum
legend("topright","optimum",pch=19,lwd=2) # add a legend
dev.off() # close the graphical device
