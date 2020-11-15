# 1
# install.packages("RCurl") # if needed, install the package
library(RCurl)             
# 2
fires=getURL("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")
write(fires,file="forestfires.csv") # write to working directory
# 3, read file:
fires=read.table("forestfires.csv",header=TRUE,sep=",")
# 4
aug=fires$temp[fires$month=="aug"]
cat("mean temperature in Aug.:",mean(aug),"\n")
# 5
feb=fires$temp[fires$month=="feb"]
jul=fires$temp[fires$month=="jul"]
sfeb=sample(feb,10)
sjul=sample(jul,10)
saug=sample(aug,10)
p1=t.test(saug,sfeb)$p.value
p2=t.test(saug,sjul)$p.value
p3=t.test(sjul,sfeb)$p.value
cat("p-values (Aug-Feb,Aug-Jul,Jul-Feb):",
    round(c(p1,p2,p3),digits=2),"\n")
# 6
aug100=fires[fires$month=="aug"&fires$area>100,]
print(aug100)
# 7
write.table(aug100,"aug100.csv",sep=",",row.names=FALSE)
