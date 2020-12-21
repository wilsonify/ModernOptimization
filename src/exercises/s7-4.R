# this solution assumes that file "wine-quality.R" has already been executed

# reload wine quality dataset since a new quality is defined:
file <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
d <- read.table(file = file, sep = ";", header = TRUE)

# convert the output variable into 3 classes of wine:
# "bad" <- 3,4,5  
# "average" <- 6
# "good" <- 7, 8 or 9
d$quality <- cut(d$quality, c(0, 5.5, 6.5, 10), c("bad", "average", "good"))

n <- nrow(d) # total number of samples
ns <- round(n * 0.10) # select only 10% of the samples for a fast demonstration
set.seed(12345) # for replicability
ALL <- sample(1:n, ns) # contains 10% of the index samples
# show a summary of the wine quality dataset (10%):
print(summary(d[ALL,]))
cat("output class distribuition (10% samples):\n")
print(table(d[ALL,]$quality)) # show distribution of classes

# holdout split:
# select training data (for fitting the model), 70%; and 
# test data (for estimating generalization capabilities), 30%.
H <- holdout(d[ALL,]$quality, ratio = 0.7)
cat("nr. training samples:", length(H$tr), "\n")
cat("nr. test samples:", length(H$ts), "\n")

# new evaluation function:
# x is in the form c(Gamma,C)
eval <- function(x)
{ n <- length(x)
  gamma <- 2^x[1]
  C <- 2^x[2]
  inputs <- 1:maxinputs # use all inputs
  attributes <- c(inputs, output)
  # divert console:
  # sink is used to avoid kernlab ksvm messages in a few cases
  sink(file = textConnection("rval", "w", local = TRUE))
  M <- mining(quality ~ ., d[H$tr, attributes], method = c("kfold", 3), model = "svm", search = gamma, mpar = c(C, NA))
  sink(NULL) # restores console
  # AUC for the internal 3-fold cross-validation:
  auc <- as.numeric(mmetric(M, metric = "AUCCLASS"))
  # auc now contains 3 values, the AUC for each class
  auc1 <- 1 - auc # transform auc maximization into minimization goal
  return(auc1)
}

# NSGAII multi-objective optimization:
cat("NSGAII optimization:\n")
m <- 3 # four objectives: AUC for each class and number of features
lower <- c(-15, -5)
upper <- c(3, 15)
PTM <- proc.time() # start clock
G <- nsga2(fn = eval, idim = length(lower), odim = m, lower.bounds = lower, upper.bounds = upper, popsize = 12, generations = 10)
sec <- (proc.time() - PTM)[3] # get seconds elapsed
cat("time elapsed:", sec, "\n")

# show the Pareto front:
I <- which(G$pareto.optimal)
for (i in I)
{ x <- G$par[i,]
  n <- length(x)
  gamma <- 2^x[1]
  C <- 2^x[2]
  features <- round(x[3:n])
  inputs <- which(features == 1)
  cat("gamma:", gamma, "C:", C, "; f=(",
      1 - G$value[i, 1:3], ")\n", sep = " ")
}

Pareto <- 1 - G$value[I,] # AUC for each class
Pareto <- data.frame(Pareto)
names(Pareto) <- c("AUC bad", "AUC average", "AUC good")
# sort Pareto according to f1: 
S <- sort.int(Pareto[, 1], index.return = TRUE)
Pareto <- Pareto[S$ix,]

library(scatterplot3d) # get scatterplot3d function
scatterplot3d(Pareto, xlab = "f1", ylab = "f2", zlab = "f3",
              pch = 16, type = "b")

# looking at the Pareto front, the wine expert could
# select the best model and then measure the performance
# of such model on the test set...
