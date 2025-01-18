# Load in the packages
library(transport)
# Load data
vefsna <- read.csv("vefsna.csv", header = TRUE, sep = ",")
vefsna <- as.vector(vefsna[[1]])
byskealven <- read.csv("byskealven.csv", header = TRUE, sep = ",")
byskealven <- as.vector(byskealven[[1]])
# Calculate W_p between vefsna and byskealven samples, for p=2
w2 <- wasserstein1d(vefsna, byskealven, p = 2)
cat("W2 =", w2, "\n")
