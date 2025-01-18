# Load in the packages
import numpy as np
import ot

# Load data
vefsna = np.loadtxt("vefsna.csv", delimiter=",", skiprows=1)
byskealven = np.loadtxt("byskealven.csv", delimiter=",", skiprows=1)
# Calculate W_p^p between vefsna and byskealven samples, for p=2
W2_2 = ot.wasserstein_1d(vefsna, byskealven, p=2)
# Calculate W2 using square root
W2 = np.sqrt(W2_2)
print("W2 =", W2)
