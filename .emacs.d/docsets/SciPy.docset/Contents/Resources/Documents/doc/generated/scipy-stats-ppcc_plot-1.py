# First we generate some random data from a Tukey-Lambda distribution,
# with shape parameter -0.7:

from scipy import stats
import matplotlib.pyplot as plt
np.random.seed(1234567)
x = stats.tukeylambda.rvs(-0.7, loc=2, scale=0.5, size=10000) + 1e4

# Now we explore this data with a PPCC plot as well as the related
# probability plot and Box-Cox normplot.  A red line is drawn where we
# expect the PPCC value to be maximal (at the shape parameter -0.7 used
# above):

fig = plt.figure(figsize=(12, 4))
ax1 = fig.add_subplot(131)
ax2 = fig.add_subplot(132)
ax3 = fig.add_subplot(133)
stats.probplot(x, plot=ax1)
stats.boxcox_normplot(x, -5, 5, plot=ax2)
stats.ppcc_plot(x, -5, 5, plot=ax3)
ax3.vlines(-0.7, 0, 1, colors='r', label='Expected shape value')
plt.show()
