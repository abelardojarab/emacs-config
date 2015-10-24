from scipy import stats
import matplotlib.pyplot as plt
np.random.seed(1234)  # make this example reproducible

# Generate some data and determine optimal ``lmbda`` in various ways:

x = stats.loggamma.rvs(5, size=30) + 5
y, lmax_mle = stats.boxcox(x)
lmax_pearsonr = stats.boxcox_normmax(x)

lmax_mle
# 7.177...
lmax_pearsonr
# 7.916...
stats.boxcox_normmax(x, method='all')
# array([ 7.91667384,  7.17718692])

fig = plt.figure()
ax = fig.add_subplot(111)
stats.boxcox_normplot(x, -10, 10, plot=ax)
ax.axvline(lmax_mle, color='r')
ax.axvline(lmax_pearsonr, color='g', ls='--')

plt.show()
