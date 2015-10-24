from scipy import special
from scipy import stats
import matplotlib.pyplot as plt

# Plot the CDF of the non-central F distribution, for nc=0.  Compare with the
# F-distribution from scipy.stats:

x = np.linspace(-1, 8, num=500)
dfn = 3
dfd = 2
ncf_stats = stats.f.cdf(x, dfn, dfd)
ncf_special = special.ncfdtr(dfn, dfd, 0, x)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x, ncf_stats, 'b-', lw=3)
ax.plot(x, ncf_special, 'r-')
plt.show()
