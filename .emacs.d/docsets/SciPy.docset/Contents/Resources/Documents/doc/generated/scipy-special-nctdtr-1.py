from scipy import special
from scipy import stats
import matplotlib.pyplot as plt

# Plot the CDF of the non-central t distribution, for nc=0.  Compare with the
# t-distribution from scipy.stats:

x = np.linspace(-5, 5, num=500)
df = 3
nct_stats = stats.t.cdf(x, df)
nct_special = special.nctdtr(df, 0, x)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x, nct_stats, 'b-', lw=3)
ax.plot(x, nct_special, 'r-')
plt.show()
