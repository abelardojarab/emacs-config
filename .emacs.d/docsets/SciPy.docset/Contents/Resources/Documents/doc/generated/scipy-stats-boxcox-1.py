from scipy import stats
import matplotlib.pyplot as plt

# We generate some random variates from a non-normal distribution and make a
# probability plot for it, to show it is non-normal in the tails:

fig = plt.figure()
ax1 = fig.add_subplot(211)
x = stats.loggamma.rvs(5, size=500) + 5
stats.probplot(x, dist=stats.norm, plot=ax1)
ax1.set_xlabel('')
ax1.set_title('Probplot against normal distribution')

# We now use `boxcox` to transform the data so it's closest to normal:

ax2 = fig.add_subplot(212)
xt, _ = stats.boxcox(x)
stats.probplot(xt, dist=stats.norm, plot=ax2)
ax2.set_title('Probplot after Box-Cox transformation')

plt.show()
