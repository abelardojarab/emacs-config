from scipy import stats
import matplotlib.pyplot as plt
nsample = 100
np.random.seed(7654321)

# A t distribution with small degrees of freedom:

ax1 = plt.subplot(221)
x = stats.t.rvs(3, size=nsample)
res = stats.probplot(x, plot=plt)

# A t distribution with larger degrees of freedom:

ax2 = plt.subplot(222)
x = stats.t.rvs(25, size=nsample)
res = stats.probplot(x, plot=plt)

# A mixture of two normal distributions with broadcasting:

ax3 = plt.subplot(223)
x = stats.norm.rvs(loc=[0,5], scale=[1,1.5],
                   size=(nsample/2.,2)).ravel()
res = stats.probplot(x, plot=plt)

# A standard normal distribution:

ax4 = plt.subplot(224)
x = stats.norm.rvs(loc=0, scale=1, size=nsample)
res = stats.probplot(x, plot=plt)

# Produce a new figure with a loggamma distribution, using the ``dist`` and
# ``sparams`` keywords:

fig = plt.figure()
ax = fig.add_subplot(111)
x = stats.loggamma.rvs(c=2.5, size=500)
stats.probplot(x, dist=stats.loggamma, sparams=(2.5,), plot=ax)
ax.set_title("Probplot for loggamma dist with shape parameter 2.5")

# Show the results with Matplotlib:

plt.show()
