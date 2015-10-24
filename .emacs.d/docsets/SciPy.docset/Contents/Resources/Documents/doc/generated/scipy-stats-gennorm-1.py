from scipy.stats import gennorm
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

beta = 1.3
mean, var, skew, kurt = gennorm.stats(beta, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(gennorm.ppf(0.01, beta),
                gennorm.ppf(0.99, beta), 100)
ax.plot(x, gennorm.pdf(x, beta),
       'r-', lw=5, alpha=0.6, label='gennorm pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = gennorm(beta)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = gennorm.ppf([0.001, 0.5, 0.999], beta)
np.allclose([0.001, 0.5, 0.999], gennorm.cdf(vals, beta))
# True

# Generate random numbers:

r = gennorm.rvs(beta, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
