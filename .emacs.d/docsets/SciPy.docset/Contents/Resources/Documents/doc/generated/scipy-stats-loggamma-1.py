from scipy.stats import loggamma
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c = 0.414
mean, var, skew, kurt = loggamma.stats(c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(loggamma.ppf(0.01, c),
                loggamma.ppf(0.99, c), 100)
ax.plot(x, loggamma.pdf(x, c),
       'r-', lw=5, alpha=0.6, label='loggamma pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = loggamma(c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = loggamma.ppf([0.001, 0.5, 0.999], c)
np.allclose([0.001, 0.5, 0.999], loggamma.cdf(vals, c))
# True

# Generate random numbers:

r = loggamma.rvs(c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
