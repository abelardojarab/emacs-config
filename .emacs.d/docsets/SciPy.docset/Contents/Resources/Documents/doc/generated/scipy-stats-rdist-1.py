from scipy.stats import rdist
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c = 0.9
mean, var, skew, kurt = rdist.stats(c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(rdist.ppf(0.01, c),
                rdist.ppf(0.99, c), 100)
ax.plot(x, rdist.pdf(x, c),
       'r-', lw=5, alpha=0.6, label='rdist pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = rdist(c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = rdist.ppf([0.001, 0.5, 0.999], c)
np.allclose([0.001, 0.5, 0.999], rdist.cdf(vals, c))
# True

# Generate random numbers:

r = rdist.rvs(c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
