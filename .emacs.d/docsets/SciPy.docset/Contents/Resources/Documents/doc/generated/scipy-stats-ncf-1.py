from scipy.stats import ncf
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

dfn, dfd, nc = 27, 27, 0.416
mean, var, skew, kurt = ncf.stats(dfn, dfd, nc, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(ncf.ppf(0.01, dfn, dfd, nc),
                ncf.ppf(0.99, dfn, dfd, nc), 100)
ax.plot(x, ncf.pdf(x, dfn, dfd, nc),
       'r-', lw=5, alpha=0.6, label='ncf pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = ncf(dfn, dfd, nc)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = ncf.ppf([0.001, 0.5, 0.999], dfn, dfd, nc)
np.allclose([0.001, 0.5, 0.999], ncf.cdf(vals, dfn, dfd, nc))
# True

# Generate random numbers:

r = ncf.rvs(dfn, dfd, nc, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
