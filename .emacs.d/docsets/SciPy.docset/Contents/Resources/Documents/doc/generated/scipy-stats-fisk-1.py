from scipy.stats import fisk
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c = 3.09
mean, var, skew, kurt = fisk.stats(c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(fisk.ppf(0.01, c),
                fisk.ppf(0.99, c), 100)
ax.plot(x, fisk.pdf(x, c),
       'r-', lw=5, alpha=0.6, label='fisk pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = fisk(c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = fisk.ppf([0.001, 0.5, 0.999], c)
np.allclose([0.001, 0.5, 0.999], fisk.cdf(vals, c))
# True

# Generate random numbers:

r = fisk.rvs(c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
