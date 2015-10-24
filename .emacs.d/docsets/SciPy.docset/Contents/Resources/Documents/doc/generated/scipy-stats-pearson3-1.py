from scipy.stats import pearson3
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

skew = 0.1
mean, var, skew, kurt = pearson3.stats(skew, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(pearson3.ppf(0.01, skew),
                pearson3.ppf(0.99, skew), 100)
ax.plot(x, pearson3.pdf(x, skew),
       'r-', lw=5, alpha=0.6, label='pearson3 pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = pearson3(skew)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = pearson3.ppf([0.001, 0.5, 0.999], skew)
np.allclose([0.001, 0.5, 0.999], pearson3.cdf(vals, skew))
# True

# Generate random numbers:

r = pearson3.rvs(skew, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
