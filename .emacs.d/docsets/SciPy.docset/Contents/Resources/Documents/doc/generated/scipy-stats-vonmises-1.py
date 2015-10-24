from scipy.stats import vonmises
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

kappa = 3.99
mean, var, skew, kurt = vonmises.stats(kappa, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(vonmises.ppf(0.01, kappa),
                vonmises.ppf(0.99, kappa), 100)
ax.plot(x, vonmises.pdf(x, kappa),
       'r-', lw=5, alpha=0.6, label='vonmises pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = vonmises(kappa)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = vonmises.ppf([0.001, 0.5, 0.999], kappa)
np.allclose([0.001, 0.5, 0.999], vonmises.cdf(vals, kappa))
# True

# Generate random numbers:

r = vonmises.rvs(kappa, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
