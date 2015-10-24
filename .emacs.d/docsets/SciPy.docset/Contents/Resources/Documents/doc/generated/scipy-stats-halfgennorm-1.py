from scipy.stats import halfgennorm
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

beta = 0.675
mean, var, skew, kurt = halfgennorm.stats(beta, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(halfgennorm.ppf(0.01, beta),
                halfgennorm.ppf(0.99, beta), 100)
ax.plot(x, halfgennorm.pdf(x, beta),
       'r-', lw=5, alpha=0.6, label='halfgennorm pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = halfgennorm(beta)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = halfgennorm.ppf([0.001, 0.5, 0.999], beta)
np.allclose([0.001, 0.5, 0.999], halfgennorm.cdf(vals, beta))
# True

# Generate random numbers:

r = halfgennorm.rvs(beta, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
