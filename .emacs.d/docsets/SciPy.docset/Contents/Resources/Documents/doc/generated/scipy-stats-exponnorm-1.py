from scipy.stats import exponnorm
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

K = 1.5
mean, var, skew, kurt = exponnorm.stats(K, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(exponnorm.ppf(0.01, K),
                exponnorm.ppf(0.99, K), 100)
ax.plot(x, exponnorm.pdf(x, K),
       'r-', lw=5, alpha=0.6, label='exponnorm pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = exponnorm(K)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = exponnorm.ppf([0.001, 0.5, 0.999], K)
np.allclose([0.001, 0.5, 0.999], exponnorm.cdf(vals, K))
# True

# Generate random numbers:

r = exponnorm.rvs(K, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
