from scipy.stats import truncexpon
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

b = 4.69
mean, var, skew, kurt = truncexpon.stats(b, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(truncexpon.ppf(0.01, b),
                truncexpon.ppf(0.99, b), 100)
ax.plot(x, truncexpon.pdf(x, b),
       'r-', lw=5, alpha=0.6, label='truncexpon pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = truncexpon(b)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = truncexpon.ppf([0.001, 0.5, 0.999], b)
np.allclose([0.001, 0.5, 0.999], truncexpon.cdf(vals, b))
# True

# Generate random numbers:

r = truncexpon.rvs(b, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
