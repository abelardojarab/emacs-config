from scipy.stats import genexpon
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a, b, c = 9.13, 16.2, 3.28
mean, var, skew, kurt = genexpon.stats(a, b, c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(genexpon.ppf(0.01, a, b, c),
                genexpon.ppf(0.99, a, b, c), 100)
ax.plot(x, genexpon.pdf(x, a, b, c),
       'r-', lw=5, alpha=0.6, label='genexpon pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = genexpon(a, b, c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = genexpon.ppf([0.001, 0.5, 0.999], a, b, c)
np.allclose([0.001, 0.5, 0.999], genexpon.cdf(vals, a, b, c))
# True

# Generate random numbers:

r = genexpon.rvs(a, b, c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
