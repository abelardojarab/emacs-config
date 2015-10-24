from scipy.stats import exponweib
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a, c = 2.89, 1.95
mean, var, skew, kurt = exponweib.stats(a, c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(exponweib.ppf(0.01, a, c),
                exponweib.ppf(0.99, a, c), 100)
ax.plot(x, exponweib.pdf(x, a, c),
       'r-', lw=5, alpha=0.6, label='exponweib pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = exponweib(a, c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = exponweib.ppf([0.001, 0.5, 0.999], a, c)
np.allclose([0.001, 0.5, 0.999], exponweib.cdf(vals, a, c))
# True

# Generate random numbers:

r = exponweib.rvs(a, c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
