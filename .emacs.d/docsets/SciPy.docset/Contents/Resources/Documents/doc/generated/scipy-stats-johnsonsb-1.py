from scipy.stats import johnsonsb
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a, b = 4.32, 3.18
mean, var, skew, kurt = johnsonsb.stats(a, b, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(johnsonsb.ppf(0.01, a, b),
                johnsonsb.ppf(0.99, a, b), 100)
ax.plot(x, johnsonsb.pdf(x, a, b),
       'r-', lw=5, alpha=0.6, label='johnsonsb pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = johnsonsb(a, b)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = johnsonsb.ppf([0.001, 0.5, 0.999], a, b)
np.allclose([0.001, 0.5, 0.999], johnsonsb.cdf(vals, a, b))
# True

# Generate random numbers:

r = johnsonsb.rvs(a, b, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
