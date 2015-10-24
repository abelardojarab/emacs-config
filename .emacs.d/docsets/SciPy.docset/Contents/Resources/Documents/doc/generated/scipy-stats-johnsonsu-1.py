from scipy.stats import johnsonsu
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a, b = 2.55, 2.25
mean, var, skew, kurt = johnsonsu.stats(a, b, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(johnsonsu.ppf(0.01, a, b),
                johnsonsu.ppf(0.99, a, b), 100)
ax.plot(x, johnsonsu.pdf(x, a, b),
       'r-', lw=5, alpha=0.6, label='johnsonsu pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = johnsonsu(a, b)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = johnsonsu.ppf([0.001, 0.5, 0.999], a, b)
np.allclose([0.001, 0.5, 0.999], johnsonsu.cdf(vals, a, b))
# True

# Generate random numbers:

r = johnsonsu.rvs(a, b, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
