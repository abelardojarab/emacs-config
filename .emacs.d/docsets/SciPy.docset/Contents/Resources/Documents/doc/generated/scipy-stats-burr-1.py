from scipy.stats import burr
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c, d = 10.5, 4.3
mean, var, skew, kurt = burr.stats(c, d, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(burr.ppf(0.01, c, d),
                burr.ppf(0.99, c, d), 100)
ax.plot(x, burr.pdf(x, c, d),
       'r-', lw=5, alpha=0.6, label='burr pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = burr(c, d)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = burr.ppf([0.001, 0.5, 0.999], c, d)
np.allclose([0.001, 0.5, 0.999], burr.cdf(vals, c, d))
# True

# Generate random numbers:

r = burr.rvs(c, d, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
