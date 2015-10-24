from scipy.stats import gausshyper
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a, b, c, z = 13.8, 3.12, 2.51, 5.18
mean, var, skew, kurt = gausshyper.stats(a, b, c, z, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(gausshyper.ppf(0.01, a, b, c, z),
                gausshyper.ppf(0.99, a, b, c, z), 100)
ax.plot(x, gausshyper.pdf(x, a, b, c, z),
       'r-', lw=5, alpha=0.6, label='gausshyper pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = gausshyper(a, b, c, z)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = gausshyper.ppf([0.001, 0.5, 0.999], a, b, c, z)
np.allclose([0.001, 0.5, 0.999], gausshyper.cdf(vals, a, b, c, z))
# True

# Generate random numbers:

r = gausshyper.rvs(a, b, c, z, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
