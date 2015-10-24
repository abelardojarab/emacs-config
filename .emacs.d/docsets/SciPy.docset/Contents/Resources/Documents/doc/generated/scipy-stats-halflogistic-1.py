from scipy.stats import halflogistic
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

mean, var, skew, kurt = halflogistic.stats(moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(halflogistic.ppf(0.01),
                halflogistic.ppf(0.99), 100)
ax.plot(x, halflogistic.pdf(x),
       'r-', lw=5, alpha=0.6, label='halflogistic pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = halflogistic()
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = halflogistic.ppf([0.001, 0.5, 0.999])
np.allclose([0.001, 0.5, 0.999], halflogistic.cdf(vals))
# True

# Generate random numbers:

r = halflogistic.rvs(size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
