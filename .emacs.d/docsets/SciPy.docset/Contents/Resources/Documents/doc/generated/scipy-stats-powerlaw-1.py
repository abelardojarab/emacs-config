from scipy.stats import powerlaw
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a = 1.66
mean, var, skew, kurt = powerlaw.stats(a, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(powerlaw.ppf(0.01, a),
                powerlaw.ppf(0.99, a), 100)
ax.plot(x, powerlaw.pdf(x, a),
       'r-', lw=5, alpha=0.6, label='powerlaw pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = powerlaw(a)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = powerlaw.ppf([0.001, 0.5, 0.999], a)
np.allclose([0.001, 0.5, 0.999], powerlaw.cdf(vals, a))
# True

# Generate random numbers:

r = powerlaw.rvs(a, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
