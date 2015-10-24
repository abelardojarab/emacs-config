from scipy.stats import alpha
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a = 3.57
mean, var, skew, kurt = alpha.stats(a, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(alpha.ppf(0.01, a),
                alpha.ppf(0.99, a), 100)
ax.plot(x, alpha.pdf(x, a),
       'r-', lw=5, alpha=0.6, label='alpha pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = alpha(a)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = alpha.ppf([0.001, 0.5, 0.999], a)
np.allclose([0.001, 0.5, 0.999], alpha.cdf(vals, a))
# True

# Generate random numbers:

r = alpha.rvs(a, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
