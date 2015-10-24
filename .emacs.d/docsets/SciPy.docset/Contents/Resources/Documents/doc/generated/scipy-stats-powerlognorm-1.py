from scipy.stats import powerlognorm
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c, s = 2.14, 0.446
mean, var, skew, kurt = powerlognorm.stats(c, s, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(powerlognorm.ppf(0.01, c, s),
                powerlognorm.ppf(0.99, c, s), 100)
ax.plot(x, powerlognorm.pdf(x, c, s),
       'r-', lw=5, alpha=0.6, label='powerlognorm pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = powerlognorm(c, s)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = powerlognorm.ppf([0.001, 0.5, 0.999], c, s)
np.allclose([0.001, 0.5, 0.999], powerlognorm.cdf(vals, c, s))
# True

# Generate random numbers:

r = powerlognorm.rvs(c, s, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
