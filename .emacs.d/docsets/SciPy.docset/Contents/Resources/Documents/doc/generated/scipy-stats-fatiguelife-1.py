from scipy.stats import fatiguelife
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c = 29
mean, var, skew, kurt = fatiguelife.stats(c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(fatiguelife.ppf(0.01, c),
                fatiguelife.ppf(0.99, c), 100)
ax.plot(x, fatiguelife.pdf(x, c),
       'r-', lw=5, alpha=0.6, label='fatiguelife pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = fatiguelife(c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = fatiguelife.ppf([0.001, 0.5, 0.999], c)
np.allclose([0.001, 0.5, 0.999], fatiguelife.cdf(vals, c))
# True

# Generate random numbers:

r = fatiguelife.rvs(c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
