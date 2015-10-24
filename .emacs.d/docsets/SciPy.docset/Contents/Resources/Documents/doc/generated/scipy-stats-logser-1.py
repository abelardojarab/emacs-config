from scipy.stats import logser
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

p = 0.6
mean, var, skew, kurt = logser.stats(p, moments='mvsk')

# Display the probability mass function (``pmf``):

x = np.arange(logser.ppf(0.01, p),
              logser.ppf(0.99, p))
ax.plot(x, logser.pmf(x, p), 'bo', ms=8, label='logser pmf')
ax.vlines(x, 0, logser.pmf(x, p), colors='b', lw=5, alpha=0.5)

# Alternatively, the distribution object can be called (as a function)
# to fix the shape and location. This returns a "frozen" RV object holding
# the given parameters fixed.

# Freeze the distribution and display the frozen ``pmf``:

rv = logser(p)
ax.vlines(x, 0, rv.pmf(x), colors='k', linestyles='-', lw=1,
        label='frozen pmf')
ax.legend(loc='best', frameon=False)
plt.show()

# Check accuracy of ``cdf`` and ``ppf``:

prob = logser.cdf(x, p)
np.allclose(x, logser.ppf(prob, p))
# True

# Generate random numbers:

r = logser.rvs(p, size=1000)
