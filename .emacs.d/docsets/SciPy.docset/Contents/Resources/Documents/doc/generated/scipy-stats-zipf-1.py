from scipy.stats import zipf
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

a = 6.5
mean, var, skew, kurt = zipf.stats(a, moments='mvsk')

# Display the probability mass function (``pmf``):

x = np.arange(zipf.ppf(0.01, a),
              zipf.ppf(0.99, a))
ax.plot(x, zipf.pmf(x, a), 'bo', ms=8, label='zipf pmf')
ax.vlines(x, 0, zipf.pmf(x, a), colors='b', lw=5, alpha=0.5)

# Alternatively, the distribution object can be called (as a function)
# to fix the shape and location. This returns a "frozen" RV object holding
# the given parameters fixed.

# Freeze the distribution and display the frozen ``pmf``:

rv = zipf(a)
ax.vlines(x, 0, rv.pmf(x), colors='k', linestyles='-', lw=1,
        label='frozen pmf')
ax.legend(loc='best', frameon=False)
plt.show()

# Check accuracy of ``cdf`` and ``ppf``:

prob = zipf.cdf(x, a)
np.allclose(x, zipf.ppf(prob, a))
# True

# Generate random numbers:

r = zipf.rvs(a, size=1000)
