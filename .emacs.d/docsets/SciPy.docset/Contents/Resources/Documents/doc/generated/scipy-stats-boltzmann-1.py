from scipy.stats import boltzmann
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

lambda_, N = 1.4, 19
mean, var, skew, kurt = boltzmann.stats(lambda_, N, moments='mvsk')

# Display the probability mass function (``pmf``):

x = np.arange(boltzmann.ppf(0.01, lambda_, N),
              boltzmann.ppf(0.99, lambda_, N))
ax.plot(x, boltzmann.pmf(x, lambda_, N), 'bo', ms=8, label='boltzmann pmf')
ax.vlines(x, 0, boltzmann.pmf(x, lambda_, N), colors='b', lw=5, alpha=0.5)

# Alternatively, the distribution object can be called (as a function)
# to fix the shape and location. This returns a "frozen" RV object holding
# the given parameters fixed.

# Freeze the distribution and display the frozen ``pmf``:

rv = boltzmann(lambda_, N)
ax.vlines(x, 0, rv.pmf(x), colors='k', linestyles='-', lw=1,
        label='frozen pmf')
ax.legend(loc='best', frameon=False)
plt.show()

# Check accuracy of ``cdf`` and ``ppf``:

prob = boltzmann.cdf(x, lambda_, N)
np.allclose(x, boltzmann.ppf(prob, lambda_, N))
# True

# Generate random numbers:

r = boltzmann.rvs(lambda_, N, size=1000)
