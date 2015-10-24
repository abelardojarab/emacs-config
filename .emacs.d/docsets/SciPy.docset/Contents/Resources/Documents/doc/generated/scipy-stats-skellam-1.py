from scipy.stats import skellam
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

mu1, mu2 = 15, 8
mean, var, skew, kurt = skellam.stats(mu1, mu2, moments='mvsk')

# Display the probability mass function (``pmf``):

x = np.arange(skellam.ppf(0.01, mu1, mu2),
              skellam.ppf(0.99, mu1, mu2))
ax.plot(x, skellam.pmf(x, mu1, mu2), 'bo', ms=8, label='skellam pmf')
ax.vlines(x, 0, skellam.pmf(x, mu1, mu2), colors='b', lw=5, alpha=0.5)

# Alternatively, the distribution object can be called (as a function)
# to fix the shape and location. This returns a "frozen" RV object holding
# the given parameters fixed.

# Freeze the distribution and display the frozen ``pmf``:

rv = skellam(mu1, mu2)
ax.vlines(x, 0, rv.pmf(x), colors='k', linestyles='-', lw=1,
        label='frozen pmf')
ax.legend(loc='best', frameon=False)
plt.show()

# Check accuracy of ``cdf`` and ``ppf``:

prob = skellam.cdf(x, mu1, mu2)
np.allclose(x, skellam.ppf(prob, mu1, mu2))
# True

# Generate random numbers:

r = skellam.rvs(mu1, mu2, size=1000)
