from scipy.stats import randint
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

low, high = 7, 31
mean, var, skew, kurt = randint.stats(low, high, moments='mvsk')

# Display the probability mass function (``pmf``):

x = np.arange(randint.ppf(0.01, low, high),
              randint.ppf(0.99, low, high))
ax.plot(x, randint.pmf(x, low, high), 'bo', ms=8, label='randint pmf')
ax.vlines(x, 0, randint.pmf(x, low, high), colors='b', lw=5, alpha=0.5)

# Alternatively, the distribution object can be called (as a function)
# to fix the shape and location. This returns a "frozen" RV object holding
# the given parameters fixed.

# Freeze the distribution and display the frozen ``pmf``:

rv = randint(low, high)
ax.vlines(x, 0, rv.pmf(x), colors='k', linestyles='-', lw=1,
        label='frozen pmf')
ax.legend(loc='best', frameon=False)
plt.show()

# Check accuracy of ``cdf`` and ``ppf``:

prob = randint.cdf(x, low, high)
np.allclose(x, randint.ppf(prob, low, high))
# True

# Generate random numbers:

r = randint.rvs(low, high, size=1000)
