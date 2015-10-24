from scipy.stats import planck
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

lambda_ = 0.51
mean, var, skew, kurt = planck.stats(lambda_, moments='mvsk')

# Display the probability mass function (``pmf``):

x = np.arange(planck.ppf(0.01, lambda_),
              planck.ppf(0.99, lambda_))
ax.plot(x, planck.pmf(x, lambda_), 'bo', ms=8, label='planck pmf')
ax.vlines(x, 0, planck.pmf(x, lambda_), colors='b', lw=5, alpha=0.5)

# Alternatively, the distribution object can be called (as a function)
# to fix the shape and location. This returns a "frozen" RV object holding
# the given parameters fixed.

# Freeze the distribution and display the frozen ``pmf``:

rv = planck(lambda_)
ax.vlines(x, 0, rv.pmf(x), colors='k', linestyles='-', lw=1,
        label='frozen pmf')
ax.legend(loc='best', frameon=False)
plt.show()

# Check accuracy of ``cdf`` and ``ppf``:

prob = planck.cdf(x, lambda_)
np.allclose(x, planck.ppf(prob, lambda_))
# True

# Generate random numbers:

r = planck.rvs(lambda_, size=1000)
