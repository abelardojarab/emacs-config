from scipy.stats import tukeylambda
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

lam = 3.13
mean, var, skew, kurt = tukeylambda.stats(lam, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(tukeylambda.ppf(0.01, lam),
                tukeylambda.ppf(0.99, lam), 100)
ax.plot(x, tukeylambda.pdf(x, lam),
       'r-', lw=5, alpha=0.6, label='tukeylambda pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = tukeylambda(lam)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = tukeylambda.ppf([0.001, 0.5, 0.999], lam)
np.allclose([0.001, 0.5, 0.999], tukeylambda.cdf(vals, lam))
# True

# Generate random numbers:

r = tukeylambda.rvs(lam, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
