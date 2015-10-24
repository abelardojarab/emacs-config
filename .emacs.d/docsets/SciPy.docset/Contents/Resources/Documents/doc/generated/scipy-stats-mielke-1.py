from scipy.stats import mielke
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

k, s = 10.4, 3.6
mean, var, skew, kurt = mielke.stats(k, s, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(mielke.ppf(0.01, k, s),
                mielke.ppf(0.99, k, s), 100)
ax.plot(x, mielke.pdf(x, k, s),
       'r-', lw=5, alpha=0.6, label='mielke pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = mielke(k, s)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = mielke.ppf([0.001, 0.5, 0.999], k, s)
np.allclose([0.001, 0.5, 0.999], mielke.cdf(vals, k, s))
# True

# Generate random numbers:

r = mielke.rvs(k, s, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
