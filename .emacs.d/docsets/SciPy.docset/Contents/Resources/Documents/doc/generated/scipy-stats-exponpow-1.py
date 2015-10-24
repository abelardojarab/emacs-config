from scipy.stats import exponpow
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

b = 2.7
mean, var, skew, kurt = exponpow.stats(b, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(exponpow.ppf(0.01, b),
                exponpow.ppf(0.99, b), 100)
ax.plot(x, exponpow.pdf(x, b),
       'r-', lw=5, alpha=0.6, label='exponpow pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = exponpow(b)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = exponpow.ppf([0.001, 0.5, 0.999], b)
np.allclose([0.001, 0.5, 0.999], exponpow.cdf(vals, b))
# True

# Generate random numbers:

r = exponpow.rvs(b, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
