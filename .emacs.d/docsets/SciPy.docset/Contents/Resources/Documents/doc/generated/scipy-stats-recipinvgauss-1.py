from scipy.stats import recipinvgauss
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

mu = 0.63
mean, var, skew, kurt = recipinvgauss.stats(mu, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(recipinvgauss.ppf(0.01, mu),
                recipinvgauss.ppf(0.99, mu), 100)
ax.plot(x, recipinvgauss.pdf(x, mu),
       'r-', lw=5, alpha=0.6, label='recipinvgauss pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = recipinvgauss(mu)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = recipinvgauss.ppf([0.001, 0.5, 0.999], mu)
np.allclose([0.001, 0.5, 0.999], recipinvgauss.cdf(vals, mu))
# True

# Generate random numbers:

r = recipinvgauss.rvs(mu, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
