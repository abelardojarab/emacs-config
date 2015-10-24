from scipy.stats import frechet_r
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

c = 1.89
mean, var, skew, kurt = frechet_r.stats(c, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(frechet_r.ppf(0.01, c),
                frechet_r.ppf(0.99, c), 100)
ax.plot(x, frechet_r.pdf(x, c),
       'r-', lw=5, alpha=0.6, label='frechet_r pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = frechet_r(c)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = frechet_r.ppf([0.001, 0.5, 0.999], c)
np.allclose([0.001, 0.5, 0.999], frechet_r.cdf(vals, c))
# True

# Generate random numbers:

r = frechet_r.rvs(c, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
