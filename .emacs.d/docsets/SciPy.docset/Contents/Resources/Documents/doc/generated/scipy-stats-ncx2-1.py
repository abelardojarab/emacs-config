from scipy.stats import ncx2
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

df, nc = 21, 1.06
mean, var, skew, kurt = ncx2.stats(df, nc, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(ncx2.ppf(0.01, df, nc),
                ncx2.ppf(0.99, df, nc), 100)
ax.plot(x, ncx2.pdf(x, df, nc),
       'r-', lw=5, alpha=0.6, label='ncx2 pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = ncx2(df, nc)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = ncx2.ppf([0.001, 0.5, 0.999], df, nc)
np.allclose([0.001, 0.5, 0.999], ncx2.cdf(vals, df, nc))
# True

# Generate random numbers:

r = ncx2.rvs(df, nc, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
