from scipy.stats import nct
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

df, nc = 14, 0.24
mean, var, skew, kurt = nct.stats(df, nc, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(nct.ppf(0.01, df, nc),
                nct.ppf(0.99, df, nc), 100)
ax.plot(x, nct.pdf(x, df, nc),
       'r-', lw=5, alpha=0.6, label='nct pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = nct(df, nc)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = nct.ppf([0.001, 0.5, 0.999], df, nc)
np.allclose([0.001, 0.5, 0.999], nct.cdf(vals, df, nc))
# True

# Generate random numbers:

r = nct.rvs(df, nc, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
