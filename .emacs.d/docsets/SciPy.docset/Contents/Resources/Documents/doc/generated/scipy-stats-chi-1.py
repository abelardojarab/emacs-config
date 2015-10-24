from scipy.stats import chi
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

df = 78
mean, var, skew, kurt = chi.stats(df, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(chi.ppf(0.01, df),
                chi.ppf(0.99, df), 100)
ax.plot(x, chi.pdf(x, df),
       'r-', lw=5, alpha=0.6, label='chi pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = chi(df)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = chi.ppf([0.001, 0.5, 0.999], df)
np.allclose([0.001, 0.5, 0.999], chi.cdf(vals, df))
# True

# Generate random numbers:

r = chi.rvs(df, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
