from scipy.stats import nakagami
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

nu = 4.97
mean, var, skew, kurt = nakagami.stats(nu, moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(nakagami.ppf(0.01, nu),
                nakagami.ppf(0.99, nu), 100)
ax.plot(x, nakagami.pdf(x, nu),
       'r-', lw=5, alpha=0.6, label='nakagami pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = nakagami(nu)
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = nakagami.ppf([0.001, 0.5, 0.999], nu)
np.allclose([0.001, 0.5, 0.999], nakagami.cdf(vals, nu))
# True

# Generate random numbers:

r = nakagami.rvs(nu, size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
