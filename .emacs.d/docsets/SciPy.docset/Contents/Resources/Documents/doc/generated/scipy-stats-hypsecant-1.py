from scipy.stats import hypsecant
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

mean, var, skew, kurt = hypsecant.stats(moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(hypsecant.ppf(0.01),
                hypsecant.ppf(0.99), 100)
ax.plot(x, hypsecant.pdf(x),
       'r-', lw=5, alpha=0.6, label='hypsecant pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = hypsecant()
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = hypsecant.ppf([0.001, 0.5, 0.999])
np.allclose([0.001, 0.5, 0.999], hypsecant.cdf(vals))
# True

# Generate random numbers:

r = hypsecant.rvs(size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
