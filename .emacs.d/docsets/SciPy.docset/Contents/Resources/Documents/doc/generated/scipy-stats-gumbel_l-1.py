from scipy.stats import gumbel_l
import matplotlib.pyplot as plt
fig, ax = plt.subplots(1, 1)

# Calculate a few first moments:

mean, var, skew, kurt = gumbel_l.stats(moments='mvsk')

# Display the probability density function (``pdf``):

x = np.linspace(gumbel_l.ppf(0.01),
                gumbel_l.ppf(0.99), 100)
ax.plot(x, gumbel_l.pdf(x),
       'r-', lw=5, alpha=0.6, label='gumbel_l pdf')

# Alternatively, the distribution object can be called (as a function)
# to fix the shape, location and scale parameters. This returns a "frozen"
# RV object holding the given parameters fixed.

# Freeze the distribution and display the frozen ``pdf``:

rv = gumbel_l()
ax.plot(x, rv.pdf(x), 'k-', lw=2, label='frozen pdf')

# Check accuracy of ``cdf`` and ``ppf``:

vals = gumbel_l.ppf([0.001, 0.5, 0.999])
np.allclose([0.001, 0.5, 0.999], gumbel_l.cdf(vals))
# True

# Generate random numbers:

r = gumbel_l.rvs(size=1000)

# And compare the histogram:

ax.hist(r, normed=True, histtype='stepfilled', alpha=0.2)
ax.legend(loc='best', frameon=False)
plt.show()
