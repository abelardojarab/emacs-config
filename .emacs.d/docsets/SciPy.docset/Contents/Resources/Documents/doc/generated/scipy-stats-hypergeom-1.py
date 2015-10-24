from scipy.stats import hypergeom
import matplotlib.pyplot as plt

# Suppose we have a collection of 20 animals, of which 7 are dogs.  Then if
# we want to know the probability of finding a given number of dogs if we
# choose at random 12 of the 20 animals, we can initialize a frozen
# distribution and plot the probability mass function:

[M, n, N] = [20, 7, 12]
rv = hypergeom(M, n, N)
x = np.arange(0, n+1)
pmf_dogs = rv.pmf(x)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x, pmf_dogs, 'bo')
ax.vlines(x, 0, pmf_dogs, lw=2)
ax.set_xlabel('# of dogs in our group of chosen animals')
ax.set_ylabel('hypergeom PMF')
plt.show()

# Instead of using a frozen distribution we can also use `hypergeom`
# methods directly.  To for example obtain the cumulative distribution
# function, use:

prb = hypergeom.cdf(x, M, n, N)

# And to generate random numbers:

R = hypergeom.rvs(M, n, N, size=10)
