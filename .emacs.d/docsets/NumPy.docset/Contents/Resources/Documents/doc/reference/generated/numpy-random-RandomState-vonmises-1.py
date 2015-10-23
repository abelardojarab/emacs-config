# Draw samples from the distribution:

mu, kappa = 0.0, 4.0 # mean and dispersion
s = np.random.vonmises(mu, kappa, 1000)

# Display the histogram of the samples, along with
# the probability density function:

import matplotlib.pyplot as plt
from scipy.special import i0
plt.hist(s, 50, normed=True)
x = np.linspace(-np.pi, np.pi, num=51)
y = np.exp(kappa*np.cos(x-mu))/(2*np.pi*i0(kappa))
plt.plot(x, y, linewidth=2, color='r')
plt.show()
