import scipy.signal
import matplotlib.pyplot as plt

# First define some input parameters for the signal:

A = 2.
w = 1.
phi = 0.5 * np.pi
nin = 1000
nout = 100000
frac_points = 0.9 # Fraction of points to select

# Randomly select a fraction of an array with timesteps:

r = np.random.rand(nin)
x = np.linspace(0.01, 10*np.pi, nin)
x = x[r >= frac_points]
normval = x.shape[0] # For normalization of the periodogram

# Plot a sine wave for the selected times:

y = A * np.sin(w*x+phi)

# Define the array of frequencies for which to compute the periodogram:

f = np.linspace(0.01, 10, nout)

# Calculate Lomb-Scargle periodogram:

import scipy.signal as signal
pgram = signal.lombscargle(x, y, f)

# Now make a plot of the input data:

plt.subplot(2, 1, 1)
# <matplotlib.axes.AxesSubplot object at 0x102154f50>
plt.plot(x, y, 'b+')
# [<matplotlib.lines.Line2D object at 0x102154a10>]

# Then plot the normalized periodogram:

plt.subplot(2, 1, 2)
# <matplotlib.axes.AxesSubplot object at 0x104b0a990>
plt.plot(f, np.sqrt(4*(pgram/normval)))
# [<matplotlib.lines.Line2D object at 0x104b2f910>]
plt.show()
