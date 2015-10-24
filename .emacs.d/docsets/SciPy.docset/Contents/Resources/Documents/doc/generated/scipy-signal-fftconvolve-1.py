# Autocorrelation of white noise is an impulse.  (This is at least 100 times
# as fast as `convolve`.)

from scipy import signal
sig = np.random.randn(1000)
autocorr = signal.fftconvolve(sig, sig[::-1], mode='full')

import matplotlib.pyplot as plt
fig, (ax_orig, ax_mag) = plt.subplots(2, 1)
ax_orig.plot(sig)
ax_orig.set_title('White noise')
ax_mag.plot(np.arange(-len(sig)+1,len(sig)), autocorr)
ax_mag.set_title('Autocorrelation')
fig.tight_layout()
fig.show()

# Gaussian blur implemented using FFT convolution.  Notice the dark borders
# around the image, due to the zero-padding beyond its boundaries.
# The `convolve2d` function allows for other types of image boundaries,
# but is far slower.

from scipy import misc
lena = misc.lena()
kernel = np.outer(signal.gaussian(70, 8), signal.gaussian(70, 8))
blurred = signal.fftconvolve(lena, kernel, mode='same')

fig, (ax_orig, ax_kernel, ax_blurred) = plt.subplots(1, 3)
ax_orig.imshow(lena, cmap='gray')
ax_orig.set_title('Original')
ax_orig.set_axis_off()
ax_kernel.imshow(kernel, cmap='gray')
ax_kernel.set_title('Gaussian kernel')
ax_kernel.set_axis_off()
ax_blurred.imshow(blurred, cmap='gray')
ax_blurred.set_title('Blurred')
ax_blurred.set_axis_off()
fig.show()
