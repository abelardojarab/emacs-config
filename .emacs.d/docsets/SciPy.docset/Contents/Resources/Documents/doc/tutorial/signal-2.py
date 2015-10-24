import numpy as np
from scipy import signal, misc
import matplotlib.pyplot as plt

image = misc.lena()
w = np.zeros((50, 50))
w[0][0] = 1.0
w[49][25] = 1.0
image_new = signal.fftconvolve(image, w)

plt.figure()
plt.imshow(image)
plt.gray()
plt.title('Original image')
plt.show()

plt.figure()
plt.imshow(image_new)
plt.gray()
plt.title('Filtered image')
plt.show()
