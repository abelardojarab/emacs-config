import numpy as np
from scipy import signal, misc
import matplotlib.pyplot as plt

image = misc.lena()
w = signal.gaussian(50, 5.0)
image_new = signal.sepfir2d(image, w, w)

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
