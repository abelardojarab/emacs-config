import scipy.misc
face = scipy.misc.face()
face.shape
# (768, 1024, 3)
face.max()
# 230
face.dtype
# dtype('uint8')

import matplotlib.pyplot as plt
plt.gray()
plt.imshow(face)
plt.show()
