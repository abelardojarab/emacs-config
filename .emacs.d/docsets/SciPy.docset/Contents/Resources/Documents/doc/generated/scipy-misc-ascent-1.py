import scipy.misc
ascent = scipy.misc.ascent()
ascent.shape
# (512, 512)
ascent.max()
# 255

import matplotlib.pyplot as plt
plt.gray()
plt.imshow(ascent)
plt.show()
