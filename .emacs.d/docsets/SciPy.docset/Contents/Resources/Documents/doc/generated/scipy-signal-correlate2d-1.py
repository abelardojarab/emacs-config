# Use 2D cross-correlation to find the location of a template in a noisy
# image:

from scipy import signal
from scipy import misc
lena = misc.lena() - misc.lena().mean()
template = np.copy(lena[235:295, 310:370]) # right eye
template -= template.mean()
lena = lena + np.random.randn(*lena.shape) * 50 # add noise
corr = signal.correlate2d(lena, template, boundary='symm', mode='same')
y, x = np.unravel_index(np.argmax(corr), corr.shape) # find the match

import matplotlib.pyplot as plt
fig, (ax_orig, ax_template, ax_corr) = plt.subplots(1, 3)
ax_orig.imshow(lena, cmap='gray')
ax_orig.set_title('Original')
ax_orig.set_axis_off()
ax_template.imshow(template, cmap='gray')
ax_template.set_title('Template')
ax_template.set_axis_off()
ax_corr.imshow(corr, cmap='gray')
ax_corr.set_title('Cross-correlation')
ax_corr.set_axis_off()
ax_orig.plot(x, y, 'ro')
fig.show()
