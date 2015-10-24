# Plot a 13th-order filter's impulse response using both `lfilter` and
# `sosfilt`, showing the instability that results from trying to do a
# 13th-order filter in a single stage (the numerical error pushes some poles
# outside of the unit circle):

import matplotlib.pyplot as plt
from scipy import signal
b, a = signal.ellip(13, 0.009, 80, 0.05, output='ba')
sos = signal.ellip(13, 0.009, 80, 0.05, output='sos')
x = np.zeros(700)
x[0] = 1.
y_tf = signal.lfilter(b, a, x)
y_sos = signal.sosfilt(sos, x)
plt.plot(y_tf, 'r', label='TF')
plt.plot(y_sos, 'k', label='SOS')
plt.legend(loc='best')
plt.show()
