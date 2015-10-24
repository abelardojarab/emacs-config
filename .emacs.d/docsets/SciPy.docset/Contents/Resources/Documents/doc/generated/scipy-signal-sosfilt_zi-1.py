# Filter a rectangular pulse that begins at time 0, with and without
# the use of the `zi` argument of `scipy.signal.sosfilt`.

from scipy import signal
import matplotlib.pyplot as plt

sos = signal.butter(9, 0.125, output='sos')
zi = signal.sosfilt_zi(sos)
x = (np.arange(250) < 100).astype(int)
f1 = signal.sosfilt(sos, x)
f2, zo = signal.sosfilt(sos, x, zi=zi)

plt.plot(x, 'k--', label='x')
plt.plot(f1, 'b', alpha=0.5, linewidth=2, label='filtered')
plt.plot(f2, 'g', alpha=0.25, linewidth=4, label='filtered with zi')
plt.legend(loc='best')
plt.show()
