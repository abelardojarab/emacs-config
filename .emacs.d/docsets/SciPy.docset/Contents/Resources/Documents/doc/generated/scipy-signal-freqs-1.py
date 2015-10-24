from scipy.signal import freqs, iirfilter

b, a = iirfilter(4, [1, 10], 1, 60, analog=True, ftype='cheby1')

w, h = freqs(b, a, worN=np.logspace(-1, 2, 1000))

import matplotlib.pyplot as plt
plt.semilogx(w, 20 * np.log10(abs(h)))
plt.xlabel('Frequency')
plt.ylabel('Amplitude response [dB]')
plt.grid()
plt.show()
