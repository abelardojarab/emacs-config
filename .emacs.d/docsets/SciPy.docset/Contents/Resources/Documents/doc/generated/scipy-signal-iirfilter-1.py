# Generate a 17th-order Chebyshev II bandpass filter and plot the frequency
# response:

from scipy import signal
import matplotlib.pyplot as plt

b, a = signal.iirfilter(17, [50, 200], rs=60, btype='band',
                        analog=True, ftype='cheby2')
w, h = signal.freqs(b, a, 1000)
fig = plt.figure()
ax = fig.add_subplot(111)
ax.semilogx(w, 20 * np.log10(abs(h)))
ax.set_title('Chebyshev Type II bandpass frequency response')
ax.set_xlabel('Frequency [radians / second]')
ax.set_ylabel('Amplitude [dB]')
ax.axis((10, 1000, -100, 10))
ax.grid(which='both', axis='both')
plt.show()
