# Design an analog highpass filter such that the passband is within 3 dB
# above 30 rad/s, while rejecting -60 dB at 10 rad/s.  Plot its
# frequency response, showing the passband and stopband constraints in gray.

from scipy import signal
import matplotlib.pyplot as plt

N, Wn = signal.ellipord(30, 10, 3, 60, True)
b, a = signal.ellip(N, 3, 60, Wn, 'high', True)
w, h = signal.freqs(b, a, np.logspace(0, 3, 500))
plt.semilogx(w, 20 * np.log10(abs(h)))
plt.title('Elliptical highpass filter fit to constraints')
plt.xlabel('Frequency [radians / second]')
plt.ylabel('Amplitude [dB]')
plt.grid(which='both', axis='both')
plt.fill([.1, 10,  10,  .1], [1e4, 1e4, -60, -60], '0.9', lw=0) # stop
plt.fill([30, 30, 1e9, 1e9], [-99,  -3,  -3, -99], '0.9', lw=0) # pass
plt.axis([1, 300, -80, 3])
plt.show()
