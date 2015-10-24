# Design a digital bandstop filter which rejects -60 dB from 0.2*(fs/2) to
# 0.5*(fs/2), while staying within 3 dB below 0.1*(fs/2) or above
# 0.6*(fs/2).  Plot its frequency response, showing the passband and
# stopband constraints in gray.

from scipy import signal
import matplotlib.pyplot as plt

N, Wn = signal.cheb2ord([0.1, 0.6], [0.2, 0.5], 3, 60)
b, a = signal.cheby2(N, 60, Wn, 'stop')
w, h = signal.freqz(b, a)
plt.semilogx(w / np.pi, 20 * np.log10(abs(h)))
plt.title('Chebyshev II bandstop filter fit to constraints')
plt.xlabel('Normalized frequency')
plt.ylabel('Amplitude [dB]')
plt.grid(which='both', axis='both')
plt.fill([.01, .1, .1, .01], [-3,  -3, -99, -99], '0.9', lw=0) # stop
plt.fill([.2,  .2, .5,  .5], [ 9, -60, -60,   9], '0.9', lw=0) # pass
plt.fill([.6,  .6,  2,   2], [-99, -3,  -3, -99], '0.9', lw=0) # stop
plt.axis([0.06, 1, -80, 3])
plt.show()
