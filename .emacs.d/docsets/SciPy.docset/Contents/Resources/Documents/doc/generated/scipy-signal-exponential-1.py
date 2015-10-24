# Plot the symmetric window and its frequency response:

from scipy import signal
from scipy.fftpack import fft, fftshift
import matplotlib.pyplot as plt

M = 51
tau = 3.0
window = signal.exponential(M, tau=tau)
plt.plot(window)
plt.title("Exponential Window (tau=3.0)")
plt.ylabel("Amplitude")
plt.xlabel("Sample")

plt.figure()
A = fft(window, 2048) / (len(window)/2.0)
freq = np.linspace(-0.5, 0.5, len(A))
response = 20 * np.log10(np.abs(fftshift(A / abs(A).max())))
plt.plot(freq, response)
plt.axis([-0.5, 0.5, -35, 0])
plt.title("Frequency response of the Exponential window (tau=3.0)")
plt.ylabel("Normalized magnitude [dB]")
plt.xlabel("Normalized frequency [cycles per sample]")

# This function can also generate non-symmetric windows:

tau2 = -(M-1) / np.log(0.01)
window2 = signal.exponential(M, 0, tau2, False)
plt.figure()
plt.plot(window2)
plt.ylabel("Amplitude")
plt.xlabel("Sample")
