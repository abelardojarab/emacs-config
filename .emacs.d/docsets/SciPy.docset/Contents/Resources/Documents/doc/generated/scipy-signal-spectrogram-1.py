from scipy import signal
import matplotlib.pyplot as plt

# Generate a test signal, a 2 Vrms sine wave whose frequency linearly changes
# with time from 1kHz to 2kHz, corrupted by 0.001 V**2/Hz of white noise
# sampled at 10 kHz.

fs = 10e3
N = 1e5
amp = 2 * np.sqrt(2)
noise_power = 0.001 * fs / 2
time = np.arange(N) / fs
freq = np.linspace(1e3, 2e3, N)
x = amp * np.sin(2*np.pi*freq*time)
x += np.random.normal(scale=np.sqrt(noise_power), size=time.shape)

# Compute and plot the spectrogram.

f, t, Sxx = signal.spectrogram(x, fs)
plt.pcolormesh(t, f, Sxx)
plt.ylabel('Frequency [Hz]')
plt.xlabel('Time [sec]')
plt.show()
