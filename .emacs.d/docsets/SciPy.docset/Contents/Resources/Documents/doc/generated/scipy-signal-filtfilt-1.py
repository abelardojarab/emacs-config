# The examples will use several functions from `scipy.signal`.

from scipy import signal
import matplotlib.pyplot as plt

# First we create a one second signal that is the sum of two pure sine
# waves, with frequencies 5 Hz and 250 Hz, sampled at 2000 Hz.

t = np.linspace(0, 1.0, 2001)
xlow = np.sin(2 * np.pi * 5 * t)
xhigh = np.sin(2 * np.pi * 250 * t)
x = xlow + xhigh

# Now create a lowpass Butterworth filter with a cutoff of 0.125 times
# the Nyquist rate, or 125 Hz, and apply it to ``x`` with `filtfilt`.
# The result should be approximately ``xlow``, with no phase shift.

b, a = signal.butter(8, 0.125)
y = signal.filtfilt(b, a, x, padlen=150)
np.abs(y - xlow).max()
# 9.1086182074789912e-06

# We get a fairly clean result for this artificial example because
# the odd extension is exact, and with the moderately long padding,
# the filter's transients have dissipated by the time the actual data
# is reached.  In general, transient effects at the edges are
# unavoidable.

# The following example demonstrates the option ``method="gust"``.

# First, create a filter.

b, a = signal.ellip(4, 0.01, 120, 0.125)  # Filter to be applied.
np.random.seed(123456)

# `sig` is a random input signal to be filtered.

n = 60
sig = np.random.randn(n)**3 + 3*np.random.randn(n).cumsum()

# Apply `filtfilt` to `sig`, once using the Gustafsson method, and
# once using padding, and plot the results for comparison.

fgust = signal.filtfilt(b, a, sig, method="gust")
fpad = signal.filtfilt(b, a, sig, padlen=50)
plt.plot(sig, 'k-', label='input')
plt.plot(fgust, 'b-', linewidth=4, label='gust')
plt.plot(fpad, 'c-', linewidth=1.5, label='pad')
plt.legend(loc='best')
plt.show()

# The `irlen` argument can be used to improve the performance
# of Gustafsson's method.

# Estimate the impulse response length of the filter.

z, p, k = signal.tf2zpk(b, a)
eps = 1e-9
r = np.max(np.abs(p))
approx_impulse_len = int(np.ceil(np.log(eps) / np.log(r)))
approx_impulse_len
# 137

# Apply the filter to a longer signal, with and without the `irlen`
# argument.  The difference between `y1` and `y2` is small.  For long
# signals, using `irlen` gives a significant performance improvement.

x = np.random.randn(5000)
y1 = signal.filtfilt(b, a, x, method='gust')
y2 = signal.filtfilt(b, a, x, method='gust', irlen=approx_impulse_len)
print(np.max(np.abs(y1 - y2)))
# 1.80056858312e-10
