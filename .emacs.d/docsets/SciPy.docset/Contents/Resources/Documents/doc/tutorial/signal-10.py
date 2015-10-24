import numpy as np
import scipy.signal as signal
import matplotlib.pyplot as plt

t = np.linspace(-10, 10, 20)
y = 1 + t + 0.01*t**2
yconst = signal.detrend(y, type='constant')
ylin = signal.detrend(y, type='linear')

plt.plot(t, y, '-rx')
plt.plot(t, yconst, '-bo')
plt.plot(t, ylin, '-k+')
plt.grid()
plt.legend(['signal', 'const. detrend', 'linear detrend'])
plt.show()
