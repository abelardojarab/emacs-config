import matplotlib.pyplot as plt
from scipy.interpolate import InterpolatedUnivariateSpline
x = np.linspace(-3, 3, 50)
y = np.exp(-x**2) + 0.1 * np.random.randn(50)
spl = InterpolatedUnivariateSpline(x, y)
plt.plot(x, y, 'ro', ms=5)
xs = np.linspace(-3, 3, 1000)
plt.plot(xs, spl(xs), 'g', lw=3, alpha=0.7)
plt.show()

# Notice that the ``spl(x)`` interpolates `y`:

spl.get_residual()
# 0.0
