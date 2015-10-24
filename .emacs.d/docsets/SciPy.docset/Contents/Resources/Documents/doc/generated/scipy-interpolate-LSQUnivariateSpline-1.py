from scipy.interpolate import LSQUnivariateSpline
import matplotlib.pyplot as plt
x = np.linspace(-3, 3, 50)
y = np.exp(-x**2) + 0.1 * np.random.randn(50)

# Fit a smoothing spline with a pre-defined internal knots:

t = [-1, 0, 1]
spl = LSQUnivariateSpline(x, y, t)

xs = np.linspace(-3, 3, 1000)
plt.plot(x, y, 'ro', ms=5)
plt.plot(xs, spl(xs), 'g-', lw=3)
plt.show()

# Check the knot vector:

spl.get_knots()
# array([-3., -1., 0., 1., 3.])
