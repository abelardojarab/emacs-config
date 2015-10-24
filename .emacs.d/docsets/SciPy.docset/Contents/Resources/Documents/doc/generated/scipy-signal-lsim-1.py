# Simulate a double integrator y'' = u, with a constant input u = 1

from scipy import signal
system = signal.lti([[0., 1.], [0., 0.]], [[0.], [1.]], [[1., 0.]], 0.)
t = np.linspace(0, 5)
u = np.ones_like(t)
tout, y, x = signal.lsim(system, u, t)
import matplotlib.pyplot as plt
plt.plot(t, y)
