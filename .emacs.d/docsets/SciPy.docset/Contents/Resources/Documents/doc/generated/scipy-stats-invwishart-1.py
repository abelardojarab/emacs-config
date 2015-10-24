import matplotlib.pyplot as plt
from scipy.stats import invwishart, invgamma
x = np.linspace(0.01, 1, 100)
iw = invwishart.pdf(x, df=6, scale=1)
iw[:3]
# array([  1.20546865e-15,   5.42497807e-06,   4.45813929e-03])
ig = invgamma.pdf(x, 6/2., scale=1./2)
ig[:3]
# array([  1.20546865e-15,   5.42497807e-06,   4.45813929e-03])
plt.plot(x, iw)

# The input quantiles can be any shape of array, as long as the last
# axis labels the components.
