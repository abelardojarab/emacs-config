import matplotlib.pyplot as plt
from scipy.stats import wishart, chi2
x = np.linspace(1e-5, 8, 100)
w = wishart.pdf(x, df=3, scale=1); w[:5]
# array([ 0.00126156,  0.10892176,  0.14793434,  0.17400548,  0.1929669 ])
c = chi2.pdf(x, 3); c[:5]
# array([ 0.00126156,  0.10892176,  0.14793434,  0.17400548,  0.1929669 ])
plt.plot(x, w)

# The input quantiles can be any shape of array, as long as the last
# axis labels the components.
