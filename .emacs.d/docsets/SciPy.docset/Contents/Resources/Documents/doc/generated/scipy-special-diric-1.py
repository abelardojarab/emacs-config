from scipy import special
import matplotlib.pyplot as plt

x = np.linspace(-8*np.pi, 8*np.pi, num=201)
plt.figure(figsize=(8,8));
for idx, n in enumerate([2,3,4,9]):
    plt.subplot(2, 2, idx+1)
    plt.plot(x, special.diric(x, n))
    plt.title('diric, n={}'.format(n))
plt.show()
