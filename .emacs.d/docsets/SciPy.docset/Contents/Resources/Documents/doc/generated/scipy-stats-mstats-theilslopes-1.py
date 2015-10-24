from scipy import stats
import matplotlib.pyplot as plt

x = np.linspace(-5, 5, num=150)
y = x + np.random.normal(size=x.size)
y[11:15] += 10  # add outliers
y[-5:] -= 7

# Compute the slope, intercept and 90% confidence interval.  For comparison,
# also compute the least-squares fit with `linregress`:

res = stats.theilslopes(y, x, 0.90)
lsq_res = stats.linregress(x, y)

# Plot the results. The Theil-Sen regression line is shown in red, with the
# dashed red lines illustrating the confidence interval of the slope (note
# that the dashed red lines are not the confidence interval of the regression
# as the confidence interval of the intercept is not included). The green
# line shows the least-squares fit for comparison.

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x, y, 'b.')
ax.plot(x, res[1] + res[0] * x, 'r-')
ax.plot(x, res[1] + res[2] * x, 'r--')
ax.plot(x, res[1] + res[3] * x, 'r--')
ax.plot(x, lsq_res[1] + lsq_res[0] * x, 'g-')
plt.show()
