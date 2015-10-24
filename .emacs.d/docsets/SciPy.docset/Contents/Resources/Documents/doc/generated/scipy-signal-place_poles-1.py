# A simple example demonstrating real pole placement using both KNV and YT
# algorithms.  This is example number 1 from section 4 of the reference KNV
# publication ([R194]_):

from scipy import signal
import matplotlib.pyplot as plt

A = np.array([[ 1.380,  -0.2077,  6.715, -5.676  ],
              [-0.5814, -4.290,   0,      0.6750 ],
              [ 1.067,   4.273,  -6.654,  5.893  ],
              [ 0.0480,  4.273,   1.343, -2.104  ]])
B = np.array([[ 0,      5.679 ],
              [ 1.136,  1.136 ],
              [ 0,      0,    ],
              [-3.146,  0     ]])
P = np.array([-0.2, -0.5, -5.0566, -8.6659])

# Now compute K with KNV method 0, with the default YT method and with the YT
# method while forcing 100 iterations of the algorithm and print some results
# after each call.

fsf1 = signal.place_poles(A, B, P, method='KNV0')
fsf1.gain_matrix
# array([[ 0.20071427, -0.96665799,  0.24066128, -0.10279785],
# [ 0.50587268,  0.57779091,  0.51795763, -0.41991442]])

fsf2 = signal.place_poles(A, B, P)  # uses YT method
fsf2.computed_poles
# array([-8.6659, -5.0566, -0.5   , -0.2   ])

fsf3 = signal.place_poles(A, B, P, rtol=-1, maxiter=100)
fsf3.X
# array([[ 0.52072442+0.j, -0.08409372+0.j, -0.56847937+0.j,  0.74823657+0.j],
# [-0.04977751+0.j, -0.80872954+0.j,  0.13566234+0.j, -0.29322906+0.j],
# [-0.82266932+0.j, -0.19168026+0.j, -0.56348322+0.j, -0.43815060+0.j],
# [ 0.22267347+0.j,  0.54967577+0.j, -0.58387806+0.j, -0.40271926+0.j]])

# The absolute value of the determinant of X is a good indicator to check the
# robustness of the results, both ``'KNV0'`` and ``'YT'`` aim at maximizing
# it.  Below a comparison of the robustness of the results above:

abs(np.linalg.det(fsf1.X)) < abs(np.linalg.det(fsf2.X))
# True
abs(np.linalg.det(fsf2.X)) < abs(np.linalg.det(fsf3.X))
# True

# Now a simple example for complex poles:

A = np.array([[ 0,  7/3.,  0,   0   ],
              [ 0,   0,    0,  7/9. ],
              [ 0,   0,    0,   0   ],
              [ 0,   0,    0,   0   ]])
B = np.array([[ 0,  0 ],
              [ 0,  0 ],
              [ 1,  0 ],
              [ 0,  1 ]])
P = np.array([-3, -1, -2-1j, -2+1j]) / 3.
fsf = signal.place_poles(A, B, P, method='YT')

# We can plot the desired and computed poles in the complex plane:

t = np.linspace(0, 2*np.pi, 401)
plt.plot(np.cos(t), np.sin(t), 'k--')  # unit circle
plt.plot(fsf.requested_poles.real, fsf.requested_poles.imag,
         'wo', label='Desired')
plt.plot(fsf.computed_poles.real, fsf.computed_poles.imag, 'bx',
         label='Placed')
plt.grid()
plt.axis('image')
plt.axis([-1.1, 1.1, -1.1, 1.1])
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, numpoints=1)
