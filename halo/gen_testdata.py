"""
This program generates 3D positional data based on the Poisson distribution
for particles in a mock dark matter halo.
"""
#from __future__ import division
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

npart = 40000
nlambda = npart*10

#Generate the data
pos = []
for i in range(3):
	n = np.random.poisson(nlambda,npart)
	p = []
	for j in range(len(n)):
		#These scaling factors are adjusted to ensure values are generally between -1 and 1
		p.append((float(n[j] / npart) - 10.0)*(22.0))
	pos.append(p)
print(len(pos),len(pos[0]),pos[0][0])

#For displaying the mock halo
fig = plt.figure(figsize=(12,12))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(pos[0],pos[1],pos[2],s=0.2)
fig.savefig('test.png',format='png')

#Write data to file
#with open('test_data_1e5.dat','w') as f:
#	for i in range(len(pos[0])):
#		f.write("%.4f" % pos[0][i] + '\t' + "%.4f" % pos[1][i] + '\t' + "%.4f" % pos[2][i] + '\n')