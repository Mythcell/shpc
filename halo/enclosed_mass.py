"""
This program plots the enclosed mass within a dark matter halo
"""

import numpy as np
import collections
import matplotlib.pyplot as plt

#First do the enclosed mass
with open('results.dat','r') as f:
	vals = np.loadtxt(f)
	
rtw = {}
for i in vals:
	rtw[i[0]] = i[2]
r_to_w = collections.OrderedDict(sorted(rtw.items()))

wc = 0
for k, v in r_to_w.items():
	wc = wc + v
	r_to_w[k] = wc

fig = plt.figure(figsize=(6,6))
plt.plot(r_to_w.keys(),r_to_w.values())
plt.xlabel('radius'); plt.ylabel('mass/total mass'); plt.xscale('log'); plt.grid(True)
plt.yscale('log')
fig.savefig('encmass.png',bbox_inches='tight',format='png')
fig.clear()

#Then do the annihilation luminosity
with open('flux.dat','r') as f:
	vals = np.loadtxt(f)

rtl = {}
for i in vals:
	rtl[i[0]] = i[1]
r_to_l = collections.OrderedDict(sorted(rtl.items()))

lc = 0
for k, v in r_to_l.items():
	lc = lc + v
	r_to_l[k] = lc

plt.plot(r_to_l.keys(),r_to_l.values())
plt.xlabel('radius'); plt.ylabel('luminosity/total luminosity'); plt.xscale('log'); plt.grid(True)
plt.yscale('log')
fig.savefig('enclum.png',bbox_inches='tight',format='png')
fig.clear()

plt.plot(r_to_w.keys(),r_to_w.values())
plt.plot(r_to_l.keys(),r_to_l.values())
plt.xlabel('radius'); plt.ylabel('mass/total mass, luminosity/total luminosity'); plt.xscale('log'); plt.grid(True)
plt.yscale('log'); #plt.xlim(0.0005, 1)
fig.savefig('encboth_a.png',bbox_inches='tight',format='png')


