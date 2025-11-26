#!/usr/bin/env python

import json
import sys
import numpy as np

import matplotlib

matplotlib.use('Agg')  # For headless use

import matplotlib.pyplot as plt

progname = sys.argv[1]
benchmark = sys.argv[2]
data_sizes = list(map(int, sys.argv[3:]))

opencl_filename = '{}-opencl.json'.format(progname)
c_filename = '{}-c.json'.format(progname)
multicore_filename = '{}-multicore.json'.format(progname)

opencl_json = json.load(open(opencl_filename))
c_json = json.load(open(c_filename))
multicore_json = json.load(open(multicore_filename))

key = '{}.fut:{}'.format(progname, benchmark)

opencl_measurements = opencl_json[key]['datasets']
c_measurements = c_json[key]['datasets']
multicore_measurements = multicore_json[key]['datasets']

opencl_runtimes = [
    np.mean(opencl_measurements['[{}]i32 [{}]i32'.format(n, n)]['runtimes']) / 1000
    for n in data_sizes
]
c_runtimes = [
    np.mean(c_measurements['[{}]i32 [{}]i32'.format(n, n)]['runtimes']) / 1000
    for n in data_sizes
]
multicore_runtimes = [
    np.mean(multicore_measurements['[{}]i32 [{}]i32'.format(n, n)]['runtimes']) / 1000
    for n in data_sizes
]

fig, ax = plt.subplots()

opencl_runtime_plot = ax.plot(data_sizes, opencl_runtimes, 'b-', label='OpenCL runtime')
c_runtime_plot = ax.plot(data_sizes, c_runtimes, 'g-', label='Sequential (C) runtime')
multicore_runtime_plot = ax.plot(data_sizes, multicore_runtimes, 'r-', label='Multicore runtime')

ax.set_xlabel('Input size')
ax.set_ylabel('Runtime (ms)', color='k')
ax.tick_params('y', colors='k')
plt.xticks(data_sizes, rotation='vertical')
ax.semilogx()

plots = opencl_runtime_plot + c_runtime_plot + multicore_runtime_plot
labels = [p.get_label() for p in plots]
ax.legend(plots, labels, loc=0)

fig.tight_layout()

plt.savefig('{}.pdf'.format(benchmark), bbox_inches='tight')