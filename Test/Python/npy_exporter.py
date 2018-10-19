#!/usr/bin/env python
# coding=utf-8
import numpy as npy
from numpy import random
import wolframclient.serializers as wxf


def npy2wxf(path):
	data = npy.load(path)
	wxf.export(data, path + '.wxf', target_format='wxf')


npy.save('image_mean.npy', random.rand(1, 3, 32, 32))
npy2wxf('image_mean.npy')
