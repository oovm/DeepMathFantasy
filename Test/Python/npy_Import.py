#!/usr/bin/env python
# coding=utf-8
import mxnet as mx
import numpy as np

# numpy.ndarray 数据无法用 write() 写入
img = mx.nd.random.normal(shape=(32, 32)).asnumpy()
f = open("2darry.txt", 'w')
np.savetxt(f, img, delimiter=",")
f.close()

# 高维 numpy.ndarray 必须 reshape
# a = np.random.rand(3, 32, 32)
# np.savetxt('3darry.txt', a.flatten(), header=str(a.shape))


data = np.load("image_mean.npy")
np.savetxt('3darry.txt', data.flatten(), header=str(data.shape))
