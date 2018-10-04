#!/usr/bin/env python
# coding=utf-8
import mxnet as mx
import numpy as np

try:
	import cPickle as pickle
except:
	import pickle

obj = {'a': 'b', 'c': 'd'}
obj2 = [0, 1, 1, 0, 1]
f = open('obj.pkl', 'wb')
pickle.dump(obj, f, protocol=2)
pickle.dump(obj2, f, protocol=2)
f.close()

f = open('obj.pkl', 'rb')
x1 = pickle.load(f)
x2 = pickle.load(f)
print(x1, '\n', x2)
f.close()

# numpy.ndarray 数据无法用 write() 写入,数据间用'，'相隔
img = mx.nd.random.normal(shape=(32, 32)).asnumpy()
f = open("2darry.txt", 'w')
np.savetxt(f, img, delimiter=",")
f.close()

# 高维 numpy.ndarray 必须 reshape
a = np.random.rand(3, 32, 32)
np.savetxt('3darry.txt', a.flatten(), header=str(a.shape))
