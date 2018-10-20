#!/usr/bin/env python
# coding=utf-8
import torch
import wolframclient.serializers as wxf
from collections import OrderedDict

#params=torch.load('checkpoint.pth', map_location=torch.device('cpu'))
#print(params)

x = torch.tensor([[1, 2, 3], [4, 5, 6]])
print(x)
wxf.export(x, 'tensor.wxf', target_format='wxf')
