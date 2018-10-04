#!/usr/bin/env python
# coding=utf-8
"""https://github.com/dmlc/gluon-cv"""

import gluoncv as gv
from gluoncv.utils import export_block


def zoo_import(name):
	"""Download from Gluoncv Zoo"""
	net = gv.model_zoo.get_model(name, pretrained=True)
	export_block(name, net, preprocess=True)


zoo_import('cifar_resnet20_v2')
zoo_import('cifar_resnet56_v2')
zoo_import('cifar_resnet110_v2')
zoo_import('ssd_512_resnet50_v1_coco')
