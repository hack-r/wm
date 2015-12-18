#!/usr/bin/env python3

import sklearn.feature_extraction
import csv

with open('T:/RNA/Baltimore/Jason/ad_hoc/wm/data/train.csv', newline='') as train:
	data = csv.reader(train, delimiter=',')
	for row in data:
		print(', '.join(row))
