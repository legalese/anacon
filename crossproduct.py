#!/usr/bin/env python

##
# Generate cross product of all actions in a dictionary
# John J. Camilleri, Jan 2012
#
# Usage:
#	crossproduct.py dictionary.txt
##

import sys
import re
import os
from subprocess import Popen, PIPE, STDOUT, call
from time import time, strftime
from cgi import escape

if __name__ == "__main__":
	
	if (len(sys.argv) < 2):
		exit("You must specify a dictionary filename, e.g.: " + sys.argv[0] + " dictionary.txt")

	dic = {}
	with open(sys.argv[1], 'r') as f:
		for ix,line in enumerate(f):
			key = line.split('=')[0].strip()
			value = line.split('=')[1].strip()
			dic[key] = value

	hist = []
	for (k1,v1) in dic.items():
		for (k2,v2) in dic.items():
			key = set( (k1, k2) )
			if not( key in hist ) and v1 != v2:
				print "%s # %s ;" % (v1, v2)
				hist.append(key)
