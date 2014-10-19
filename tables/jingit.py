#!/usr/bin/env python

from jinja2 import Template
import json
import argparse
from jinja2 import Environment, FileSystemLoader
from jinja2 import Environment, PackageLoader

def prettyNum(value, dec=4):
  value = float(value)
  if (value==0):
    return "0"
  if (abs(value)<10**(-dec)):
    return "%.1e" % value
  else:
    return "%.3g" % value

def getJson(filename):
  with open(filename) as data_file:    
    data = json.load(data_file)
  return(data)



# create environment, add filter
env = Environment(loader=FileSystemLoader('./'))
env.filters['prettyNum'] = prettyNum

# process input arguments

parser = argparse.ArgumentParser(description='Load a json file and apply on template file.')
parser.add_argument('json', type=argparse.FileType('r'))
args = parser.parse_args()

# load and parse the json description
with args.json as data_file:    
    tpls = json.load(data_file)

for k in tpls.keys():
  print "processing ", k
  tpl = tpls[k]

  if isinstance(tpl['data'], dict):
    data = {}
    for k2 in tpl['data'].keys():
      data[k2] = getJson(tpl['data'][k2])
  else:
    data = getJson(tpl['data'])

  template = env.get_template(tpl["template"])
  print "  > template ", tpl["template"]
  print "  > data     ", tpl["data"]
  print "  > output   ", tpl["output"]
  fout = open(tpl["output"], 'w')
  fout.write(template.render(data).encode('utf-8'))
  fout.close()


