#!/usr/bin/env python

# process input arguments
# from jinja2 import Template
import json
import argparse
import os
from jinja2 import Environment, FileSystemLoader


# def perc(x,y):
#   x = float(x)
#   y = float(y)
#   return((x- y) / y)

def prettyNum(value, dec=4):
    value = float(value)
    if (value == 0):
        return "%.2f" % 0
    value = round(value,dec)
    if (abs(value) > 0.0001):
        return "%g" % value
    else:
        return "%.2e" % value


def prettyTimes(value, times=1000, dec=0):
    value = float(value) * 1000
    if (value == 0):
        return "%.2f" % 0
    return "%g" % value


def prettyDollar(value, dec=4):
    value = float(value)
    if (value == 0):
        return "\\$ %.2f" % 0
    value = round(value,dec)
    if (abs(value) > 0.0001):
        return "\\$ %g" % value
    elif (abs(value) >= 0.00001):
        return "\\$ %.5f" % value


def prettyPerc(value, dec=4):
    value = float(value)
    if (value == 0):
        return "%.2f \phantom{,}\\%%" % 0
    value = round(value,dec)
    return "%g \phantom{,}\\%%" % value


def prettyPercLessOne(value,dec):
    value = float(value)
    if (value == 0):
        return "%.2f \phantom{,}\\%%" % 0
    value = round(100 * (value - 1),dec)
    pval = '{0:.1f}'.format(value)
    return "%s \phantom{,}\\%%" % pval


def getJson(filename):
    with open(filename) as data_file:
        data = json.load(data_file)
    return(data)


# create environment, add filter
env = Environment(loader=FileSystemLoader('./'))
env.filters['prettyNum'] = prettyNum
env.filters['prettyPerc'] = prettyPerc
env.filters['prettyPercLessOne'] = prettyPercLessOne
env.filters['prettyDollar'] = prettyDollar
env.filters['prettyTimes'] = prettyTimes


parser = argparse.ArgumentParser(description='Load a json file and apply on template file.')
parser.add_argument('json', type=argparse.FileType('r'))
args = parser.parse_args()

# load and parse the json description
with args.json as data_file:
    tpls = json.load(data_file)


home = os.environ['HOME']

for k in tpls.keys():
    print "processing ", k
    tpl = tpls[k]

    if isinstance(tpl['data'], dict):
        data = {}
        for k2 in tpl['data'].keys():
            data[k2] = getJson(os.path.join(home,tpl['data'][k2]))
    else:
        data = getJson(os.path.join(home,tpl['data']))

    template = env.get_template(tpl["template"])
    print "  > template ", tpl["template"]
    print "  > data     ", os.path.join(home,tpl["data"])
    print "  > output   ", os.path.join(home,tpl["output"])
    fout = open(os.path.join(home,tpl["output"]), 'w')
    fout.write(template.render(data).encode('utf-8'))
    fout.close()
