
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

def prettyNum2(value,n):
    value = float(value)
    return '{number:.{digits}f}'.format(number=value, digits=n)


def prettyNum(value, dec=4):
    value = float(value)
    if (value == 0.0):
        return '{number:.{digits}f}'.format(number=0.0, digits=2)
    if (abs(value) > 0.0001):
        return '{number:.{digits}f}'.format(number=value, digits=dec)
    else:
        return '{number:.{digits}e}'.format(number=value, digits=dec)


def prettyTimes(value, times=1000, dec=0):
    value = float(value) * 1000
    if (value == 0):
        return '{number:.{digits}f}'.format(number=0.0, digits=2)
    return '{number:.{digits}f}'.format(number=value, digits=dec)


def prettyDollar(value, dec=4):
    value = float(value)
    if (value == 0):
        return "\\$ {number:.{digits}f}".format(number=0.0, digits=2)
    value = round(value,dec)
    return '\\$ {number:.{digits}f}'.format(number=value, digits=dec)


def prettyPerc(value, dec=4):
    value = float(value)
    if (value == 0):
        return r'{number:.{digits}f} \phantom{{,}} \% '.format(number=0.0, digits=2)
    value = round(value,dec)
    return r'{number:.{digits}f} \phantom{{,}} \% '.format(number=value, digits=dec)


def prettyPercLessOne(value,dec):
    value = float(value)
    if (value == 0):
        return r'{number:.{digits}f} \phantom{{,}} \% '.format(number=0, digits=2)
    value = 100 * (value - 1)
    return r'{number:.{digits}f} \phantom{{,}} \% '.format(number=value, digits=dec)


def getJson(filename):
    with open(filename) as data_file:
        data = json.load(data_file)
    return(data)


# create environment, add filter
env = Environment(loader=FileSystemLoader('./'))
env.filters['prettyNum2'] = prettyNum2
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
    print('processing ',k)
    tpl = tpls[k]

    if isinstance(tpl['data'], dict):
        data = {}
        for k2 in tpl['data'].keys():
            data[k2] = getJson(os.path.join(home,tpl['data'][k2]))
    else:
        data = getJson(os.path.join(home,tpl['data']))

    template = env.get_template(tpl["template"])
    print( '  > template ', tpl["template"])
    print( '  > data     ', os.path.join(home,tpl["data"]))
    print( '  > output   ', os.path.join(home,tpl["output"]))
    fout = open(os.path.join(home,tpl["output"]), 'wb')
    fout.write(template.render(data).encode('utf-8'))
    fout.close()
