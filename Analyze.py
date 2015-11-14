#! usr/bin/python

import re

def main():
    inst = {'mem_store':0,
            'mem_load':0,
            'load_addr':0,
            'load_indirect':0,
            'sub':0,
            'add':0,
            'jump':0
            }
    file = open('test_dummy.out', 'r')
    for line in file:
        if (re.search('sw', line) != None):
            inst['mem_store'] = inst['mem_store'] + 1
        elif (re.search('lw', line) != None):
            inst['mem_load'] = inst['mem_load'] + 1
        elif (re.search('la', line) != None):
            inst['load_addr'] = inst['load_addr'] + 1
        elif (re.search('li', line) != None):
            inst['load_indirect'] = inst['load_indirect'] + 1
        elif (re.search('sub', line) != None):
            inst['sub'] = inst['sub'] + 1
        elif (re.search('add', line) != None):
            inst['add'] = inst['add'] + 1
        elif (re.search('bne' or 'beq' or 'bgt' or 'bge' or 'blt' or 'ble' or 'jal' or ' b ', line) != None):
            inst['jump'] = inst['jump'] + 1

    print inst

if __name__ == '__main__':
    main()
