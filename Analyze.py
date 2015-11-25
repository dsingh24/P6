#! usr/bin/python

import argparse

def main():
    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('-infile')
    args = parser.parse_args()
    instr = {'mem_store':0,
            'mem_load':0,
            'load_addr':0,
            'load_indirect':0,
            'sub':0,
            'add':0,
            'jump':0
            }
    file = open(args.infile, 'r')
    for line in file:
        if line.find('sw') != -1:
            instr['mem_store'] = instr['mem_store'] + 1
        elif line.find('lw') != -1:
            instr['mem_load'] = instr['mem_load'] + 1
        elif line.find('la') != -1:
            instr['load_addr'] = instr['load_addr'] + 1
        elif line.find('li') != -1:
            instr['load_indirect'] = instr['load_indirect'] + 1
        elif line.find('sub') != -1:
            instr['sub'] = instr['sub'] + 1
        elif line.find('add') != -1:
            instr['add'] = instr['add'] + 1
        elif line.find('bne') != -1 or line.find('beq') != -1 or \
             line.find('glt') != -1 or line.find('bge') != -1 or \
             line.find('blt') != -1 or line.find('ble') != -1 or \
             line.find('jal') != -1 or line.find('b') != -1:
            instr['jump'] = instr['jump'] + 1

    print instr

if __name__ == '__main__':
    main()
