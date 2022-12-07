#!/usr/bin/env python3


import sys

stacks = [[' ']]

PREMIEREPARTIE = False


def apply_command(cmd):
    print ('split:', line.split())
    _,nb,_,init,_,dest = line.split()
    nb = int(nb)
    init = int(init)
    dest = int(dest)

    if PREMIEREPARTIE:
        for i in range(nb):
            caisse = stacks[init].pop()
            stacks[dest].append(caisse)
    else:
        caisses = stacks[init][-nb:]
        stacks[init] = stacks[init][:-nb]
        stacks[dest] += caisses

in_stacks = True

for line in sys.stdin:
    line = line.strip()
    if line == "":
        in_stacks = False
        continue

    if in_stacks:
        stacks.append(list(line))
    else:
        apply_command(line)



top_stacks = []

for s in stacks:
    top_stacks.append(s.pop())

print ("Valeur finale:", ''.join(top_stacks))



