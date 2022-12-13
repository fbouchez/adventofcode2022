import sys
import ast
from enum import IntEnum
from functools import cmp_to_key


class Cmp(IntEnum):
    EQ = 0
    LT = -1
    GT = 1



def lessthan (la, lb):
    if type(la) == int and type(lb) == int:
        if la == lb:
            return Cmp.EQ
        elif la < lb:
            return Cmp.LT
        else:
            return Cmp.GT

    if type(la) == list and type(lb) == list:
        idx = 0
        while True:
            if idx == len(la):
                if idx == len(lb):
                    return Cmp.EQ
                else:
                    return Cmp.LT
            if idx == len(lb):
                return Cmp.GT

            cmp = lessthan(la[idx], lb[idx])
            if cmp != Cmp.EQ:
                return cmp
            idx += 1

    if type(la) == int:
        la = [la]
    else:
        assert type(lb) == int
        lb = [lb]

    return lessthan(la, lb)



def check_pair(la,lb):

    print (la," vs ", lb)

    cmp = lessthan(la, lb)
    assert cmp != Cmp.EQ

    if cmp == Cmp.LT:
        print ("Bon ordre")
        return True
    else:
        assert cmp == Cmp.GT
        print ("Mauvais ordre")
        return False


def convert_to_list(a):
    return ast.literal_eval(a)


lines = sys.stdin.readlines()
it = iter(lines)

pair_idx = 1
accu = 0

packets = []

while True:
    a = next(it, None)
    if a is None:
        break
    b = next(it)
    _ = next(it,None) # blank line

    la = convert_to_list(a)
    lb = convert_to_list(b)

    packets.append(la)
    packets.append(lb)

    if check_pair(la, lb):
        accu += pair_idx

    pair_idx += 1



print ("résulat", accu)


packets.append([[2]])
packets.append([[6]])

spack = sorted(packets, key = cmp_to_key(lessthan))
key=cmp_to_key

print (spack)

idx2 = spack.index([[2]])
idx6 = spack.index([[6]])

print ("Indices: ", idx2, idx6)
print ("Résultat partie 2", (idx2+1) * (idx6+1))

