#!/usr/bin/env python3


all = []
current = 0
try:
    while True:
        l = input()
        if l == '':
            all.append(current)
            current = 0
            continue

        current += int(l)
except EOFError:
    all.append(current)


s = list(sorted(all))
print (s)

print (s[-1] + s[-2] + s[-3])

