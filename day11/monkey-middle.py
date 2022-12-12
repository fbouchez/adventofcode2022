import sys
# import queue
from collections import deque 

# num_monkeys = 4
num_monkeys = 8

PREMIEREPARTIE = False

class Monkey:

    def __init__(self):
        self.id = None
        self.num_inspect = 0
        self.items = None
        self.op = None
        self.test = None # entier pour test divisibilité
        self.iftrue = None # monkey to send if true
        self.iffalse = None # monkey to send if false

    def __str__(self):
        return (
            "Monkey" + str(self.id) +
            "\n\t" + str(self.items) +
            "\n\t" + self.op +
            "\n\ttest " + str(self.test) +
            "\n\ttrue " + str(self.iftrue) +
            "\n\tfalse " + str(self.iffalse) +
            "\n\tinspects " + str(self.num_inspect)
        )


    def receive_item(self, n):
        # self.items.put(n)
        self.items.append(n)

    def incr_inspect(self):
        self.num_inspect += 1

    def simulate_round(self):
        while self.items:
            # old = self.items.get()
            old = self.items.popleft()
            self.incr_inspect()
            _locals = locals()
            # print (_locals)
            exec(self.op, None, _locals)
            # print (_locals)
            new = _locals['new']
            # print ("Nouvelle valeur:", new)

            if PREMIEREPARTIE:
                new = new // 3
            else:
                new = new % ppm

            if new % self.test == 0:
                send_to(self.iftrue, new)
            else:
                send_to(self.iffalse, new)



def send_to(midx, it):
    monkey = monkeys[midx]
    monkey.receive_item(it)




monkeys = [ Monkey() for _ in range (num_monkeys)]

lines = sys.stdin.readlines()

def get_int(l):
    lst = l.split()
    return int(lst[-1])


midx = 0
monkey = monkeys[midx]

for l in lines:
    l = l.strip()

    if l == "":
        midx += 1
        if midx == num_monkeys:
            break
        monkey = monkeys[midx]
        continue

    if l.startswith('Monkey'):
        assert (l[7] == str(midx))
        monkey.id = midx
        continue

    if l.startswith('Starting'):
        _,rst = l.split(": ")

        monkey.items = deque(
            map(int, rst.split(", "))
        )
        # for it in rst.split(", "):
            # monkey.receive_item(it)
        continue

    if l.startswith('Operation'):
        _,rst = l.split(": ")
        monkey.op = rst
        continue

    if l.startswith('Test'):
        monkey.test = get_int(l)
        continue

    if l.startswith('If true'):
        monkey.iftrue = get_int(l)
        continue

    if l.startswith('If false'):
        monkey.iffalse = get_int(l)
        continue

    assert false



ppm = 1

for m in monkeys:
    print(m)
    ppm *= m.test


print ("ppm", ppm)

def do_round(i):
    for m in monkeys:
        m.simulate_round()

# for midx in range(1):
# for midx in range(20):
for midx in range(10000):
    do_round(midx)



insps = []

for m in monkeys:
    print(m)
    insps.append (m.num_inspect)

x,y = sorted(insps)[-2:]

print ("plus grands:", x, y)
print (x*y)

