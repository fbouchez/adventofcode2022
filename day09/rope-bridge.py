import sys

MAXSIZE = 1000

grid = [ [False] * MAXSIZE  for _ in range (MAXSIZE)]



hx = MAXSIZE // 2
hy = MAXSIZE // 2


# num_noeuds = 2
num_noeuds = 10


positions = [ (hx,hy) for _ in range(num_noeuds) ]

def apply_line(l):
    direct = l[0]
    num = int(l[2:])

    for i in range(num):
        move_one(direct)

vects = {
    'R': (1, 0),
    'L': (-1, 0),
    'U': (0, -1),
    'D': (0, 1),
}


def pretty_grid():

    print('-------------')
    for y in range (480,510):
        line = []
        for x in range (480, 510):
            if (x,y) in positions:
                i = positions.index((x,y))
                line.append(str(i))
            else:
                line.append('.')
        print (''.join(line))
    print('-------------')




def move_one(direct):
    # pretty_grid()
    # print ("Moving head in", direct)
    dx, dy = vects[direct]
    hx, hy = positions[0]
    hx += dx
    hy += dy
    positions[0] = hx, hy
    update_tail(1)


count_pos_uniques = 1

def update_tail(n):
    global count_pos_uniques

    hx, hy = positions[n-1]
    tx, ty = positions[n]

    diffx = abs(hx - tx)
    diffy = abs(hy - ty)

    if diffx <= 1 and diffy <= 1:
        return

    assert diffx <= 2 or diffy <= 2

    dx = (hx - tx)
    if dx: dx //= diffx
    dy = (hy - ty)
    if dy: dy //= diffy

    tx += dx
    ty += dy

    positions[n] = tx, ty

    if n < num_noeuds - 1:
        update_tail(n+1)
    else:
        if not grid[ty][tx]:
            grid[ty][tx] = True
            count_pos_uniques += 1



for l in sys.stdin:
    apply_line(l.strip())

print ("Nombre de positions uniques:", count_pos_uniques)

pretty_grid()
