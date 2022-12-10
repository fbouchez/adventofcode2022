import sys

def read_grid():
    grid = []
    for l in sys.stdin:
        l = l.strip()
        row = list(map(int, list(l)))
        grid.append(row)
    return grid


grid = read_grid()

visible = [
    [False] * len(grid[0]) for l in grid
]


def test_tree(r, c, prev):
    h = grid[r][c]
    if h > prev:
        visible[r][c] = True
        prev = h
    return prev



def scan_rowOrCol(r, row=True):
    for it in [
        range(len(grid[r])),
        range(len(grid[r])-1,-1,-1)
    ]:
        prev = -1
        for c in it:
            if row:
                prev = test_tree(r, c, prev)
            else:
                prev = test_tree(c, r, prev)

def scan_all():
    for r in range(len(grid)):
        scan_rowOrCol(r, True)
        scan_rowOrCol(r, False)


def count_visible():
    cnt = 0

    for r in visible:
        for b in r:
            if b:
                cnt += 1

    return cnt


print (grid)
print (visible)
scan_all()

print (visible)

print (count_visible())

def compute_scenic_score(r, c):
    h = grid[r][c]

    tot_score = 1
    for dx, dy in [
            (-1, 0),
            (1, 0),
            (0, -1),
            (0, 1)
    ]:
        score = 0
        x = r
        y = c

        x += dx
        y += dy

        while (
                x >= 0 and x < len(grid)
            and y >= 0 and y < len(grid)
        ):
            score += 1
            if grid[x][y] >= h:
                break
            x += dx
            y += dy

        tot_score *= score

    return tot_score




best_score = 0

for r in range(len(grid)):
    for c in range(len(grid)):
        score = compute_scenic_score(r,c)
        if score > best_score:
            best_score = score


print ("Best tree score:", best_score)

