import sys


CAVE_WIDTH = 700
CAVE_HEIGHT = 300

sand_init_x = 500


cave = [ ['.'] * CAVE_WIDTH for _ in range (CAVE_HEIGHT)]



def init_cave(cave):
    global CAVE_WIDTH
    maxy = 0
    for l in sys.stdin:
        coords = l.strip().split(' -> ')
        prev = None
        for c in coords:
            x,y = list(map(int, c.split(',')))

            if y > maxy:
                maxy = y

            if prev == None:
                prev = x,y
                continue
            else:
                px, py = prev


            draw_line(cave, x,y,px,py)
            prev = x,y

    # START Part 2
    for x in range(CAVE_WIDTH):
        cave[maxy+2][x] = '#'
    # END Part 2


    cave[0][sand_init_x] = '+'


def draw_line(cave, x,y,px,py):
    dx = (x - px)
    if dx: dx //= abs (x - px)
    dy = (y - py)
    if dy: dy //= abs (y - py)

    while px != x or py != y:
        cave[py][px] = '#'
        px += dx
        py += dy
    cave[py][px] = '#'



def pretty_cave(cave):
    for y in range(12):
        print (''.join(cave[y][493:505]))



num_grain_of_sand_stuck = 0

def simulate_one_grain_of_sand(cave):
    """
    Return True if the grain is stuck,
    False if the grain of sand falls forever
    """
    global sand_init_x, num_grain_of_sand_stuck

    x = sand_init_x
    y = 0
    while y < len(cave)-1:
        if cave[y+1][x] == '.':
            y += 1
            continue
        if cave[y+1][x-1] == '.':
            y += 1
            x -= 1
            continue
        if cave[y+1][x+1] == '.':
            y += 1
            x += 1
            continue

        # le grain est bloqué
        num_grain_of_sand_stuck += 1

        # START Part 2
        if cave[y][x] == '+':
            # end of simulation of part 2
            cave[y][x] = 'o'
            return False

        # END Part 2

        cave[y][x] = 'o'
        return True

    return False



def simulate_cave(cave):
    while simulate_one_grain_of_sand(cave):
        pretty_cave(cave)
        pass

init_cave(cave)
simulate_cave(cave)

print ("Total grains:", num_grain_of_sand_stuck)

