import sys

# INPUTSMALL = False
INPUTSMALL = True

class Grid:
    def __init__(self, lox, hix, loy, hiy):

        self.grid = [
            ['.'] * (hix - lox + 1) for _ in range(hiy - loy + 1)
        ]

        self.lox = lox
        self.loy = loy


    def get(self, x, y):
        return self.grid[y - self.loy][x - self.lox]

    def set(self, x, y, ch):
        print("Setting grid at", x, y)

        offx = x - self.lox
        offy = y - self.loy

        if self.grid[offy][offx] == '.':
            self.grid[offy][offx] = ch

    def not_possible(self, yline):
        return (
            self.grid[yline - self.loy].count('#')
            # len(self.grid[yline - self.loy])
            # - self.grid[yline - self.loy].count('.')
            # - self.grid[yline - self.loy].count('.')
            # - self.grid[yline - self.loy].count('.')
            )



    def __str__(self):
        lns = []
        for row in self.grid:
            lns.append(''.join(row))
        return '\n'.join(lns)


class Yline:
    def __init__(self, lox, hix, y):
        self.line = ['.'] * (hix - lox + 1)
        self.lox = lox
        self.hix = hix
        self.y = y


    def set(self, x, y, ch):
        # print ("Setting ", ch, "at", x, y)
        if y != self.y:
            return
        # print("Setting grid at", x, y)

        offx = x - self.lox

        if self.line[offx] == '.':
            # print ("\tREALLY setting ", ch, "at", x, y)
            self.line[offx] = ch


    def not_possible(self, yline=None): # second arg ignored
        return (
            # len(self.line) - self.line.count('.')
            self.line.count('#')
            )

    def count_sensors(self):
        return self.line.count('S')

    def count_beacons(self):
        return self.line.count('B')

    def __str__(self):
        return ''.join(self.line)




class Sensor:
    def __init__(self, x, y, xb, yb):
        self.x = x
        self.y = y
        self.radius = abs(xb - x) + abs(yb - y)


    def put_in_line(self, yline):
        print ("Checking sensor", self.x, self.y, self.radius)
        checky = yline.y

        if abs(checky - self.y) > self.radius:
            # no intersection between yline and sensor disc
            return

        intersect_width = self.radius - abs(checky - self.y)

        for dx in range(intersect_width + 1):
            yline.set(self.x + dx, yline.y, '#')
            yline.set(self.x - dx, yline.y, '#')


    def debug_show_disc(self, grid, ch='#'):
        print ("Checking sensor", self.x, self.y, self.radius)

        for dy in range(-self.radius, self.radius + 1):
            y = self.y + dy
            intersect_width = self.radius - abs(dy)
            for dx in range(intersect_width + 1):
                grid.set(self.x + dx, y, ch)
                grid.set(self.x - dx, y, ch)


def parse_sensor(l, gridline):
    lst = l.split()
    x = int(lst[2][2:-1])
    y = int(lst[3][2:-1])

    xb = int(lst[8][2:-1])
    yb = int(lst[9][2:])

    gridline.set(x, y, 'S')
    gridline.set(xb, yb, 'B')

    return Sensor(x, y, xb, yb)




if INPUTSMALL:
    gridline = Grid(-10, 40, -10, 40)
    print (gridline)
else:
    gridline = Yline(-4_000_000, 8_000_000, 2_000_000)
    # gridline = Yline(-4_000_000, 8_000_000, 10)


sensors = []
for l in sys.stdin:
    sensors.append( parse_sensor(l, gridline) )



if INPUTSMALL:
    print (gridline)
else:
    print ("Number of sensors in line:", gridline.count_sensors())
    print ("Number of beacons in line:", gridline.count_beacons())
# else:
    # print (gridline)


# for s in sensors:
    # if s.x == 8 and s.y == 7:
        # s.debug_show_disc(grid)

nch = ord('0')
for s in sensors:
    nch += 1
    if INPUTSMALL:
        s.debug_show_disc(gridline, chr(nch))
    else:
        s.put_in_line(gridline)
        print ("Number of not possible positions:", gridline.not_possible())

if INPUTSMALL:
    print (gridline)
    checky = 10
    print ("Number of not possible positions:", gridline.not_possible(checky))


else:
    print ("Number of not possible positions:", gridline.not_possible())
    print ("Number of sensors in line:", gridline.count_sensors())
    print ("Number of beacons in line:", gridline.count_beacons())
    print (''.join(gridline.line[3_999_996:4_000_030]))

####B######################
####B#####################

#too high 5367038
#too low  5367035
#RIGHT:   5367037 <-- but why !??!
