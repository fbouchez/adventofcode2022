import sys

INPUTSMALL = False
# INPUTSMALL = True

if INPUTSMALL:
    xlimit = 20
    ylimit = 20
else:
    xlimit = 4_000_000
    ylimit = 4_000_000

def distance(x,y,x2,y2):
    return abs(x2 - x) + abs(y2 - y)



class Grid:
    def __init__(self, lox, hix, loy, hiy):

        self.grid = [
            ['.'] * (hix - lox + 1) for _ in range(hiy - loy + 1)
        ]

        self.lox = lox
        self.loy = loy


    def get(self, x, y):
        return self.grid[y - self.loy][x - self.lox]

    def set(self, x, y, ch, force=False):
        # print("Setting grid at", x, y)

        offx = x - self.lox
        offy = y - self.loy

        if self.grid[offy][offx] == '.' or force:
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


items = {}


class Sensor:
    char = '0'

    def __init__(self, x, y, xb, yb):
        self.x = x
        self.y = y
        self.radius = distance(x,y,xb,yb)

        assert (x,y) not in items
        items[(x,y)] = 'sensor'

        if (xb,yb) in items:
            print ('Already in items', items[(xb,yb)])
        else:
            items[(xb,yb)] = 'beacon'

        self.char = Sensor.char
        Sensor.char = chr(ord(Sensor.char) + 1)


    def __str__(self):
        return (
            "Sensor" + self.char + "(" + str(self.x) +
            ", " + str(self.y) +
            " r=" + str(self.radius) + ")"
        )


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


    def debug_show_disc(self, grid):
        print ("Checking sensor", self.x, self.y, self.radius)

        for dy in range(-self.radius, self.radius + 1):
            y = self.y + dy
            intersect_width = self.radius - abs(dy)
            for dx in range(intersect_width + 1):
                grid.set(self.x + dx, y, self.char)
                grid.set(self.x - dx, y, self.char)

    def inside(self, x,y):
        return distance(self.x, self.y, x, y) <= self.radius


    def intersect_edge(self, sens):
        """
        find the two "intersection" points of the sensors:
        the the points just outside of the sensors, at the intersection
        of the two circles of radii +1
        """


        x = self.x
        y = self.y
        r = self.radius
        s = self

        x2 = sens.x
        y2 = sens.y
        r2 = sens.radius
        s2 = sens

        if (x2 == x):
            print ("ALERT: discarding on x", s, s2)
            return [] # TODO: adjust if necessary to handle

        if (y2 == y):
            print ("ALERT: discarding on y", s, s2)
            return [] # TODO: adjust if necessary

        # TODO: adjust >= to > if we do not find candidates
        if distance(x,y,x2,y2) >= r + r2:
            print ("No intersection", s, s2, distance(x,y,x2,y2), ">=", r+r2)
            if distance(x,y,x2,r2) <= r + r2 + 1:
                print ("ALERT: discarding too just", s, s2)
            return []

        if (x2 < x):
            # will be handled later, or has already been handled
            return []
            # swapping (what we were doing before)
            (x,y,r,x2,y2,r2) = (x2,y2,r2,x,y,r)
            s = sens
            s2 = self

        # x is necessary < x2 here
        # compute extremities of first sensor toward s2

        if y2 < y:
            points = [
                    # first point to check
                    #        start  diag
                ( ((x, y-r), (0, -1, 1, 1)),  # if outside
                  ((x-r, y), (-1, 0, 1, -1))   # if inside
                ),
                    # second point to check
                ( ((x+r, y), (1, 0, -1, -1)),  # if outside
                  ((x, y+r), (0, 1, 1, -1))  # if inside
                )
            ]

        else:
            points = [
                    # first point to check
                    #        start  diag
                ( ((x+r, y), (1, 0, -1, 1)),  # if outside
                  ((x, y-r), (0, -1, 1, 1))   # if inside
                ),
                    # second point to check
                ( ((x, y+r), (0, 1, 1, -1)),  # if outside
                  ((x-r, y), (-1, 0, 1, 1)),  # if inside
                )

            ]

        extrem = []
        for (point, backup) in points:
            coord, move = point
            x, y = coord

            if s2.inside(x,y):
                # get the backup
                bcoord, bmove = backup
                x, y = bcoord
                inix, iniy, dx, dy = bmove
            else:
                inix, iniy, dx, dy = move

            px = x + inix
            py = y + iniy

            exts = s2.go_toward_center(px, py, dx, dy)
            extrem = extrem + exts

        return extrem


    def go_toward_center(self, px, py, dx, dy):
        # return point closest to center following diagonal
        # of slope (dx, dy)

        print ("starting position", px, py)
        dist_center = distance(self.x, self.y, px, py)
        print ("Current dist", dist_center)
        to_shorten = dist_center - self.radius - 1

        print ("Too much of", to_shorten)

        if to_shorten % 2 == 0:
            # precise intersection
            print ("dxy", dx, dy)
            px += dx * (to_shorten // 2)
            py += dy * (to_shorten // 2)
            dist_center = distance(self.x, self.y, px, py)
            print ("Found intersect", px, py, "at dist", dist_center)
            assert dist_center == self.radius + 1

            return [(px, py)]
        else:
            print ("ALERT: multiple options, discarding for now")
            return []


            px += dx * (to_shorten // 2)
            py += dy * (to_shorten // 2)

            # need to check if other point in vertical or horiz
            # but we have to information on other disc at this point
            # :-(
            if distance(self.x,self.y, px+dx, py) == self.radius+1:
                return [(px, py), (px+dx, py)]
            else:
                assert distance(self.x,self.y, px, py+dy) == self.radius+1
                return [(px, py), (px, py+dy)]



the_real_points = []

def check_intersection_to_all_sensors(inter):
    global xlimit, ylimit
    for x,y in inter:
        if x < 0 or y < 0 or x > xlimit or y > ylimit:
            print ("Candidate out of grid", x, y)
            return False

        candidate = True
        for s in sensors:
            d = distance(s.x, s.y, x, y)
            if d > s.radius:
                print ("Still candidate", x, y, d, s)
            else:
                print ("Too close to", s, "at dist", d)
                candidate = False
                break

        if candidate:
            print ("FOUND IT", x, y)
            print ("RESULT", 4_000_000*x + y)

            if (x,y) in items:
                print ("SADLY, already at that position:",
                       items[(x,y)])

            else:
                the_real_points.append((x,y))
                exit(0)

    return False






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

if True:
    count_inter = 3
    for s in sensors:
        for s2 in sensors:
            if s != s2:
                inter = s.intersect_edge(s2)
                print ("Insersect", s, s2, "is", inter)
                if inter:

                    b = check_intersection_to_all_sensors(inter)

                    continue

                    if count_inter > 0:
                        count_inter -= 1
                        continue

                    s2.debug_show_disc(gridline)
                    s.debug_show_disc(gridline)
                    for x,y in inter:
                        gridline.set(x,y,'*',force=True)

                    print (gridline)
                    exit (12)


print ("after the loop, still", len(the_real_points), "candidates")
print (the_real_points)



candx, candy = (2978645, 3249288)

cands = []
for dx in range (-2, 3):
    for dy in range (-2, 3):
        cands.append((candx+dx, candy+dy))

print( cands)
# exit(13)

print ("Checking around candidate")

while the_real_points:
    the_real_points.pop()

check_intersection_to_all_sensors(cands)

print ("after the look around, still", len(the_real_points), "candidates")
print (the_real_points)





#  (2978645, 3249288),

exit(42)


for s in sensors:
    if INPUTSMALL:
        s.debug_show_disc(gridline)
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

## First part
#too high 5367038
#too low  5367035
#RIGHT:   5367037 <-- but why !??! => ok was off by one somewhere



## Second part

# [(2978645, 3249288),
#  (2978645, 3249288),
#  (2978645, 3249288),
#  (2978645, 3249288)]
#
# => 9678475454760 but apparently, too low :-(
# ARGHHHH, go RTFM RTFEXERCICE actually, the answer is NOT x*y but 
# 4000000x + y
#



