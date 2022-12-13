import sys
from collections import deque


class Elevation:

    def __init__(self):
        grid = []
        y = 0
        for l in sys.stdin.readlines():
            l = l.strip()

            if 'S' in l:
                idx = l.index('S')
                self.start = (idx, y)
                lst = list(l)
                lst[idx] = 'a'
                l = ''.join(lst)

            if 'E' in l:
                idx = l.index('E')
                self.dest = (idx, y)
                lst = list(l)
                lst[idx] = 'z'
                l = ''.join(lst)

            grid.append(l)

            y += 1

        self.grid = grid
        self.height = len(grid)
        self.width = len(grid[0])


    def __str__(self):
        return (
            "start:" + str(self.start) +
            "\ndestination:" + str(self.dest) +
            "\n" +
            str(self.grid)
        )


    def neighbours(self, p):
        x,y = p
        elev = self.grid[y][x]

        neigh = []
        for dx,dy in [
                (-1, 0),
                (1, 0),
                (0, -1),
                (0, 1)
        ]:
            nx = x+dx
            ny = y+dy

            if (
                nx < 0 or
                ny < 0 or
                nx >= self.width or
                ny >= self.height
            ):
                continue
            nelev = self.grid[ny][nx]
            if ord(nelev) <= ord(elev) + 1:
                neigh.append((nx,ny))

        return neigh

    def neighbours2(self, p):
        x,y = p
        elev = self.grid[y][x]

        neigh = []
        for dx,dy in [
                (-1, 0),
                (1, 0),
                (0, -1),
                (0, 1)
        ]:
            nx = x+dx
            ny = y+dy

            if (
                nx < 0 or
                ny < 0 or
                nx >= self.width or
                ny >= self.height
            ):
                continue
            nelev = self.grid[ny][nx]
            if ord(nelev) >= ord(elev) - 1:
                neigh.append((nx,ny))

        return neigh




    def dijkstra(self):

        prio = [ self.start ]
        dist = 0

        visited = [
            [False] * self.width
            for _ in range(self.height)
        ]
        x,y = self.start
        visited[y][x] = True

        while True:
            print ("Distance actuelle:", dist)
            print ("Prio actuelle:", prio)
            dist += 1

            frontier = []
            for p in prio:
                neigh = self.neighbours(p)

                for nx,ny in neigh:
                    if (nx,ny) == self.dest:
                        print ("Youpi")
                        return dist

                    if visited[ny][nx]:
                        continue
                    frontier.append((nx,ny))
                    visited[ny][nx] = True

            prio = frontier


    def dijkstra2(self):

        prio = [ self.dest ]
        dist = 0

        visited = [
            [False] * self.width
            for _ in range(self.height)
        ]
        x,y = self.dest
        visited[y][x] = True

        while True:
            print ("Distance actuelle:", dist)
            print ("Prio actuelle:", prio)
            dist += 1

            frontier = []
            for p in prio:
                neigh = self.neighbours2(p)

                for nx,ny in neigh:
                    if self.grid[ny][nx] == 'a':
                        print ("Youpi")
                        return dist

                    if visited[ny][nx]:
                        continue
                    frontier.append((nx,ny))
                    visited[ny][nx] = True

            prio = frontier


elev = Elevation()
print(elev)

print ("Partie 1:", elev.dijkstra())
print ("Partie 2:", elev.dijkstra2())
