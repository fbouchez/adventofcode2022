import sys
import re


node_re = re.compile("^Valve (..) has flow rate=(\d+); tunnels* leads* to valves* (.*)$")


nodes = {}

def stredge (e):
    (x, d) = e
    return x + "(" + str(d) + ")"


class Node:
    def __init__(self, label, flow_rate, neighbs):
        self.id = label
        self.flow_rate = flow_rate
        self.neighbs = list(map(lambda l: (l, 1), neighbs))

        nodes[self.id] = self
        self.valve_open = False

        self.last_visit_flow = None


    def __str__(self):
        return (
            "Node(" + self.id + 
            " flow " + str(self.flow_rate) +
            " neighb: " + ', '.join(list(map(stredge, self.neighbs))) +
            ")"
        )



def pretty_dot():
    with open("graph.dot","w") as f:
        print("digraph {", file=f)
        for n in nodes:
            node = nodes[n]
            if node.flow_rate > 0:
                print (f'\t{n} [label="{n} - {node.flow_rate}" fillcolor=lightblue, style=filled]', file=f)
            for (x,d) in node.neighbs:
                print (f"\t{n} -> {x} [label={d}]", file=f)
        print("}", file=f)


max_pressure = 0
max_pressure_path = []

def update_max_pressure(pr, path):
    global max_pressure
    # print ("Checking", pr, "with", path)
    if pr > max_pressure:
        max_pressure = pr
        max_pressure_path = list(path)


def explore(n, pressure_left, current_flow, time_left, path):
    if len(path) < 5:
        print ("Exploring path", path)
    node = nodes[n]

    if time_left <= 0:
        update_max_pressure(pressure_left, path)
        return

    # check if current_flow has change since last visit
    # if not => useless to visit this node:
    if node.last_visit_flow is not None:
        if node.last_visit_flow == current_flow:
            return

    save_last_visit_flow = node.last_visit_flow
    node.last_visit_flow = current_flow


    # open the valve or not

    # node.visited = True

    # suppose valve not opened
    for x,d in node.neighbs:
        next_pressure = pressure_left + current_flow * min(d, time_left)
        path.append(x)
        explore(x, next_pressure, current_flow, time_left - d, path)
        path.pop()

    if node.flow_rate == 0:
        # we can also do nothing and stay there for the rest of the 
        # time
        path.append("stay")
        update_max_pressure(pressure_left + time_left*current_flow, path)
        path.pop()

        # restore last_visit_flow
        node.last_visit_flow = save_last_visit_flow
        return

    # now explore again but we open the valve, unless already open
    if node.valve_open:
        node.last_visit_flow = save_last_visit_flow
        return

    node.valve_open = True

    pressure_left += current_flow  # during the minute to open the valve
    time_left -= 1
    current_flow += node.flow_rate

    # Re-update last_visit_flow with flow update
    node.last_visit_flow = current_flow

    path.append("open " + n)

    for x,d in node.neighbs:
        next_pressure = pressure_left + current_flow * min(d, time_left)
        path.append(x)
        explore(x, next_pressure, current_flow, time_left - d, path)
        path.pop()

    # restore node state for subsequent searches
    path.pop()
    node.valve_open = False

    node.last_visit_flow = save_last_visit_flow





def compress_node(n):
    print ("Compressing", n)
    node = nodes[n]

    to_update = []
    for (x,dx) in node.neighbs:
        print ("\tneighbour", x, dx)
        nodex = nodes[x]

        if len(nodex.neighbs) == 2 and nodex.flow_rate == 0:
            # on compresse

            for (y, dy) in nodex.neighbs:
                if y == n:
                    continue
                s = y
                ds = dy

            print ("successeur:", s, dx+dy)

            to_update.append((x, dx, s, ds))

    # do the update
    for (x, dx, y, dy) in to_update:
        node.neighbs.remove((x, dx))
        node.neighbs.append((y, dx+dy))

        print (nodes[x])

        nodey = nodes[y]
        print ("Removing", (x,dy), "from", nodey.neighbs, y, nodey)
        nodey.neighbs.remove((x, dy))
        nodey.neighbs.append((n, dx+dy))

        del nodes[x]


    if to_update:
        return True
    else:
        return False











def compress_graph():
    print ("compressing graph")
    again = False

    for n in nodes:
        if compress_node(n):
            again = True
            # need to get out since 'nodes' has changed
            break

    if again:
        compress_graph()






for l in sys.stdin:
    m = node_re.match(l)
    assert m is not None
    g = m.groups()
    node = Node(g[0], int(g[1]), g[2].split(', '))


compress_graph()

pretty_dot()

explore("AA", 0, 0, 30, ["AA"])


print ("Max pressure path:", max_pressure_path)
print ("Max pressure value:", max_pressure)
