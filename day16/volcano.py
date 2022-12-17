import sys
import re
from collections import deque


node_re = re.compile(
    "^Valve (..) has flow rate=(\d+); tunnels* leads* to valves* (.*)$"
)


nodes = {}
valve_nodes = []


def stredge(e):
    (x, d) = e
    return x + "(" + str(d) + ")"


class Node:
    def __init__(self, label, flow_rate, neighbs):
        self.id = label
        self.flow_rate = flow_rate
        self.neighbs = list(map(lambda l: (l, 1), neighbs))

        self.last_visit_flow = None
        self.valve_open = False
        self.visited = False  # when doing pathfinding
        self.path_to = {}

        nodes[self.id] = self
        if flow_rate > 0:
            valve_nodes.append(self)

    def __str__(self):
        return (
            "Node("
            + self.id
            + " flow "
            + str(self.flow_rate)
            + " neighb: "
            + ", ".join(list(map(stredge, self.neighbs)))
            + " path_to:\n"
            + pathToToStr(self.path_to)
            + ")"
        )


def pathToToStr(path_to):
    l = []
    for dest in path_to:
        l.append("\t" + dest + ": " + str(path_to[dest]))
    return "\n".join(l)


def pretty_dot():
    with open("graph.dot", "w") as f:
        print("digraph {", file=f)
        for n in nodes:
            node = nodes[n]
            if node.flow_rate > 0:
                print(
                    f'\t{n} [label="{n} - {node.flow_rate}" fillcolor=lightblue, style=filled]',
                    file=f,
                )
            for (x, d) in node.neighbs:
                print(f"\t{n} -> {x} [label={d}]", file=f)
        print("}", file=f)


max_pressure = 0
max_pressure_path = []


def show(path):
    # only works for path 2
    s = []
    for who,lab in path:
        s.append(who + lab)
    return ' - '.join(s)



def update_max_pressure(pr, path):
    global max_pressure
    # print ("Checking", pr, "with", path)
    if pr > max_pressure:
        print("Found better:", pr, "with", show(path))
        max_pressure = pr
        max_pressure_path = list(path)


def explore(n, pressure_left, current_flow, time_left, path):
    if len(path) < 5:
        print("Exploring path", path)
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
    for x, d in node.neighbs:
        next_pressure = pressure_left + current_flow * min(d, time_left)
        path.append(x)
        explore(x, next_pressure, current_flow, time_left - d, path)
        path.pop()

    if node.flow_rate == 0:
        # we can also do nothing and stay there for the rest of the
        # time
        path.append("stay")
        update_max_pressure(pressure_left + time_left * current_flow, path)
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

    for x, d in node.neighbs:
        next_pressure = pressure_left + current_flow * min(d, time_left)
        path.append(x)
        explore(x, next_pressure, current_flow, time_left - d, path)
        path.pop()

    # restore node state for subsequent searches
    path.pop()
    node.valve_open = False

    node.last_visit_flow = save_last_visit_flow


def find_all_path():
    for n in nodes:
        find_path_to_all(n)


def find_path_to_all(stlabel):
    for n in nodes:
        nodes[n].visited = False

    start = nodes[stlabel]
    start.visited = True

    visit_neighb = deque([(start, 0)])

    start.path_to[stlabel] = 0

    while visit_neighb:
        (node, time) = visit_neighb.popleft()

        for (x, xd) in node.neighbs:
            nx = nodes[x]
            if nx.visited:
                continue

            nx.visited = True
            start.path_to[x] = time + xd
            visit_neighb.append((nx, time + xd))


def compress_node(n):
    print("Compressing", n)
    node = nodes[n]

    to_update = []
    for (x, dx) in node.neighbs:
        print("\tneighbour", x, dx)
        nodex = nodes[x]

        if len(nodex.neighbs) == 2 and nodex.flow_rate == 0:
            # on compresse

            for (y, dy) in nodex.neighbs:
                if y == n:
                    continue
                s = y
                ds = dy

            print("successeur:", s, dx + dy)

            to_update.append((x, dx, s, ds))

    # do the update
    for (x, dx, y, dy) in to_update:
        node.neighbs.remove((x, dx))
        node.neighbs.append((y, dx + dy))

        print(nodes[x])

        nodey = nodes[y]
        print("Removing", (x, dy), "from", nodey.neighbs, y, nodey)
        nodey.neighbs.remove((x, dy))
        nodey.neighbs.append((n, dx + dy))

        del nodes[x]

    if to_update:
        return True
    else:
        return False


def compress_graph():
    print("compressing graph")
    again = False

    for n in nodes:
        if compress_node(n):
            again = True
            # need to get out since 'nodes' has changed
            break

    if again:
        compress_graph()


## smarter exploring: test different *orderings* of valve openings


def explore_orderings():

    for node in valve_nodes:
        node.valve_open = False

    explore_ordering(nodes["AA"], 0, 0, 30, ["AA"])


def explore_ordering(cur_node, pressure, current_flow, time_left, ordering):
    print("Current:", cur_node.id, pressure, current_flow, time_left, ordering)

    num_children = 0
    for node in valve_nodes:
        n = node.id
        if node is cur_node:
            continue

        if node.valve_open:
            continue

        minutes = cur_node.path_to[n]
        # is it worth it to open it ?
        if minutes + 1 >= time_left:
            continue

        num_children += 1
        # test exploring this node before others

        ordering.append(n)
        # Go to node

        # time to go to node + open the valve
        next_pressure = pressure + (minutes + 1) * current_flow
        next_current_flow = current_flow + node.flow_rate
        node.valve_open = True

        explore_ordering(
            node, next_pressure, next_current_flow, time_left - minutes - 1, ordering
        )

        node.valve_open = False

        # restore state
        ordering.pop()

    if num_children == 0:
        # just do nothing for the time remaining
        print("Nothing worth to do from", cur_node.id, "waiting")
        next_pressure = pressure + time_left * current_flow
        ordering.append("wait")
        update_max_pressure(next_pressure, ordering)
        ordering.pop()


def explore_orderings_with_elephant():

    for node in valve_nodes:
        node.valve_open = False

    explore_ordering_elephant(
        nodes["AA"], 0, 0,
        nodes["AA"], 0, 0,
        0,
        0,
        26, # we spent 4 minutes teaching the elephant
        []
    )



def explore_ordering_elephant(
    cur_node_man, time_progress_man, man_valve,
    cur_node_eleph, time_progress_eleph, eleph_valve,
    pressure,
    current_flow,
    time_left,
    ordering
):
    # Who will chose a valve, man or elephant?
    if time_progress_man == 0:
        who = 'man'
    else:
        assert time_progress_eleph == 0
        who = 'eleph'

    ## choose a valve for current 'who'

    num_children = 0
    for node in valve_nodes:
        n = node.id
        if who == 'man':
            if node is cur_node_man:
                continue
        else:
            if node is cur_node_eleph:
                continue


        if node.valve_open:
            continue

        if who == 'man':
            minutes = cur_node_man.path_to[n]
        else:
            minutes = cur_node_eleph.path_to[n]

        # is it worth it to open it ?
        if minutes + 1 >= time_left:
            continue

        # worth it, update the next nodes
        if who == 'man':
            next_cur_node_man = node
            next_cur_node_eleph = cur_node_eleph
        else:
            next_cur_node_eleph = node
            next_cur_node_man = cur_node_man


        num_children += 1
        # test exploring this node before others

        ordering.append((who, n))
        # Go to node

        # time to go to node + open the valve
        time_busy = minutes + 1

        # find how much time advances before someone gets to be free
        if who == 'man':
            next_time_progress_man = time_busy
            next_time_progress_eleph = time_progress_eleph
            man_valve = node.flow_rate
        else:
            next_time_progress_man = time_progress_man
            next_time_progress_eleph = time_busy
            eleph_valve = node.flow_rate

        time_till_next_free = min(next_time_progress_man, next_time_progress_eleph)

        next_time_progress_man -= time_till_next_free
        next_time_progress_eleph -= time_till_next_free

        ## pressure gets released during this time
        next_pressure = pressure + time_till_next_free * current_flow

        ## get to see what will be opened next
        if next_time_progress_man == 0:
            next_current_flow = current_flow + man_valve
            next_man_valve = 0
            next_eleph_valve = eleph_valve
        else:
            next_current_flow = current_flow + eleph_valve
            next_eleph_valve = 0
            next_man_valve = man_valve

        # mark the next valve as open even if technically this is not yet true
        node.valve_open = True

        ## for the recursive call, need to be carefull
        ## when mixing time of man and elephant
        explore_ordering_elephant (
            next_cur_node_man, next_time_progress_man, next_man_valve,
            next_cur_node_eleph, next_time_progress_eleph, next_eleph_valve,
            next_pressure,
            next_current_flow,
            time_left - time_till_next_free,
            ordering
        )

        node.valve_open = False

        # restore state
        ordering.pop()

    if num_children == 0:
        # just do nothing for the time remaining
        # wait for next valve of either man or elephant
        # print("Nothing worth to do from man:", cur_node_man.id, " and eleph:", cur_node_eleph.id, "... waiting after\n\t", show(ordering))

        if time_progress_man != 0:
            assert time_progress_eleph == 0

            next_pressure = pressure + time_progress_man * current_flow
            # add flow of last valve
            current_flow += man_valve
            time_left -= time_progress_man
        else:
            next_pressure = pressure + time_progress_eleph * current_flow
            current_flow += eleph_valve
            time_left -= time_progress_eleph

        assert time_left >= 0
        next_pressure += time_left * current_flow

        ordering.append(("both", "wait"))
        update_max_pressure(next_pressure, ordering)
        ordering.pop()


for l in sys.stdin:
    m = node_re.match(l)
    assert m is not None
    g = m.groups()
    node = Node(g[0], int(g[1]), g[2].split(", "))


compress_graph()
pretty_dot()
find_all_path()
for node in nodes.values():
    print(node)


## brute exploring
# explore("AA", 0, 0, 30, ["AA"])
# print ("Max pressure path:", max_pressure_path)
# print ("Max pressure value:", max_pressure)
#

## smarter one-man exploration
if False:
    explore_orderings()
    print("Max pressure path:", max_pressure_path)
    print("Max pressure value:", max_pressure)


## for man+elephant exploration, a bit trickier, but still checking all possible orderings
else:
    explore_orderings_with_elephant()
    print("Max pressure path:", max_pressure_path)
    print("Max pressure value:", max_pressure)
