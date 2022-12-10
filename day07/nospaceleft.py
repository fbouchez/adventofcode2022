import sys


def explore (cmdline, inputlist):
    tree = None

    while cmdline is not None:
        assert cmdline[0] == '$'
        splt = cmdline[2:].split()

        # list directory contents
        if len(splt) == 1:
            assert splt[0] == 'ls'
            cmdline, tree = list_contents(inputlist)
            print ("tree after return:", tree)
            continue

        # change directories
        assert len(splt) == 2
        assert splt[0] == 'cd'
        arg = splt[1]

        if arg == '..':
            return tree
        else:
            print ("tree before going down:",tree)
            go_down(tree, arg, inputlist)
            try:
                cmdline = next(inputlist)
            except StopIteration:
                return tree
    return tree

def list_contents(inputlist):
    subdirs = {}
    files = []
    while True:
        try:
            line = next(inputlist)
        except StopIteration:
            return None, (subdirs, files)

        if line[0] == '$':
            return line, (subdirs, files)


        typ, name = line.split()
        if typ == 'dir':
            print ("Ajout dir", name)
            subdirs[name] = None
        else:
            print ("Ajout file", name, typ)
            files.append((name, int(typ)))


def go_down(tree, dirname, inputlist):
    print  ("Going into directory", dirname)
    print ("Tree at this state", tree)
    try:
        line = next(inputlist)
    except StopIteration:
        assert False
    subtree = explore(line, inputlist)
    print  ("Return from exploring", dirname)
    dirs, _ = tree
    dirs[dirname] = subtree



allsmall = 0

best_search = 100_000_000


def countsize(tree, search=None):
    global allsmall, best_search
    dirs, files = tree

    cur = 0

    for dirname in dirs:
        cur += countsize(dirs[dirname], search)

    for (f, fsize) in files:
        cur += fsize

    if cur <= 100000:
        print ("Current is small", cur)
        allsmall += cur

    if search is not None:
        print ("searching...")
        if cur > search:
            print ("candidate:", cur)
            if cur < best_search:
                best_search = cur

    return cur




cmd = next(sys.stdin)
cmd = next(sys.stdin)
root = explore (cmd, sys.stdin)

print(root)
rootsize = countsize(root)
print ("root size", rootsize)

total  = 70_000_000
unused = 30_000_000

cur_unused = total - rootsize

missing = unused - cur_unused

print ("need to free at least", missing)
print ("Tous les plus petits:", allsmall)


smallest_enough = countsize(root, search=missing)

print ("best", best_search)
