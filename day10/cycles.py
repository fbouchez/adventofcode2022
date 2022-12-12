import sys


reg = 1


def simulate(l):
    if l == "noop":
        clock()
        return
    op,num = l.split()

    assert op == "addx"

    clock()
    clock()

    update_reg(int(num))


cycle = 0

crtline = []

def clock():
    global cycle, crtline
    cycle += 1

    draw_pixel()

    if cycle % 40 == 0:
        print (''.join(crtline))
        crtline = []

    # if cycle == 20:
        # check_signal_strength()
    # elif (cycle - 20) % 40 == 0:
        # check_signal_strength()


def draw_pixel():
    global reg, crtline
    i = len(crtline)
    if abs(reg - i) <= 1:
        crtline.append('#')
    else:
        crtline.append('.')



total_strength = 0

def check_signal_strength():
    global cycle, reg, total_strength

    ss = cycle * reg
    print ("Cycle", cycle, "reg", reg, "strength", ss)

    total_strength += ss


def update_reg(n):
    global reg
    reg += n



for l in sys.stdin:
    simulate(l.strip())

print("total", total_strength)



