import sys

inclus = 0
overlaps = 0

def lint(x):
    return list(map(int,x))

for l in sys.stdin:
    prem,deux = l.split(",")

    a = lint(prem.split("-"))
    b = lint(deux.split("-"))

    (x,y),(s,t) = sorted([a,b])

    if x == s:
        # on intervertit pour être sûrs que 
        # t plus grand que y
        (x,y),(s,t) = (s,t),(x,y)

    if y >= t:
        print ("inclusion de", (x,y), "et", (s,t))
        inclus += 1
        continue


    if s <= y:
        print ("overlap de", (x,y), "et", (s,t))
        overlaps += 1


print ("Nombre d'inclusions:", inclus)
print ("Nombre d'overlaps:", overlaps)

print ("Total:", inclus + overlaps)

