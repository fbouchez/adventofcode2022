packet = input()
for part in [4, 14]:
    for i in range(len(packet)-part+1):
        s = set(packet[i:i+part])
        if len(s) == part:
            print ("Résultat:", i+part)
            break
