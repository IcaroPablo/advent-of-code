room  = []
orientation = "up"
steps = []

def convert(symbol):
    return 0 if symbol == '.' else 1

with open('sampled6.txt', 'r') as file:
# with open('minisample.txt', 'r') as file:
    # lines = [map(convert, line.rstrip()) for line in file]
    for line in file.readlines():
        symbols = []

        for symbol in line:
            if(symbol == "\n"):
                continue

            if(symbol == "^"):
                symbols.append(symbol)
                continue

            symbols.append(convert(symbol))

        room.append(symbols)

for line in room:
    print(line)

# def guard_in_room(_room):
#     return filter(lambda element: element == "^", _room)

def get_guard_pos(_room):
    for i in range(0, len(_room[0])):
        for j in range(0, len(_room)):
            if(_room[i][j] == "^"):
                return i, j

def get_next_pos(i, j, _orientation):
    if _orientation == "up":
        return i - 1, j
    elif _orientation == "right":
        return i, j + 1
    elif _orientation == "down":
        return i + 1, j
    else: 
        return i, j - 1

def scan_next_pos(_room, _orientation):
    i, j = get_guard_pos(_room)
    m, n = get_next_pos(i, j, _orientation)

    if(n < 0 or n > len(_room[0]) - 1):
        return 2

    if(m < 0 or m > len(_room) - 1):
        return 2

    return _room[m][n]

def rotate(_orientation):
    if orientation == "up":
        return "right"
    elif orientation == "right":
        return "down"
    elif orientation == "down":
        return "left"
    else: 
        return "up"

# while(guard_in_room(room)):
while(True):
# for i in range(0, 42):
    # print("aqui")
    # print(orientation)
    # print(get_guard_pos(room))
    # print(scan_next_pos(room, orientation))

    if(scan_next_pos(room, orientation) == 2):
        # steps += 1
        break

    if(scan_next_pos(room, orientation) == 1):
        orientation = rotate(orientation)
        continue
    
    
    i, j = get_guard_pos(room)
    m, n = get_next_pos(i, j, orientation)

    if ([m, n] not in steps):
        steps.append([m, n])

    room[i][j] = 0
    room[m][n] = "^"

print(len(steps))

