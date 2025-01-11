def get_room():
    _room = []

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

                symbols.append(0 if symbol == '.' else 1)

            _room.append(symbols)

    return _room

# Traceback (most recent call last):
#   File "/home/treebeard/aocd6p2.py", line 223, in <module>
#     run(guard1, False)
#   File "/home/treebeard/aocd6p2.py", line 204, in run
#     sub_looped = run(guard2, True)
#                  ^^^^^^^^^^^^^^^^^
#   File "/home/treebeard/aocd6p2.py", line 161, in run
#     if(scan_next_pos(guard.room, guard.orientation) == 2):
#        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#   File "/home/treebeard/aocd6p2.py", line 73, in scan_next_pos
#     i, j = get_guard_pos(_room)
#     ^^^^
# TypeError: cannot unpack non-iterable NoneType object

# room_from_file = get_room()

class Guard:
    def __init__(self):
        # print("   ")
        # for line in room_from_file:
        #     print(line)
        # for line in get_room():
        #     print(line)
        # self.room = room_from_file.copy()
        self.room = get_room()
        self.orientation = "up"
        self.steps = []
        self.obstacles = []

# for line in room:
#     print(line)

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

def scan_pos(_room, m, n):
    if(n < 0 or n > len(_room[0]) - 1):
        return 2

    if(m < 0 or m > len(_room) - 1):
        return 2

    return _room[m][n]

def scan_next_pos(_room, _orientation):
    i, j = get_guard_pos(_room)
    m, n = get_next_pos(i, j, _orientation)

    if(n < 0 or n > len(_room[0]) - 1):
        return 2

    if(m < 0 or m > len(_room) - 1):
        return 2

    return _room[m][n]

def rotate(_orientation):
    if _orientation == "up":
        return "right"
    elif _orientation == "right":
        return "down"
    elif _orientation == "down":
        return "left"
    else: 
        return "up"

def append_step(m, n, _orientation, rotated, _steps):
    for i in range(0, len(_steps)):
        if _steps[i][0] == m and _steps[i][1] == n:
            was_here = _orientation in _steps[i][2]
            # print(_steps[i][2])
            # old_orientations = list(_steps[i][2])
            _steps[i][2].append(_orientation)

            # print("pos: " + str(m) + ", " + str(n) + "  |  o1: " + old_orientation + "    |    " + "o2: " + _orientation)
            # if [m, n] == [7, 6]:
            #     print("aqui no loop")
            #     print(was_here)

            return was_here
            # return False

    _step = [m, n, [_orientation], rotated]
    _steps.append(_step)

    return False

def search_cross(m, n, _orientation, _steps, _room):
    # for step in _steps:
    #     print(step)
    
    if _orientation == "right":
        for i in range(m + 1, len(_room)):
            if _room[i][n] == 1:
                r, s = get_next_pos(m, n, _orientation)
                if scan_pos(_room, r, s) == 0:
                    # crosses.append([r, s])
                    return [r, s]
                # break
    if _orientation == "left":
        for i in range(0, m):
            if _room[i][n] == 1:
                r, s = get_next_pos(m, n, _orientation)
                if scan_pos(_room, r, s) == 0:
                    # crosses.append([r, s])
                    return [r, s]
                # break
    if _orientation == "up":
        for j in range(n + 1, len(_room[0])):
            if _room[m][j] == 1:
                r, s = get_next_pos(m, n, _orientation)
                if scan_pos(_room, r, s) == 0:
                    # crosses.append([r, s])
                    return [r, s]
                # break
    if _orientation == "down":
        for j in range(0, n):
            if _room[m][j] == 1:
                r, s = get_next_pos(m, n, _orientation)
                if scan_pos(_room, r, s) == 0:
                    # crosses.append([r, s])
                    return [r, s]
                # break

    return []

    #         print("m: " + str(m) + ", n: " + str(n))

def run(guard, is_sub):
    rotated = 0
    is_loop = False

    while(True):
        if(scan_next_pos(guard.room, guard.orientation) == 2):
            i, j = get_guard_pos(guard.room)
            past_orientation = append_step(i, j, guard.orientation, rotated,  guard.steps)
            break

        if(scan_next_pos(guard.room, guard.orientation) == 1):
            guard.orientation = rotate(guard.orientation)
            rotated = 1
            continue
        
        i, j = get_guard_pos(guard.room)

        # if [i, j] == [6, 4]:
        #     print("oi")

        m, n = get_next_pos(i, j, guard.orientation)

        past_orientation = append_step(i, j, guard.orientation, rotated,  guard.steps)

        if past_orientation:
            is_loop = True
            break

        rotated = 0

        if not is_sub:
            cross = search_cross(m, n, guard.orientation, guard.steps, guard.room)

            if cross != []:
                guard2 = Guard()
                # guard2.room = guard1.room.copy()
                # guard2.room = get_room()

                # print(cross)

                p = cross[0]
                q = cross[1]

                # print("p: " + str(p) + ", q: " + str(q))
                # print(guard2.room[p][q])

                guard2.room[p][q] = 1

                sub_looped = run(guard2, True)

                if sub_looped:
                    past_obstacles = list(filter(lambda o: o[0] == p and o[1] == q, guard.obstacles))

                    if len(past_obstacles) == 0:
                        guard.obstacles.append([p, q])

        guard.room[i][j] = 0
        guard.room[m][n] = "^"

    return is_loop

guard1 = Guard()
guard1.room = room_from_file.copy()

# for line in guard1.room:
#     print(line)

run(guard1, False)

print(len(guard1.steps))

for step in guard1.steps:
    print(step)

# for obs in obstacles:
#     print(obs)

print(len(guard1.obstacles))

