#!/usr/bin/python3.10

import sys

days = []

with open('Main.hs', 'r') as file:
    day = ''
    for line in file:
        match line.split():
            case [_, 'Day', n, _]:
                days.append(day)
                day = ''
            case _:
                day += line
days[0] = '\n'.join(days[0].split('\n')[5:])
for i, day in enumerate(days[1:]):
    with open(f'Day{i + 1}.hs', 'w') as file:
        file.write(f'module Day{i + 1} where')
        # print(days[0], file=file)
        file.write(day)

# print(days[int(sys.argv[1])])
