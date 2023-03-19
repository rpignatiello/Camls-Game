import curses
from curses import wrapper
import time
import json


def main(stdscr):
    i = 0
    while True:
        # Clear anything previously displayed on screen
        stdscr.clear()

        f = open('./data/state.json')
        d = json.load(f)

        # Printing Json Values and time that shows updating capability
        stdscr.addstr('Camls: ' + d['Camls'] + '\n')
        for b in d['Buildings']:
            stdscr.addstr(b['name'] + ': ' + b['quantity'] + '\n')
        stdscr.addstr('time: ' + str(i))
        i += 1

        stdscr.refresh()
        f.close
        time.sleep(0.5)


wrapper(main)
