@* Introduction.
This is a computer game that you might be familiar with.
It has other popular names, such as KMines in KDE, and Gnomine in GNOME.
So, you get the idea.

The purpose of writing this program is to practice programming
with CURSES.
CURSES is a library for building fancy terminal-based programs.
While playing the game, the user can move the cursor around the
screen to decide where to perform actions.
In fact, the program can also be extended to support mouse actions.
%Ideally, mouse actions should also be supported.

@ Quick game guide. for those who don't want to read the code,
here is how you play the game: run in your terminal
$$\tt boom\ \it \langle rows\rangle\ \langle cols\rangle\ \langle mines\rangle$$
Then you will see a window on you screen.  Use the arrow keys
({\tt vi} users: your favorite keys work as well.\null)
to nagivate, and hit the space key to reveal a square.

The number in a revealed square is the number of mines in its surrounding
squares.  Given this information, you may determine that some cells
must be mines.  In that case, use {\tt m} key to mark the square.

The timer on the top-left corner shows the seconds elapsed since game start.
The counter on the top-right corner shows the number of remaining mines.
The meaning of other things on the screen are left for you to find out\dots


@ The header files of \CEE/ standard library are included here.

@c
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <assert.h>

@* Game state.
The game state is store in the |state_t| structure.

Each cell is represented by the pair $\it(state, value)$.
When the cell contains a mine, its value is |MINE|; otherwise its value
is the number of mines in its surrounding cells (0--8).
The state of a cell indicates whether it is revealed, marked, or hidden.

@d MINE 9
@d REVEALED 0
@d HIDDEN 1
@d MARKED 2

@c
typedef struct {
	unsigned value:4;
	unsigned state:4;
} cell_t;

typedef struct {
	unsigned char rows, cols;
	unsigned short mines, revealed, marked;
	cell_t cells[];
} state_t;

@ The program often wants to visit the 8 surrounding cells for a specific cell.
We define a generic function, |walk_around|, for this job.
@c
void walk_around(state_t *s, int z, void (*fn)(state_t *s, int))
{
	int i, j, cols, rows;

	cols = s->cols;
	rows = s->rows;
	assert(z >= 0);
	i = z / cols;
	assert(i < rows);
	j = z % cols;
	if (i > 0) {
		if (j > 0) fn(s, z - cols - 1); /* $\nwarrow$ */
		fn(s, z - cols); /* $\uparrow$ */
	}
	if (j < cols - 1) {
		if (i > 0) fn(s, z - cols + 1); /* $\nearrow$ */
		fn(s, z + 1); /* $\rightarrow$ */
	}
	if (j > 0) {
		fn(s, z - 1); /* $\leftarrow$ */
		if (i < rows - 1) fn(s, z + cols - 1); /* $\swarrow$ */
	}
	if (i < rows - 1) {
		fn(s, z + cols); /* $\downarrow$ */
		if (j < cols - 1) fn(s, z + cols + 1); /* $\searrow$ */
	}
}

@ Function |mkstate| creates a new game state given the number of rows,
columns, and mines.  The location of the mines will be randomly generated.

@c
static void inc_mine_cnt(state_t *, int);
state_t *mkstate(int rows, int cols, int mines)
{
	size_t cells;
	int i;
	state_t *s;

	assert(0 < rows && rows <= UCHAR_MAX);
	assert(0 < cols && cols <= UCHAR_MAX);
	cells = rows*cols;
	assert(cells < 1ul << 15);
	assert(cells > mines); /* Otherwise not really an interesting game */

	s = malloc(offsetof(state_t, cells) + cells * sizeof (cell_t));
	s->rows = rows;
	s->cols = cols;
	s->mines = mines;
	s->revealed = s->marked = 0;
	for (i = 0; i < cells; i++) {
		s->cells[i].state = HIDDEN;
		s->cells[i].value = 0;
	}
	@<Randomly generate mines@>;
	return s;
}

@ We call |rand| function to generate random positions to place mines.
After placing a mine, we call |walk_around| to increase the counter in
the surrounding cells.

@<Randomly generate mines@>=
while (mines-- > 0) {
	int z;

	do
		z = rand() % cells;
	while (s->cells[z].value == MINE);
	s->cells[z].value = MINE;
	walk_around(s, z, inc_mine_cnt);
}

@ @c
void inc_mine_cnt(state_t *s, int z)
{
	if (s->cells[z].value != MINE) {
		assert(s->cells[z].value < 8);
		++s->cells[z].value;
	}
}

@ When the state of a cell changes, the user interface needs to be updated.
We put a forward declaration of |cell_callback| here, which will be called
whenever the state of a cell changes.  The definition of this function
will be given in ``User interface'' section.

@c
void cell_callback(state_t *s, int z);

@ When the user decide to reveal a cell, the state of the cell changes
from |HIDDEN| to |REVEALED|.  If the cell has no surrounding mines,
its surrounding mines are recursively revealed.

@c
void reveal(state_t *s, int z)
{
	if (s->cells[z].state != HIDDEN)
		return;
	s->cells[z].state = REVEALED;
	cell_callback(s, z);
	++s->revealed;
	if (s->cells[z].value == 0)
		walk_around(s, z, reveal);
}

@ Other than revealing a cell, the user can also mark the cell.
The state of the cell changes from |HIDDEN| to |MARKED|.
Marked cells cannot be revealed unless it is unmarked.

@c
void toggle_mark(state_t *s, int z)
{
	switch (s->cells[z].state) {
	case REVEALED:
		return;
	case HIDDEN:
		s->cells[z].state = MARKED;
		++s->marked;
		break;
	case MARKED:
		s->cells[z].state = HIDDEN;
		--s->marked;
		break;
	}
	cell_callback(s, z);
}

@* User interface.
We use the CURSES library for the user interface.
The header file \.{<curse.h>} must be present, and
you may want to link against the CURSES library.
(For example, \.{-lcurses} for GCC.)

@c
#include <curses.h>

@ Four windows will be used for the user interface.
They are defined as global variables.

@s WINDOW int
@c
WINDOW *timewin, *facewin, *countwin, *fieldwin;

@ When a new game state is created, |ini_windows| is called to
initialize the windows.

The code sections |@<Draw frame around...@>| and |@<Create windows@>|
will call |wnoutrefresh| to refresh the window content, but the
screen is not redrawn until |doupdate| is called.

@c
void ini_windows(state_t *s)
{
	int rows, cols;
	int i;

	rows = s->rows;
	cols = s->cols;
	initscr();
	noecho();
	cbreak();
	if (rows > LINES - 3 || cols > COLS - 2 || COLS < 11) {
		fprintf(stderr, "Your screen is not large enough.\n");
		endwin();
		exit(EXIT_FAILURE);
	}
	@<Initialize color@>;
	@<Draw frame around field@>;
	@<Create windows@>;
	doupdate();
}

@ To make the game colorful, we initialize several color pairs that can be referred
to by the macro |COLOR_PAIR|.

@<Initialize color@>=
start_color();
init_pair(1, COLOR_RED, COLOR_BLACK);
init_pair(2, COLOR_GREEN, COLOR_BLACK);
init_pair(3, COLOR_BLUE, COLOR_BLACK);
init_pair(4, COLOR_YELLOW, COLOR_BLACK);
init_pair(5, COLOR_CYAN, COLOR_BLACK);
init_pair(6, COLOR_BLACK, COLOR_RED);

@ The first window contains the time since game start.
@<Create windows@>=
timewin = newwin(1, 3, 0, 0);
wattron(timewin, COLOR_PAIR(1));
waddstr(timewin, "000");
wnoutrefresh(timewin);

@ The second window contains an {\mc ASCII}-art face,
which can be \.{\^\_\^} (normal), \.{@@\_@@} (won), or \.{x\_x} (lose).
@<Create windows@>+=
facewin = newwin(1, 3, 0, (cols - 1) / 2);
wattron(facewin, COLOR_PAIR(4) | A_STANDOUT);
waddstr(facewin, "^_^");
wnoutrefresh(facewin);

@ The third window contains the number of mines to be marked.
@<Create windows@>+=
countwin = newwin(1, 3, 0, cols - 1);
wattron(countwin, COLOR_PAIR(1));
wprintw(countwin, "%03d", s->mines);
wnoutrefresh(countwin);

@ The last window contains the ``mine field''.
@<Create windows@>+=
fieldwin = newwin(rows, cols, 2, 1);
keypad(fieldwin, 1);
wtimeout(fieldwin, 200);
for (i = 0; i < rows; i++)
	mvwhline(fieldwin, i, 0, ACS_BULLET, cols);
wmove(fieldwin, 0, 0);
wnoutrefresh(fieldwin);

@ The mine field is framed.
The frame is drawn on |stdscr| instead of |fieldwin| so that the
coordinates in |fieldwin| map directly to the coordinates of the cells.

@<Draw frame around field@>=
mvaddch(1, 0, ACS_ULCORNER);
hline(ACS_HLINE, cols);
mvaddch(1, cols + 1, ACS_URCORNER);
for (i = 0; i < rows; i++) {
	mvaddch(i + 2, 0, ACS_VLINE);
	mvaddch(i + 2, cols + 1, ACS_VLINE);
}
mvaddch(rows + 2, 0, ACS_LLCORNER);
hline(ACS_HLINE, cols);
mvaddch(rows + 2, cols + 1, ACS_LRCORNER);
wnoutrefresh(stdscr);

@ Function |fin_windows| is called when the windows are no longer needed.
@c
void fin_windows(void)
{
	delwin(timewin);
	delwin(facewin);
	delwin(countwin);
	delwin(fieldwin);
	endwin();
}

@ As stated before, |cell_callback| is called whenever the state of a cell
changes.

For the CURSES interface, this callback updates the character at the
position indicated by |z|.
This is not necessarily the cursor position due to recursive revealing.

@c
static int cell_to_ch(cell_t);
void cell_callback(state_t *s, int z)
{
	int i, j;

	i = z / s->cols;
	j = z % s->cols;
	mvwaddch(fieldwin, i, j, cell_to_ch(s->cells[z]));
}

@  Function |cell_to_ch| converts a cell into a CURSES character.
CURSES supports several special characters (\.{ACS\_*})
that are outside {\mc ASCII}.

@c
int cell_to_ch(cell_t c)
{
	static int revealed[10] = { /* predefined styles */
		' ', /* 0: empty */
		'1' | COLOR_PAIR(3) | A_BOLD, /* 1: bold blue */
		'2' | COLOR_PAIR(2) | A_BOLD, /* 2: bold green */
		'3' | COLOR_PAIR(1) | A_BOLD, /* 3: bold red */
		'4' | COLOR_PAIR(4) | A_BOLD, /* 4: bold yellow */
		'5' | COLOR_PAIR(2), /* 5: green */
		'6' | COLOR_PAIR(5) | A_BOLD, /* 6: bold cyan */
		'7', /* 7: default */
		'8' | A_BOLD, /* 8: bold */
		'*' | COLOR_PAIR(1) | A_STANDOUT, /* $*$: standout red */
	};
	switch (c.state) {
	case HIDDEN:
		return ACS_BULLET; /* $\bullet$ */
	case MARKED:
		return '*' | COLOR_PAIR(1) | A_BOLD; /* red $*$ */
	}
	assert(c.state == REVEALED || c.value <= MINE);
	return revealed[c.value];
}

@* The main program.
The main program maintains the game state and the user interface
and response to the user input.

@d STARTED 0
@d PLAYING 1
@d FINISHED 2
@c
int main(int argc, char *argv[])
{
	time_t start_time = 0;
	int status;
	int rows = 21, cols = 78, mines = 150;
	state_t *s = NULL;

	if (argc == 4) {
		sscanf(argv[1], "%d", &rows);
		sscanf(argv[2], "%d", &cols);
		sscanf(argv[3], "%d", &mines);
	}
	srand(time(NULL));

	atexit(fin_windows);
restart:
	status = STARTED;
	s = mkstate(rows, cols, mines);
	ini_windows(s);

	@<The main loop@>;

quit:
	free(s);
	return 0;
}

@  In the main loop, we use |getch| to receive a keyboard event.
This call blocks only for a short period (due to the |wtimeout| call
when creating |fieldwin|).  If no key was pressed during that period,
it returns $-1$.  Therefore, the loop is repeated when the program is
idle, so we have the chance to update the content in |timewin|, which
may change every second. 

@<The main loop@>=
for (;;) {
	int ch;
	int x, y, z;

	@<Get keyboard event@>;
	@<Update |timewin|@>;
	switch (ch) {
	@<Cases for direction keys@>;
	@<Cases for shifted direction keys@>;
	@<Case for space key@>;
	@<Case for \.{m} key@>;
	case 'r':
		fin_windows();
		free(s);
		goto restart;
	case 'q':
		goto quit;
	}
	wnoutrefresh(fieldwin);
	doupdate();
}

@ @<Get keyboard event@>=
getyx(fieldwin, y, x);
assert(0 <= x && x < s->cols && 0 <= y && y < s->rows);
z = y * s->cols + x;
ch = wgetch(fieldwin);

@ @<Update |timewin|@>=
if (status == PLAYING) {
	time_t duration;
	
	duration = time(NULL) - start_time;
	if (duration <= 999) {
		mvwprintw(timewin, 0, 0, "%03d", duration);
		wnoutrefresh(timewin);
	}
}

@ Arrow keys can be used for nagivating on the screen.

For the convenience of \.{vi} users, \.{hjkl} keys works as well

@<Cases for direction keys@>=
case KEY_LEFT: case 'h':
	wmove(fieldwin, y, x - 1);
	break;
case KEY_RIGHT: case 'l':
	wmove(fieldwin, y, x + 1);
	break;
case KEY_UP: case 'k':
	wmove(fieldwin, y - 1, x);
	break;
case KEY_DOWN: case 'j':
	wmove(fieldwin, y + 1, x);
	break;

@ Shift + arrow keys are for ``big moves'', which move the cursor
5~positions at a time.

Again, uppercase \.{HJKL} keys works as well.

@<Cases for shifted direction keys@>=
case KEY_SLEFT: case 'H':
	wmove(fieldwin, y, x - 5);
	break;
case KEY_SRIGHT: case 'L':
	wmove(fieldwin, y, x + 5);
	break;
case KEY_SR: case 'K':
	wmove(fieldwin, y - 5, x);
	break;
case KEY_SF: case 'J':
	wmove(fieldwin, y + 5, x);
	break;

@ The space key is for the ``reveal'' action: the cell under the cursor
will be revealed.  If it turns out to be a mine, the player loses.
However, if all non-mine cells have been revealed, the player wins.

The call to |wmove| is necessary, or the cursor would displace
after revealing.

@<Case for space key@>=
case ' ':
	if (status == FINISHED)
		break;
	reveal(s, z);
	if (s->cells[z].state == REVEALED && s->cells[z].value == MINE) {
		/* revealed a mine---lost */
		mvwaddstr(facewin, 0, 0, "x_x");
		status = FINISHED;
	} else if (s->revealed == s->rows * s->cols - s->mines) {
		/* revealed all non-mines---won */
		mvwaddstr(facewin, 0, 0, "@@_@@");
		status = FINISHED;
	}
	wnoutrefresh(facewin);
post_action:
	if (status == STARTED) {
		status = PLAYING;
		time(&start_time);
	}
	wmove(fieldwin, y, x);
	break;

@ The \.{m} key is for the ``mark'' action: the hidden cell will
become ``marked''.  A marked cell is visually distiguishable from
other (hidden or revealed) cells and cannot be revealed.
Therefore, it is helpful to mark the cells that is believed to have
mines.

Once a cell is marked, the counter in |countwin| will decrease by one,
and vice-versa.  However, the counter going to zero does not necessarily
make the player win because the marks may be incorrect.
Furthermore, when the number of marks is larger than the number of mines,
|countwin| will display zero instead of a funny negative number.

@<Case for \.{m} key@>=
case 'm':
	if (status == FINISHED)
		break;
	toggle_mark(s, z);
	mvwprintw(countwin, 0, 0, "%03d",
		s->marked > s->mines ? 0 : s->mines - s->marked);
	wnoutrefresh(countwin);
	goto post_action;

@* Index.
