@*Introduction.
This program is meant to be a practice of {\mc CURSES} programming.
It is a classic computer game where a snake keeps eating food and growing
longer.

@c
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <curses.h>

@*The game.  We store the state of the game in the |game| structure.
It contains a two-dimensional array of cells, each being either
empty (0) or non-empty (1).

We also need to record the location of the
snake body, which is stored in the |body| field.
The array |body| functions as a circular buffer: |body[tail]| is the
coordination of the snake tail while |body[front-1]| (or |body[rows*cols-1]|,
if |front==0|) is the coordination of the snake head.
The capacity of the buffer equals the number of
cells in the field.  We do not need to worry about the buffer being
empty or overflow as they will not happen.

The |dir| field can be one of 0--3 for N, E, S and W.

@c
typedef unsigned coord_t; /* point $(i,j)$ encoded as |i*cols+j| */
struct game {
	unsigned rows, cols;
	unsigned char *cells;
	coord_t *body; /* circular buffer */
	unsigned front, rear; /* buffer pointers */
	unsigned dir:2, prev_dir:2;
	coord_t food;
};

@ Forward declarations.
@s chtype int
@c
void game_dtor(struct game *);
void putch(unsigned, const chtype);

@ Constructor.

@c
struct game *game_ctor(struct game *g, unsigned rows, unsigned cols)
{
	unsigned ncells;
	coord_t head;

	assert(rows > 2 && cols > 2);
	if (g == NULL) return NULL;
	@<Initialize fields in |game|@>;
	@<Mark border as non-empty@>;
	@<Create snake body@>;
	@<Create food at head location@>;
	return g;
}

@ Destructor.
@c
void game_dtor(struct game *g)
{
	free(g->cells);
	free(g->body);
}

@ @<Initialize fields...@>=
g->rows = rows;
g->cols = cols;
ncells = rows * cols;
g->cells = calloc(ncells, sizeof *g->cells);
g->body = malloc(ncells * sizeof *g->body);
g->front = g->rear = 0;
if (g->cells == NULL || g->body == NULL)
	return NULL;

@ @<Mark border as non-empty@>=
{
	int i;

	memset(g->cells, 1, cols * sizeof *g->cells);
	for (i = 1; i < rows - 1; i++)
		g->cells[i * cols] = g->cells[i * cols + cols - 1] = 1;
	memset(g->cells + (rows - 1) * cols, 1, cols * sizeof *g->cells);
}

@ The snake will be of length~1 at start, heading east.
@<Create snake body@>=
head = g->body[g->front++] = (rows / 2) * cols + (cols / 2);
g->cells[head] = 1;
g->prev_dir = g->dir = 1;
@<Draw snake head@>;

@ At game start, we put the food near the snake head's location, so that
the snake will grow immediately after the first iteration.
@<Create food at head location@>=
g->food = head + 1;
@<Draw food@>;

@ Function |game_chdir| changes the direction of the snake to |dir|.
Note that it cannot be changed to the opposite direction, so
the oddity of the current direction and |dir| must mismatch.

@c
void game_chdir(struct game *g, unsigned dir)
{
	assert(dir < 4);
	if ((g->dir^dir)&1)
		g->dir = dir;
}

@ Function |game_advance| advances the game state to the next iteration.
It returns $$\cases{0& if game ends,\cr 1& if the snake does not grow,
\cr 2& otherwise.}$$
@c
int game_advance(struct game *g)
{
	coord_t oldh, head, tail;
	unsigned n;
	int rc;
	int dz[4];

	dz[0] = -g->cols;
	dz[1] = 1;
	dz[2] = g->cols;
	dz[3] = -1;

	n = g->rows * g->cols;
	oldh = g->body[(g->front == 0) ? n-1 : g->front-1];
	head = g->body[g->front] = oldh + dz[g->dir];
	@<Advance |g->front|@>;
	tail = g->body[g->rear];
	@<Change snake head to snake body@>;
	g->prev_dir = g->dir;
	if (head != g->food) {
		@<Erase snake tail@>;
		g->cells[tail] = 0;
		@<Advance |g->rear|@>;
		rc = 1;
	}
	@<Draw snake head@>;
	if (g->cells[head])
		return 0;
	g->cells[head] = 1;
	if (head == g->food) {
		@<Create new food@>;
		@<Draw food@>;
		rc = 2;
	}
	return rc;
}

@ @<Advance |g->front|@>=
if (++g->front == n)
	g->front = 0;

@ @<Advance |g->rear|@>=
if (++g->rear == n)
	g->rear = 0;

@ @<Create new food@>=
if (((g->front > g->rear) ? g->front - g->rear : g->front + n - g->rear)
	== (g->rows - 2) * (g->cols - 2)) /* full */
	return 0;
do g->food = rand() % n; while (g->cells[g->food]);

@*User interface.

@ @c
void putch(unsigned z, const chtype ch)
{
	mvaddch(z / COLS, z % COLS, ch);
}

@ @<Draw snake head@>=
putch(head, ' ' | COLOR_PAIR(1) | A_REVERSE);

@ @<Change snake head to snake body@>=
{
	int ch;

	switch (g->prev_dir * 4 + g->dir) {
	case 0: case 10: ch = ACS_VLINE; break;
	case 5: case 15: ch = ACS_HLINE; break;
	case 1: case 14: ch = ACS_ULCORNER; break;
	case 3: case 6: ch = ACS_URCORNER; break;
	case 9: case 12: ch = ACS_LLCORNER; break;
	case 11: case 4: ch = ACS_LRCORNER; break;
	default: ch = '?'; break;
	}
	putch(oldh, ch | COLOR_PAIR(2) | A_BOLD | A_REVERSE);
}

@ @<Erase snake tail@>=
putch(tail, ' ');

@ @<Draw food@>=
putch(g->food, '*' | COLOR_PAIR(1));

@ @c
void play(unsigned rows, unsigned cols)
{
	struct game *g;
	int ch;
	int timeout = 500;

	g = malloc(sizeof *g);
	if (game_ctor(g, rows, cols) == NULL)
		goto quit;
	box(stdscr, 0, 0);
	curs_set(0); /* hide cursor */
	for (;;) {
		timeout(timeout);
		ch = getch();
		@<Handle key event@>;
		switch (game_advance(g)) {
		case 0:
			goto die;
		case 2:
			if (timeout > 50)
				timeout -= 10;
		}
	}
die:
	timeout(-1);
	getch();
quit:
	game_dtor(g);
	free(g);
}

@ @<Handle key event@>=
switch (ch) {
case ERR: /* no key pressed */
	break;
case 'q': case 27:
	goto quit;
case 'h': case KEY_LEFT:
	game_chdir(g, 3);
	break;
case 'j': case KEY_DOWN:
	game_chdir(g, 2);
	break;
case 'k': case KEY_UP:
	game_chdir(g, 0);
	break;
case 'l': case KEY_RIGHT:
	game_chdir(g, 1);
	break;
default:
	continue;
}

@*The main program.
@c
int main(int argc, char *argv[])
{
	srand(time(NULL));
	initscr(); cbreak(); noecho();
	keypad(stdscr, TRUE);
	@<Initialize colors@>;
	@<Resize screen@>;
	play(LINES, COLS);
	endwin();
	return 0;
}

@ @<Initialize colors@>=
start_color();
init_pair(1, COLOR_RED, COLOR_YELLOW);
init_pair(2, COLOR_WHITE, COLOR_GREEN);

@ @<Resize screen@>=
if (argc >= 3) {
	LINES = atoi(argv[1]);
	COLS = atoi(argv[2]);
}
if (LINES < 5 || COLS < 5) {
	endwin();
	fprintf(stderr, "size is too small\n");
	return 1;
}
wresize(stdscr, LINES, COLS);

@*Index.
