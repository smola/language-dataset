quiet
  doge toe: doge tic-tac-toe in dogescript

  Homepage:  https://github.com/alexdantas/doge-toe/
  Author:    Alexandre Dantas (alexdantas) <eu@alexdantas.net>
  License:   WTFPL

  Thanks to the following:

  - The Dogescript language:
    https://github.com/remixz/dogescript
  - Doge game of life:
    https://github.com/eerwitt/doge-game-of-life
  - Dogescript grunt plugin
    https://github.com/Bartvds/grunt-dogescript
loud

shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh
shh
shh Interface: creating the Canvas, it's Context and other things on HTML
shh
shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh

shh Will only run the code when properly loaded
very canvas is $('#screen')[0]

shh Attaching the function to be called when clicked
canvas dose addEventListener with 'click' onCanvasClick false

shh Where we'll draw all the stuff
very context is canvas dose getContext with '2d'

shh Called when the user clicks on the canvas
shh @note Don't use this function, it will call `onBoardClick()` for you.
shh @see onBoardClick
such onCanvasClick much event

    very x is event.clientX - canvas.offsetLeft
    very y is event.clientY - canvas.offsetTop

    very message is 'Clicked on (' + x + ', ' + y + ')'
    plz console.loge with message

    plz onBoardClick with x y
wow

very player_one_score is $('#player-one')[0]
very player_two_score is $('#player-two')[0]


such hiliteScore much player
    very id
    rly player is PLAYER_ONE
        id is '#player-one'
    but
        id is '#player-two'
    wow

    $(id).effect(
        "highlight",
        { color: 'green'},
        6000
    );
wow

shh Loading the images
very player_one_image is new Image
player_one_image.src is 'img/player-one.png'

very player_two_image is new Image
player_two_image.src is 'img/player-two.png'

shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh
shh
shh Misc. functions
shh
shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh

shh Returns a random number between #min and #max
such randomBetween much min max
    very number is (Math.floor(Math.random() * (max-min+1) + min))
wow number


shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh
shh
shh Here we define some global variables userd across the game
shh
shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh

shh Constants to identify which thing is on a tile
very TILE_EMPTY is 0
very TILE_O     is 1
very TILE_X     is 2

shh Dimensions to draw the board
very BOARD_OFFSET_X is 10
very BOARD_OFFSET_Y is 10

shh Dimensions to draw the tiles
very BOARD_WIDTH  is 340
very BOARD_HEIGHT is 340
very TILE_WIDTH   is 100
very TILE_HEIGHT  is 100
very TILE_SPACING is 10

shh Colors for everything
very BOARD_COLOR_BG   is '#000'
very TILE_COLOR_EMPTY is '#777'
very TILE_COLOR_X     is 'red'
very TILE_COLOR_O     is 'blue'

shh The game board
shh @note Couldn't use Dogescript syntax to create 2d array.
var board = [
    [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY],
    [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY],
    [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY]
]

shh All possible players and their points
very PLAYER_ONE is 1
very PLAYER_TWO is 2
very PLAYER_ONE_POINTS is 0
very PLAYER_TWO_POINTS is 0

shh The player that must click right now
very currentPlayer is plz randomBetween with PLAYER_ONE PLAYER_TWO

shh Flag to tell if the game is still runnin'
very gameOver is false

shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh
shh
shh Now, to the game logic
shh
shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh

shh Draws the tic-tac-toe board on the screen,
shh according to which tile type is on it.
shh
shh (goes through each tile on the global board)
such drawBoard

    shh Clearing the whole board
    context.fillStyle is BOARD_COLOR_BG
    context dose fillRect with 0 0 BOARD_WIDTH BOARD_HEIGHT

    much very counterX as 0 next counterX smaller 3 next counterX more 1
       much very counterY as 0 next counterY smaller 3 next counterY more 1

           shh Deciding it's position
           very x is BOARD_OFFSET_X + (counterX * TILE_WIDTH)  + (counterX * TILE_SPACING)
           very y is BOARD_OFFSET_Y + (counterY * TILE_HEIGHT) + (counterY * TILE_SPACING)

           very tile is board[counterX][counterY]

           shh How will we draw the tile?
           rly tile is TILE_EMPTY
               context.fillStyle is TILE_COLOR_EMPTY
               context dose fillRect with x y TILE_WIDTH TILE_HEIGHT

           but rly tile is TILE_X
               context dose drawImage with player_one_image  x y

               context.fillStyle is BOARD_COLOR_BG
               context dose fillText  with PLAYER_ONE_POINTS x+10 y+10

           but rly tile is TILE_O
               context dose drawImage with player_two_image  x y

               context.fillStyle is BOARD_COLOR_BG
               context dose fillText  with PLAYER_TWO_POINTS x+10 y+10

           wow
       wow
    wow
wow

shh Converts between pixel coordinates and tiles within a board.
shh @return The board index for #x or -1 in case of error.
shh @note   I know this is ugly as fuark...
such pixelToTileX much x

    much very counter as 0 next counter smaller 3 next counter more 1
        rly x biggerish (BOARD_OFFSET_X + counter*TILE_WIDTH + counter*TILE_SPACING) and x smallerish (BOARD_OFFSET_X + TILE_WIDTH + counter*TILE_WIDTH + counter*TILE_SPACING)
            return counter;
        wow
    wow
wow -1

shh Converts between pixel coordinates and tiles within a board.
shh @return The board index for #y or -1 in case of error.
shh @note   I know this is ugly as fuark...
such pixelToTileY much y

    much very counter as 0 next counter smaller 3 next counter more 1
        rly y biggerish (BOARD_OFFSET_Y + counter*TILE_HEIGHT + counter*TILE_SPACING) and y smallerish (BOARD_OFFSET_Y + TILE_HEIGHT + counter*TILE_HEIGHT + counter*TILE_SPACING)
            return counter;
        wow
    wow
wow -1

shh Checks if the current player won the game
shh It iterates through the board, checking por three
shh equal tiles.
such wonGame

    shh Temporary variables
    very one
    very two
    very three

    shh First, let's check the diagonals
    one   is board[0][0]
    two   is board[1][1]
    three is board[2][2]

    rly one is two and two is three and three not TILE_EMPTY
        return true;
    wow

    one   is board[0][2]
    two   is board[1][1]
    three is board[2][0]

    rly one is two and two is three and three not TILE_EMPTY
        return true;
    wow

    much very counter as 0 next counter smaller 3 next counter more 1

        shh Horizontal win
        one   is board[counter][0]
        two   is board[counter][1]
        three is board[counter][2]

        rly one is two and two is three and three not TILE_EMPTY
            return true;
        wow

        shh Vertical win
        one   is board[0][counter]
        two   is board[1][counter]
        three is board[2][counter]

        rly one is two and two is three and three not TILE_EMPTY
            return true;
        wow
    wow

wow false

shh Gets called when the user clicks on the board.
such onBoardClick much x y

    shh Do nothing if won the game and it's not reset.
    rly gameOver
        return;
    wow

    shh Board inner coordinates
    very boardX is plz pixelToTileX with x
    very boardY is plz pixelToTileY with y

    shh Interrupt if user didn't click on a valid position
    rly boardX smaller 0 or boardY smaller 0
        return;
    wow

    shh Only continue if we can place the thing
    shh (current tile not empty)
    rly board[boardX][boardY] not TILE_EMPTY
        return;
    wow

    shh Placing thing according to current player
    rly currentPlayer is PLAYER_ONE
    board[boardX][boardY] is TILE_X
    but
    board[boardX][boardY] is TILE_O
    wow

    shh Refreshing board
    plz drawBoard

    shh Checking for winner
    rly wonGame()

        shh Score++
        rly currentPlayer is PLAYER_ONE
            PLAYER_ONE_POINTS is PLAYER_ONE_POINTS + 1
            plz hiliteScore with PLAYER_ONE
        but
            PLAYER_TWO_POINTS is PLAYER_TWO_POINTS + 1
            plz hiliteScore with PLAYER_TWO
        wow

        shh Refreshing HTML with scores
        player_one_score.placeholder is PLAYER_ONE_POINTS
        player_two_score.placeholder is PLAYER_TWO_POINTS

        gameOver is true
        return;
    wow

    shh Switches the current player
    rly currentPlayer is PLAYER_ONE
        currentPlayer is PLAYER_TWO
    but
        currentPlayer is PLAYER_ONE
    wow
    plz console.loge with 'switched'
wow

shh Gets called when the window is fully loaded.
shh Has the effect of restarting the game.
such resetGame
    gameOver is false

    board = [
        [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY],
        [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY],
        [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY]
    ]
    plz drawBoard
wow


shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh
shh
shh Makes jQuery call that function when the window is fully loaded.
shh @note Couldn't figure out how to make this with Dogescript syntax
shh
shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh shh

$(function() { resetGame() });

