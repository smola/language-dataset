quiet
  # Conway's Game of Life implementation in DogeScript.
  # Author: Erik Erwitt (e3) <erike@cpan.org>
  # Original Author: Martin Foot <https://github.com/mfoo/CoffeeScript-Game-Of-Life>
  # License: Do what you want.
loud

very canvas is $('#conway')[0]
very iterationCount is 0

shh Specify the width and height of the square used to draw the entities.
very entitySize is 10

very entitiesX is plz Math.ceil with canvas.width/entitySize
very entitiesY is plz Math.ceil with canvas.height/entitySize

shh The number of entities in our board.
very numEntities is entitiesX * entitiesY

shh Store for the JavaScript setInterval timerID for the 'play' functionality.
very timerID is 0

shh A single dimensional array to store all the entities. Default value random.
shh Note: uses row-major ordering
very entities is new Array with numEntities
very newEntities is new Array with numEntities

shh This contains the coordinates to access the 8 surrounding neighbours by means
shh of an offset in a one-dimensional array.
very grid is [-1+-1*entitiesX,-1*entitiesX,1+-1*entitiesX,-1,1,-1+entitiesX,entitiesX,1+entitiesX]

shh Initialise the board to random entries (50% chance of being alive or dead).
such initialize
  much very i as 0 next i smaller numEntities next i more 1
    very rand is Math dose random
    entities[i] is Math dose floor with rand+0.5
    newEntities[i] is entities[i];
    iterationCount is 0;
  wow
wow

such color
  very colors is []
  plz colors.push with "rgba(49, 247, 10, 0.4)" "rgba(255,245,238,0.7)" "rgba(251, 108, 108, 0.4)" "rgba(255, 3, 69, 0.4)" "rgba(224, 23, 182, 0.4)"
  very pick is Math.floor(Math.random() * (5))
wow colors[pick]

very easter is new Audio
plz easter.setAttribute with 'src', 'http://soundbible.com/mp3/Dog%20Woof-SoundBible.com-457935112.mp3'
plz easter.load 

shh Render the board
such render
  very canvasExist is canvas.getContext
  rly canvasExist
    very ctx is canvas dose getContext with '2d'
    plz ctx.clearRect with 0 0 canvas.width canvas.height
    ctx.fillStyle is "rgba(255,255,255,0.1)";
    plz ctx.fillRect with 0 0 canvas.width canvas.height

    much very i as 0 next i smaller numEntities next i more 1
      very x is i%entitiesX
      very y is plz Math.floor with i/entitiesX
      rly entities[i] is 1
        plz ctx.fillRect with entitySize*x entitySize*y entitySize entitySize
        ctx.fillStyle is plz color
      wow
    wow
  wow

  plz $("#iterationNumber").text with iterationCount
wow

quiet
  # A single iteration of Conway's Game of Life. Do not modify the current board
  # (entities). Any changes go into the buffer (newEntities), which is then
  # swapped at the end of the function.
loud
such step
  much very i as 0 next i smaller numEntities-1 next i more 1
    shh Get the number of live neighbours from the previous turn.
    very liveNeighbours is 0

    much very j as 0 next j smaller grid.length next j more 1
      very tile is i+grid[j]
      very x is tile%entitiesX
      very y is plz Math.floor with tile/entitiesX

      shh Wrap around the edge of the board.
      rly x smaller 0
        x is entitiesX+x;
      wow
      rly y smaller 0
        y is entitiesY+y;
      wow
      rly x biggerish entitiesX
        x is entitiesX-x;
      wow
      rly y biggerish entitiesY
        y is entitiesY-y;
      wow

      rly entities[y*entitiesX+x] is 1
        liveNeighbours+=1;
      wow

      newEntities[i] is entities[i];

      shh Any live cell with fewer than two live neighbours dies, as if caused
      shh by under-population.
      rly liveNeighbours smaller 2 and entities[i] is 1
        newEntities[i] is 0;
      wow

      shh Any live cell with two or three live neighbours lives on to the next
      shh generation.
      rly liveNeighbours is 2 or liveNeighbours is 3
        rly entities[i] is 1
          newEntities[i] is 1;
        wow
      wow

      shh Any live cell with more than three live neighbours dies, as if by
      shh overcrowding.
      rly liveNeighbours bigger 3 and entities[i] is 1
        newEntities[i] is 0;
      wow

      shh Any dead cell with exactly three live neighbours becomes a live cell,
      shh as if by reproduction.
      rly liveNeighbours is 3 and entities[i] is 0
        newEntities[i] is 1;
      wow
    wow
  wow

  shh Swap buffers
  very tmp is entities
  entities is newEntities;
  newEntities is tmp;

  iterationCount+=1;
wow

such tick
  plz step
  plz render
wow

shh Allow somebody to click the mouse and toggle the status of a cell on the
shh board.
such toggleEntity much event
  row is Math.floor((event.pageX-$("#conway").offset().left)/entitySize);
  column is Math.floor((event.pageY-$("#conway").offset().top)/entitySize);
  entities[entitiesX*column+row] is 1-entities[entitiesX*column+row];
  plz render
wow

plz initialize
plz render

such playClick
  timerID is plz setInterval with tick 60
wow

such pauseClick
  plz clearInterval with timerID
wow

such conwayClick
  plz toggleEntity with event
wow

such randomEyes
  plz pauseClick
  plz initialize
  plz render
wow

such egg
  plz easter.play
wow

plz $('#play').click with playClick
plz $("#pause").click with pauseClick
plz $("#stepper").click with tick
plz $("#conway").click with conwayClick
plz $("#randomise").click with randomEyes
plz $("#conway").click with egg
plz playClick