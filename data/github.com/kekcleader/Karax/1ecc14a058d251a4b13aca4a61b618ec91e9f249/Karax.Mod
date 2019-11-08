(* Copyright 2018 Arthur Yefimov, Daniel Vysotsky, Patrik Alexandryan

This file is part of Karax.

Karax is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Karax is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Karax.  If not, see <http://www.gnu.org/licenses/>.
*)
MODULE Karax;
IMPORT Out, G := Graph;
CONST
  maxW = 32; maxH = 32;   (* Максимальные размеры карты *)
  cellW = 32; cellH = 32; (* Размеры одной клетки *)
  tilesPerLine = 4;
  firstUnit = 8;

  (* Типы анимации *)
  tShowUp* = 0; (* Показ единицы врага перед атакой *)
  tMove*   = 1; (* Перемещение единицы при атаке *)

TYPE
  Cell = RECORD
    type: INTEGER; (* Тип клетки *)
    team: INTEGER; (* За кого играет эта единица: 0/1 *)
    visible: BOOLEAN (* Показан ли игроку тип единицы *)
  END;

VAR
  s: G.Bitmap; (* Экран *)
  tiles: G.Bitmap; (* Изображения всех возможных клеток *)
  mouse, hand: G.Bitmap; (* Указатели мыши *)
  map: ARRAY maxH, maxW OF Cell; (* Карта *)
  W, H: INTEGER; (* Актуальные размеры карты *)
  mapX, mapY: INTEGER; (* Смещение карты на экране *)
  needFlip: BOOLEAN;
  
  drag: RECORD
    on: BOOLEAN; (* Происходит ли сейчас перетаскивание *)
    out: BOOLEAN; (* Находится ли мышь за пределами области перетаскивания *)
    unitX, unitY: INTEGER; (* За какую клетку на карте схватились *)
    innerX, innerY: INTEGER; (* За какое место клетки схватилась мышь *)
    x, y: INTEGER; (* Где находится перетаскиваемое на экране *)
    x1, y1, x2, y2: INTEGER (* Разрешённая область перетаскивания *)
  END;
  
  anim: RECORD
    on: BOOLEAN; (* Происходит ли сейчас анимация *)
    type: INTEGER; (* Тип анимации. См. константы выше *)
    t: INTEGER; (* Отсчёт времени *)
    x1, y1, x2, y2: INTEGER (* Начало и конец движения *)
  END;

PROCEDURE GenerateMap;
VAR x, y: INTEGER;
BEGIN
  W := 18; H := 12;
  (* Основа *)
  FOR y := 0 TO H - 1 DO
    FOR x := 0 TO W - 1 DO
      map[y, x].type := 1
    END
  END;
  (* Вода *)
  FOR y := H DIV 2 - 1 TO H DIV 2 DO
    FOR x := 0 TO W - 1 DO map[y, x].type := 2 END;
    FOR x := 2 TO W - 3 BY 2 DO map[y, x].type := 1 END
  END;
  (* Единицы *)
  FOR x := 0 TO W - 1 DO
    FOR y := 0 TO 2 DO
      map[y, x].type := G.Random(2) + 8;
      map[y, x].team := 1;
      map[y, x].visible := FALSE
    END;
    FOR y := H - 3 TO H - 1 DO
      map[y, x].type := G.Random(2) + 8;
      map[y, x].team := 0;
      map[y, x].visible := TRUE
    END
  END
END GenerateMap;

PROCEDURE Init;
BEGIN
  GenerateMap;
  mapX := (s.w - cellW * W) DIV 2;
  mapY := (s.h - cellH * H) DIV 2
END Init;

(* Рисует клетку карты (x; y) на экране в точке (dx, dy)
   со значением альфа-канала, равным alpha *)
PROCEDURE DrawCell(x, y, dx, dy, alpha: INTEGER);
VAR
  t: INTEGER; (* Тип клетки *)
  sx, sy: INTEGER; (* Смещение рисунка клетки в tiles *)
  clr: INTEGER;
BEGIN
  t := map[y, x].type;
  IF (t >= firstUnit) & ~map[y, x].visible THEN t := 0 END;
  sx := t MOD tilesPerLine * cellW;
  sy := t DIV tilesPerLine * cellH;
  IF t >= firstUnit THEN
    (* Подложка клетки (только для единиц) *)
    G.Blit(tiles, s, 0, 0, cellW, cellH, dx, dy)
  END;
  (* Изображение единицы *)
  G.SetAlpha(tiles, alpha);
  G.Blit(tiles, s, sx, sy, cellW, cellH, dx, dy);
  (* Цвет команды *)
  IF t >= firstUnit THEN
    IF map[y, x].team = 0 THEN clr := G.MakeCol(230, 0, 0)
    ELSE clr := G.MakeCol(0, 128, 255)
    END;
    G.RectFill(s, dx + 1, dy + 1, dx + 3, dy + 3, clr)
  END
END DrawCell;

PROCEDURE DrawMap;
VAR x, y, dx, dy: INTEGER;
BEGIN
  FOR y := 0 TO H - 1 DO
    FOR x := 0 TO W - 1 DO
      dx := mapX + x * cellW;
      dy := mapY + y * cellH;
      DrawCell(x, y, dx, dy, 255)
    END
  END
END DrawMap;

PROCEDURE DrawDrag;
VAR dx, dy: INTEGER;
BEGIN
  G.Rect(s, (drag.x - mapX) DIV cellW * cellW + mapX,
    (drag.y - mapY) DIV cellH * cellH + mapY,
    (drag.x - mapX) DIV cellW * cellW + mapX + cellW - 1,
    (drag.y - mapY) DIV cellH * cellH + mapY + cellH - 1,
    G.MakeCol(255, 0, 0));
  (* Ограничитель хода *)
  G.Rect(s, drag.x1, drag.y1, drag.x2 - 1, drag.y2 - 1,
    G.MakeCol(255, 0, 0))
END DrawDrag;

(* Действие в конце анимации перемещения *)
PROCEDURE AnimMoveEnd;
BEGIN
  IF anim.type = tMove THEN
    map[anim.y2, anim.x2] := map[anim.y1, anim.x1];
    map[anim.y1, anim.x1].type := 1
  END
END AnimMoveEnd;

PROCEDURE AnimShowUp;
CONST frameCount = 800;
VAR t: INTEGER;
BEGIN
  t := G.GetTicks() - anim.t;
  IF t > frameCount THEN t := frameCount END;
  DrawCell(anim.x2, anim.y2,
    mapX + anim.x2 * cellW, mapY + anim.y2 * cellH,
    t * 255 DIV frameCount);
  IF t = frameCount THEN
    anim.type := tMove;
    anim.t := G.GetTicks()
  END
END AnimShowUp;

PROCEDURE AnimMove;
CONST animFrames = 400;
VAR sx, sy, dx, dy, x, y, t: INTEGER;
BEGIN
  t := G.GetTicks() - anim.t;
  IF t > animFrames THEN t := animFrames END;
  (* Перекрытие оригинальной клетки *)
  sx := mapX + anim.x1 * cellW;
  sy := mapY + anim.y1 * cellH;
  G.Blit(tiles, s, cellW, 0, cellW, cellH, sx, sy);
  (* Перемещающаяся (плавающая) клетка *)
  dx := mapX + anim.x2 * cellW;
  dy := mapY + anim.y2 * cellH;
  x := sx + (dx - sx) * t DIV animFrames;
  y := sy + (dy - sy) * t DIV animFrames;
  DrawCell(anim.x1, anim.y1, x, y, 255);
  IF t = animFrames THEN
    anim.on := FALSE;
    G.ShowMouse(TRUE);
    AnimMoveEnd
  END
END AnimMove;

PROCEDURE DrawAnim;
BEGIN
  CASE anim.type OF
    tShowUp: AnimShowUp
  | tMove: AnimMove
  ELSE
  END
END DrawAnim;

PROCEDURE Act;
BEGIN
  G.ClearScreen;
  DrawMap;
  IF anim.on THEN DrawAnim END;
  IF drag.on THEN DrawDrag END;
  IF needFlip THEN G.Flip END
END Act;

PROCEDURE IsDraggable(x, y: INTEGER): BOOLEAN;
BEGIN
  RETURN (map[y, x].type >= firstUnit) & (map[y, x].team = 0)
END IsDraggable;

PROCEDURE IsDroppable(x, y: INTEGER): BOOLEAN;
BEGIN
  RETURN (map[y, x].type = 1) OR
    (map[y, x].type >= firstUnit) & (map[y, x].team # 0)
END IsDroppable;

PROCEDURE Drop(x, y: INTEGER);
BEGIN
  anim.on := TRUE;
  IF (map[y, x].type >= firstUnit) & ~map[y, x].visible THEN
    map[y, x].visible := TRUE;
    G.ShowMouse(FALSE);
    anim.type := tShowUp
  ELSE anim.type := tMove
  END;
  anim.t := G.GetTicks();
  anim.x1 := drag.unitX;
  anim.y1 := drag.unitY;
  anim.x2 := x;
  anim.y2 := y
END Drop;

PROCEDURE WrapXY(VAR x, y: INTEGER; x1, y1, x2, y2: INTEGER);
BEGIN
  IF x < x1 THEN x := x1 ELSIF x > x2 THEN x := x2 END;
  IF y < y1 THEN y := y1 ELSIF y > y2 THEN y := y2 END
END WrapXY;

PROCEDURE DragMove;
VAR x, y: INTEGER;
BEGIN
  G.GetMousePos(x, y);
  drag.out := (x < drag.x1) OR (x > drag.x2) OR
    (y < drag.y1) OR (y > drag.y2);
  WrapXY(x, y, drag.x1 + drag.innerX, drag.y1 + drag.innerY,
    drag.x2 + drag.innerX - cellW, drag.y2 + drag.innerY - cellH);
  drag.x := x; drag.y := y
END DragMove;

PROCEDURE OnMouseDown(VAR e: G.Event);
VAR x, y, r: INTEGER;
BEGIN
  x := (e.x - mapX) DIV cellW;
  y := (e.y - mapY) DIV cellH;
  IF (x >= 0) & (x < W) & (y >= 0) & (y < H) THEN
    IF (e.button = 0) & ~anim.on & IsDraggable(x, y) THEN
      drag.on := TRUE;
      drag.unitX := x;
      drag.unitY := y;
      drag.innerX := (e.x - mapX) MOD cellW;
      drag.innerY := (e.y - mapY) MOD cellH;
      r := 4; (* Максимальный радиус перемещения *)
      drag.x1 := mapX + (x - r) * cellW;
      drag.x2 := drag.x1 + (2 * r + 1) * cellW;
      drag.y1 := mapY + (y - r) * cellH;
      drag.y2 := drag.y1 + (2 * r + 1) * cellH;
      DragMove;
      G.SetMousePointer(hand, 6, 4)
    END
  END
END OnMouseDown;

PROCEDURE OnMouseUp(VAR e: G.Event);
VAR x, y: INTEGER;
BEGIN
  IF drag.on THEN
    drag.on := FALSE;
    G.SetMousePointer(mouse, 0, 0);
    x := (drag.x - mapX) DIV cellW;
    y := (drag.y - mapY) DIV cellH;
    IF (x >= 0) & (x < W) & (y >= 0) & (y < H) &
       ((drag.unitX # x) OR (drag.unitY # y)) &
       IsDroppable(x, y) & ~drag.out THEN
      Drop(x, y)
    END
  END
END OnMouseUp;

PROCEDURE OnMouseMove(VAR e: G.Event);
BEGIN
  IF drag.on THEN DragMove END
END OnMouseMove;

PROCEDURE Run;
VAR quit: BOOLEAN;
  e: G.Event;
BEGIN
  needFlip := TRUE; quit := FALSE;
  drag.on := FALSE;
  REPEAT
    IF anim.on THEN G.WaitEvents(10) ELSE G.WaitEvents(200) END;
    WHILE G.PollEvent(e) DO
      CASE e.type OF
        G.quit: quit := TRUE
      | G.keyDown: IF e.key.code = G.kEsc THEN quit := TRUE END
      | G.mouseDown: OnMouseDown(e)
      | G.mouseUp: OnMouseUp(e)
      | G.mouseMove: OnMouseMove(e); needFlip := TRUE
      ELSE
      END
    END;
    Act
  UNTIL quit
END Run;

BEGIN
  G.Settings(640, 400, {G.fullscreen, G.spread, G.sharpPixels, G.initMouse});
  s := G.Init();
  IF s = NIL THEN Out.String('Could not init graph.'); Out.Ln
  ELSE
    tiles := G.LoadBitmap('data/graph/tiles.png');
    IF tiles = NIL THEN Out.String('Could not load tiles.'); Out.Ln
    ELSE
      mouse := G.LoadBitmap('data/graph/mouse.png');
      IF mouse # NIL THEN G.SetMousePointer(mouse, 0, 0) END;
      hand := G.LoadBitmap('data/graph/hand.png');
      Init;
      G.ClearScreen;
      Run;
      G.Close
    END
  END
END Karax.
