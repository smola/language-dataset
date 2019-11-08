/**
 * File              : Brainfuck.HC
 * Author            : Ahmed Nasser
 * Date              : 05.08.2019
 * Last Modified Date: 05.08.2019
 * Last Modified By  : Ahmed Nasser <ahmednasser@gmail.com>
 *
 *
 * __________               .__        _____              __      
 * \______   \____________  |__| _____/ ____\_ __   ____ |  | __  
 *  |    |  _/\_  __ \__  \ |  |/    \   __\  |  \_/ ___\|  |/ /  
 *  |    |   \ |  | \// __ \|  |   |  \  | |  |  /\  \___|    <   
 *  |______  / |__|  (____  /__|___|  /__| |____/  \___  >__|_ \ 
 *         \/             \/        \/                 \/     \/
 * 
 *  ___________                   .__         ________    _________
 *  \__    ___/___   _____ ______ |  |   ____ \_____  \  /   _____/
 *    |    |_/ __ \ /     \\____ \|  | _/ __ \ /   |   \ \_____  \ 
 *    |    |\  ___/|  Y Y  \  |_> >  |_\  ___//    |    \/        \
 *    |____| \___  >__|_|  /   __/|____/\___  >_______  /_______  /
 *               \/      \/|__|             \/        \/        \/ 
 *
 */

U8 Input()
{
  return GetI64("Brainfuck: enter cell value (000 to 255): ", 0, 0,
      255) (U8);
}

U8 Err()
{
  I64 i     = 0;
  I64 line  = 1;
  I64 col   = 0;
  while (i < ip)
  {
    if (code[i] == '\n')
    {
      line++;
      col = 0;
    }
    else
    {
      col++;
    }
    i++;
  }
  Print("Brainfuck: %d:%d: ERR:%s\n", line, col, msg);
}

/* Main interpenting function */
U0 BrainfuckExec(U8* code)
{
  U8 data[30000];
  MemSet(data, 0, sizeof(data));
  I64 ip      = 0;
  I64 old_ip  = 0;
  I64 dp      = 0;
  I64 depth   = 0;

select:
  switch(code[ip])
  {
    case '>': goto gt;
    case '<': goto lt;
    case '+': goto plus;
    case '-': goto minus;
    case '.': goto dot;
    case ',': goto com;
    case '[': goto lbrc;
    case ']': goto rbrc;
    case '\0':
        finish:
        return;
    default:
        ip++;
        goto select;
  }

  /* Move data pointer to next elem */
gt:
  if (dp >= sizeof(data))
  {
    Err("Attempt to set data pointer beyond upper limit", code, ip);
    goto finish;
  }
  dp++;
  ip++;
  goto select;

  /* Move data pointer to prev elem */
lt:
  if (!dp)
  {
    Err("Attempt to set data pointer below Zero", code, ip);
    goto finish;
  }
  dp--;
  ip++;
  goto select;

  /* Increment elem at data pointer */
plus:
  data[dp]++;
  ip++;
  goto select;

  /* Decrement elem at data pointer */
minus:
  data[dp]--;
  ip++;
  goto select;

  /* Print elem at data pointr */
dot:
  Print("%c", data[dp]);
  ip++;
  goto select;

  /* Go to matching right bracket when elem at data pointer is zero */
lbrc:
  if (data[dp])
  {
    ip++;
    goto select;
  }
  depth = 0;
  old_ip = ip;
  while(TRUE)
  {
    ip++;
    switch(code[ip])
    {
    case '[':
      depth++;
      break;
    case ']':
      if (depth)
      {
        depth--;
      }
      else
      {
        ip++;
        goto select;
      }
      break;
    case '\0':
      Err("Missing matching ']'", code, old_ip);
      goto finish; 
    }
  }

  /* Go back to matching right bracket when elem at data pointer is
   * non zero */
rbrc:
  if (!data[dp])
  {
    ip++;
    goto select;
  }
  depth = 0;
  old_ip = ip;
  while(TRUE)
  {
    if (!ip)
    {
      Err("Missing matching '['", code, old_ip);
      goto finish;
    }
    ip--;
    switch(code[ip])
    {
    case ']':
      depth++;
      break;
    case '[':
      if (depth)
      {
        depth--;
      }
      else
      {
        ip++;
        goto select;
      }
    }
  }
}















