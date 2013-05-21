UNIT mouser;
{************************************************************}
{* Mouse-interfacing BIOS calls.                            *}
{*                                                          *}
{* JCA 1992-93. Derived from Microsoft manual.              *}
{************************************************************}



INTERFACE

USES DOS;

CONST
  JCA : string = 'By John C. Atkeson';

  PROCEDURE makecursor;

  PROCEDURE showmouse;

  PROCEDURE hidemouse;

  PROCEDURE getmouse(VAR mousex,mousey:INTEGER; VAR leftbutton,rightbutton:BOOLEAN);

  PROCEDURE setmouse(mousex,mousey:INTEGER);

  PROCEDURE setmousewindow(xmin,ymin,xmax,ymax:INTEGER);

  PROCEDURE getmousemove(VAR xoffset,yoffset : INTEGER);

  PROCEDURE getmousebutton(VAR thisbutton:BOOLEAN; buttonnumber:BYTE);

  PROCEDURE setmickey(mickey,pixels : INTEGER);

  PROCEDURE mousewait;

{**************************************************************}
{>>>>>>>>>>>>>>>>>>>>}  IMPLEMENTATION  {<<<<<<<<<<<<<<<<<<<<<<}
{**************************************************************}

VAR
  dummy        : integer;
  // reg       : REGISTERS;  { DOS Registers for mouse I/O. }

  PROCEDURE findmouse(VAR foundmouse : BOOLEAN;numofbuttons:INTEGER);
  {************************************************************}
  {* Is there a mouse on the system?  Let's find out.         *}
  {************************************************************}
  BEGIN
    (* dummy
    reg.ax:=00;
    INTR($33,reg);
    IF (reg.ax<>0) THEN BEGIN
      foundmouse:=TRUE;
      MOVE(reg.bx,numofbuttons,2);  { reg.bx returns number of buttons. }
    END ELSE BEGIN
      foundmouse:=FALSE;
    END;
    *)
  END;


  PROCEDURE makecursor;
  {*****************************************************************}
  {* Set a cheezy little arrow to replace the nice one built into  *}
  {* the mouse driver.                                             *}
  {*****************************************************************}
  TYPE
    cursortype  = ARRAY[0..1,0..15] OF WORD;
  CONST
    cursorpattern : cursortype=
          (($FFFF,$FFFF,$FFFF,$FFFF,
            $FFFF,$FFFF,$FFFF,$FFFF,
            $FFFF,$FFFF,$FFFF,$FFFF,
            $FFFF,$FFFF,$FFFF,$FFFF),

           ($8000,$E000,$F800,$FE00,
            $D800,$0C00,$0600,$0300,
            $0000,$0000,$0000,$0000,
            $0000,$0000,$0000,$0000));
  VAR
    hotspotx,hotspoty : INTEGER;
    cursor            : cursortype;
  BEGIN
    hotspotx:=0; hotspoty:=0;
    cursor:=cursorpattern;
    (* dummy
    reg.ax:=$09;
    MOVE(hotspotx,reg.bx,2); MOVE(hotspoty,reg.cx,2);
    reg.es:=SEG(cursor[0,0]); reg.dx:=OFS(cursor[0,0]);
    INTR($33,reg);
    *)
  END;


  PROCEDURE showmouse;
  {************************************************************}
  {* Make the mouse arrow visible.                            *}
  {************************************************************}
  BEGIN  (*  dummy reg.ax:=$01; INTR($33,reg); *) END;


  PROCEDURE hidemouse;
  {************************************************************}
  {* Hide the mouse arrow.                                    *}
  {************************************************************}
  BEGIN  (*  dummy reg.ax:=$02; INTR($33,reg); *) END;


  PROCEDURE getmouse(VAR mousex,mousey:INTEGER; VAR leftbutton,rightbutton:BOOLEAN);
  {************************************************************}
  {* Get the mouse's current locaton and button status.       *}
  {* Most useful of all of these routines.                    *}
  {************************************************************}
  VAR buttons : WORD;
  BEGIN
    (*  dummy
    reg.ax:=$03;  reg.bx:=$00000000; INTR($33,reg); MOVE(reg.bx,buttons,2);
    leftbutton:=(buttons AND $01)>0; rightbutton:=(buttons AND $02)>0;
    MOVE(reg.cx,mousex,2); MOVE(reg.dx,mousey,2);
    *)
  END;


  PROCEDURE setmouse(mousex,mousey:INTEGER);
  {************************************************************}
  {*  Forces mouse to location on screen.                     *}
  {************************************************************}
  BEGIN
    (*  dummy  reg.ax:=$04; MOVE(mousex,reg.cx,2); MOVE(mousey,reg.dx,2); INTR($33,reg); *)
  END;


  PROCEDURE setmousewindow(xmin,ymin,xmax,ymax:INTEGER);
  {************************************************************}
  {*  Sets mouse x and y limits.                              *}
  {************************************************************}
  BEGIN
    (*  dummy
    reg.ax:=$07; MOVE(xmin,reg.cx,2); MOVE(xmax,reg.dx,2); INTR($33,reg);
    reg.ax:=$08; MOVE(ymin,reg.cx,2); MOVE(ymax,reg.dx,2); INTR($33,reg);
    *)
  END;


  PROCEDURE getmousemove(VAR xoffset,yoffset : INTEGER);
  {************************************************************}
  {* Gets relative motion of mouse since last polling.        *}
  {************************************************************}
  BEGIN
    (*  dummy
    reg.ax:=11;
    INTR($33,reg);
    MOVE(reg.cx,xoffset,2); MOVE(reg.dx,yoffset,2);
    *)
  END;


  PROCEDURE getmousebutton(VAR thisbutton:BOOLEAN; buttonnumber:BYTE);
  {************************************************************}
  {* Just gets a button status.                               *}
  {************************************************************}
  VAR buttoncount : INTEGER;
  BEGIN
    (*  dummy
    reg.ax:=05;
    reg.bx:=buttonnumber;
    INTR($33,reg);
    thisbutton:=odd(reg.ax);
    buttoncount:=reg.bx;
    *)
  END;


  PROCEDURE setmickey(mickey,pixels : INTEGER);
  {************************************************************}
  {* Sets mickey/pixel ratio, whatever that is.               *}
  {* Maybe it's like resolution of movement.                  *}
  {************************************************************}
  BEGIN
    (*  dummy
    reg.ax:=15;
    reg.cx:=mickey div pixels;  { horizontal. }
    reg.dx:=mickey div pixels;  { vertical. }
    INTR($33,reg);
    *)
  END;

  PROCEDURE mousewait;
  {**************************************************************}
  {*  Works as a de-bouncer for the mouse buttons.              *}
  {* stalls the mouse until the user has quit pressing anything.*}
  {**************************************************************}
  VAR mousex,mousey:INTEGER; lbutton,rbutton:BOOLEAN;
  BEGIN
    mousex:=0; mousey:=0; lbutton:=FALSE; rbutton:=FALSE;
    REPEAT
      getmouse(mousex,mousey,lbutton,rbutton);
    UNTIL NOT(lbutton OR rbutton);
  END;


END.



        (($FFFF,$FFFF,$FFFF,$FFFF,
          $FFFF,$FFFF,$FFFF,$FFFF,
          $FFFF,$FFFF,$FFFF,$FFFF,
          $FFFF,$FFFF,$FFFF,$FFFF),

         ($8000,$E000,$F800,$FE00,
          $D800,$0C00,$0300,$0000,
          $0000,$0000,$0000,$0000,
          $0000,$0000,$0000,$0000));

{  cursorpattern : cursortype=
        (($FFFF,$8000),
         ($FFFF,$E000),
         ($FFFF,$F800),
         ($FFFF,$FE00),
         ($FFFF,$D800),
         ($FFFF,$0C00),
         ($FFFF,$0300),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000),
         ($FFFF,$0000)); }
