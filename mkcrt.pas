UNIT MKCRT;
{$G+}

{ By Martin Kal‚n in 1995 - totally free to copy and/or modify.      }

{ Not a single BIOS function call, only direct I/O to BIOS data area
  and direct writes to video memory.                                 }

Interface

  Var
    ScreenLines: {Lines on screen - 1}  Byte absolute $40:$84;
    ScreenCols:  {Columns on screen}    Byte absolute $40:$4a;
    DisplayMode: {Current display mode} Byte absolute $40:$49;
    DisplayPage: {Current display page} Byte absolute $40:$62;
    PageSize:    {Size of current page} Word absolute $40:$4c;
    TextSeg:     {Color or monochrome}  Word;
    CursorPos:   {Column,Row for pages} Array[0..7] of Word absolute $40:$50;
    CursorSize:  {Starting/Ending row}  Word absolute $40:$60;

  Procedure InitSeg;
  Procedure CharXY  (x,y: word; c: char);
  Procedure TextXY  (x,y: word; s: string);
  Procedure CharXYC (x,y: word; c: char;   Attr: byte);
  Procedure TextXYC (x,y: word; s: string; Attr: byte);
  Procedure GotoXY  (x,y: byte);
  Function  TextAttr:     byte;
  Function  WhereX:       byte;
  Function  WhereY:       byte;

Implementation

  Procedure InitSeg;
  Begin
    if DisplayMode in [$7..$20] then
      TextSeg := $B000
    else
      TextSeg := $B800;
  End;

  Procedure CharXY(x,y : Word;c : Char); Assembler;
  Asm
    mov ax,$40        {BIOS segment               }
    mov es,ax
    mov al,es:ScreenCols
    mov ah,2          { al = ScreenCols, ah = 2   }
    mul ah            { ax = ScreenCols*2         }
    mov bx,y          { bx = y                    }
    mul bx            { ax = ScreenCols*2*y       }
    add ax,x
    add ax,x          { ax = ScreenCols*2*y+x*2   }
    push ax           { Save on stack             }
    mov ax,es:PageSize
    xor bh,bh         { Need a word for PageSize  }
    mov bl,es:DisplayPage
    mul bx            { ax = PageSize*DisplayPage }
    pop bx            { Restore last result       }
    add ax,bx         { Finally ax = the offset   }
    mov di,ax         { Move the offset into di   }

    mov cx,TextSeg    { Transfer segment via cx...}
    mov es,cx         { ...into es                }

    mov al,c          { Put character into al     }
    stosb             { Move al to es:di          }
  End;

  Procedure CharXYC(x,y : Word;c : Char;Attr : Byte); Assembler;
  Asm
    mov ax,$40        {BIOS segment               }
    mov es,ax
    mov al,es:ScreenCols
    mov ah,2          { al = ScreenCols, ah = 2   }
    mul ah            { ax = ScreenCols*2         }
    mov bx,y          { bx = y                    }
    mul bx            { ax = ScreenCols*2*y       }
    add ax,x
    add ax,x          { ax = ScreenCols*2*y+x*2   }
    push ax           { Save on stack             }
    mov ax,es:PageSize
    xor bh,bh         { Need a word for PageSize  }
    mov bl,es:DisplayPage
    mul bx            { ax = PageSize*DisplayPage }
    pop bx            { Restore last result       }
    add ax,bx         { Finally ax = the offset   }
    mov di,ax         { Move the offset into di   }

    mov cx,TextSeg    { Transfer segment via cx...}
    mov es,cx         { ...into es                }

    mov al,c          { Put character into al     }
    mov ah,Attr       { Put attribute into ah     }
    stosw             { Move ax to es:di          }
  End;

  Procedure TextXY(x,y : Word;s : String);
  Var len : Byte;
  Begin
  len := Ord(s[0]);
    Asm
      mov ax,$40        {BIOS segment               }
      mov es,ax
      mov al,es:ScreenCols
      mov ah,2          { al = ScreenCols, ah = 2   }
      mul ah            { ax = ScreenCols*2         }
      mov bx,y          { bx = y                    }
      mul bx            { ax = ScreenCols*2*y       }
      add ax,x
      add ax,x          { ax = ScreenCols*2*y+x*2   }
      push ax           { Save on stack             }
      mov ax,es:PageSize
      xor bh,bh         { Need a word for PageSize  }
      mov bl,es:DisplayPage
      mul bx            { ax = PageSize*DisplayPage }
      pop bx            { Restore last result       }
      add ax,bx         { Finally ax = the offset   }
      mov di,ax         { Move the offset into di   }

      mov cx,TextSeg    { Transfer segment via cx...}
      mov es,cx         { ...into es                }

      xor si,si         { Index for character       }
      inc si            { First char = length       }
      xor ch,ch
      mov cl,len        { Number of chars to output }
    @loop:
      mov al,char(s+si)
      stosb             { Move al to es:di          }
      inc si            { Next char                 }
      inc di            { Preserve next attribute   }
      loop @loop
    End;
  End;

  Procedure TextXYC(x,y : Word;s : String;Attr : Byte);
  Var len : Byte;
  Begin
  len := Ord(s[0]);
    Asm
      mov ax,$40        {BIOS segment               }
      mov es,ax
      mov al,es:ScreenCols
      mov ah,2          { al = ScreenCols, ah = 2   }
      mul ah            { ax = ScreenCols*2         }
      mov bx,y          { bx = y                    }
      mul bx            { ax = ScreenCols*2*y       }
      add ax,x
      add ax,x          { ax = ScreenCols*2*y+x*2   }
      push ax           { Save on stack             }
      mov ax,es:PageSize
      xor bh,bh         { Need a word for PageSize  }
      mov bl,es:DisplayPage
      mul bx            { ax = PageSize*DisplayPage }
      pop bx            { Restore last result       }
      add ax,bx         { Finally ax = the offset   }
      mov di,ax         { Move the offset into di   }

      mov cx,TextSeg    { Transfer segment via cx...}
      mov es,cx         { ...into es                }

      xor si,si         { Index for character       }
      inc si            { First char = length       }
      xor ch,ch
      mov cl,len        { Number of chars to output }
      mov ah,attr       { Same attribute for string }
    @loop:
      mov al,char(s+si)
      stosw             { Move ax to es:di          }
      inc si            { Next char                 }
      loop @loop
    End;
  End;

  Procedure GotoXY(x,y:byte); Assembler;
  Asm
    mov ax,$40            {BIOS data segment}
    mov es,ax
    mov ax,$50            {Cursor position (Column,Row) for displaypages}
    add al,es:DisplayPage
    add al,es:DisplayPage {Point to desired display page}
    mov di,ax
    mov ah,x
    mov al,y              {New value to store}
    mov [es:di],ax        {Store the value}
  End;

  Function WhereX: byte; Assembler;
  Asm
    mov ax,$40            {BIOS data segment}
    mov es,ax
    mov ax,$50            {Cursor position (Column,Row) for displaypages}
    add al,es:DisplayPage
    add al,es:DisplayPage
    mov di,ax
    mov al,[es:di]
  End;

  Function WhereY: byte; Assembler;
  Asm
    mov ax,$40            {BIOS data segment}
    mov es,ax
    mov ax,$50            {Cursor position (Column,Row) for displaypages}
    add al,es:DisplayPage
    add al,es:DisplayPage
    mov di,ax
    mov al,[es:di+1]
  End;

  Function TextAttr:byte;
  Var x,y : word;
      res : byte;
  Begin
    x := WhereX; y := WhereY;
    Asm
      push ds           {Save data seg on the stack }
      mov ax,$40        {BIOS segment               }
      mov es,ax
      mov al,es:ScreenCols
      mov ah,2          { al = ScreenCols, ah = 2   }
      mul ah            { ax = ScreenCols*2         }
      mov bx,y          { bx = y                    }
      mul bx            { ax = ScreenCols*2*y       }
      add ax,x
      add ax,x          { ax = ScreenCols*2*y+x*2   }
      push ax           { Save on stack             }
      mov ax,es:PageSize
      xor bh,bh         { Need a word for PageSize  }
      mov bl,es:DisplayPage
      mul bx            { ax = PageSize*DisplayPage }
      pop bx            { Restore last result       }
      add ax,bx         { ax = character offset     }
      inc ax            { Now ax = attribute offset }
      mov si,ax         { Move the offset into si   }

      mov cx,TextSeg    { Transfer segment via cx...}
      mov ds,cx         { ...into ds                }

      lodsb             { Move ds:si to al          }
      mov res,al
      pop ds            { Restore data segment      }
    End;
    TextAttr := res;
  End;

End.