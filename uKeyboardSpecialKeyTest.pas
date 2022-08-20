unit uKeyboardSpecialKeyTest;
{
  **********
  * Test d'appui des touches spéciales du clavier
  **********

  Liste des modifications :
  04/04/2017, Patrick Prémartin (Olf Software) : version Windows/Mac du programme (utilisable en VCL et Firemonkey)
}

// (c) Patrick Prémartin / Olf Software 07/2017

interface

type
  tKeyboardSpecialKeyTestPosition = (Left, Right, All, Any);

  tKeyboardSpecialKeyTest = class
    class function isAltDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isCtrlDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isShiftDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isWindowsDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
    class function isCommandDown(Position
      : tKeyboardSpecialKeyTestPosition = Any): Boolean;
  end;

implementation

{$IF Defined(MSWINDOWS)}

uses windows;
{$ELSEIF Defined(IOS)}
{$ELSEIF Defined(MACOS)}

uses macapi.appkit;
{$ELSEIF Defined(ANDROID)}
{$ELSEIF Defined(LINUX)}
{$ENDIF}

type
  tKeyboardSpecialKeyTestKeycode = (Alt, Shift, Ctrl, windows, command);

function isKeyDown(key: tKeyboardSpecialKeyTestKeycode;
  Position: tKeyboardSpecialKeyTestPosition): Boolean;
{$IF Defined(MSWINDOWS)}
{$ELSEIF Defined(IOS)}
{$ELSEIF Defined(MACOS)}
{$ELSEIF Defined(ANDROID)}
{$ELSEIF Defined(LINUX)}
{$ENDIF}
begin
  result := false;
{$IF Defined(MSWINDOWS)}
  case Position of
    Left:
      case key of
        Alt:
          result := (getKeyState(VK_LMENU) < 0);
        Shift:
          result := (getKeyState(VK_LSHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_LCONTROL) < 0);
        windows:
          result := (getKeyState(VK_LWIN) < 0);
      end;
    Right:
      case key of
        Alt:
          result := (getKeyState(VK_RMENU) < 0);
        Shift:
          result := (getKeyState(VK_RSHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_RCONTROL) < 0);
        windows:
          result := (getKeyState(VK_RWIN) < 0);
      end;
    Any:
      case key of
        Alt:
          result := (getKeyState(VK_MENU) < 0);
        Shift:
          result := (getKeyState(VK_SHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_CONTROL) < 0);
        windows:
          result := (getKeyState(VK_LWIN) < 0) or (getKeyState(VK_RWIN) < 0);
      end;
    All:
      case key of
        Alt:
          result := (getKeyState(VK_LMENU) < 0) and (getKeyState(VK_RMENU) < 0);
        Shift:
          result := (getKeyState(VK_LSHIFT) < 0) and
            (getKeyState(VK_RSHIFT) < 0);
        Ctrl:
          result := (getKeyState(VK_LCONTROL) < 0) and
            (getKeyState(VK_RCONTROL) < 0);
        windows:
          result := (getKeyState(VK_LWIN) < 0) and (getKeyState(VK_RWIN) < 0);
      end;
  end;
{$ELSEIF Defined(IOS)}
  // non géré
{$ELSEIF Defined(MACOS)}
  // non géré
//  Event := TNSEvent.Create;
//  if (Event.modifierFlags <> 0) then
//    result := true
//  else
//    result := false;
{$ELSEIF Defined(ANDROID)}
  // non géré
{$ELSEIF Defined(LINUX)}
  // non géré
{$ENDIF}
end;

{ tKeyboardSpecialKeyTest }

class function tKeyboardSpecialKeyTest.isAltDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.Alt, Position);
end;

class function tKeyboardSpecialKeyTest.isCommandDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
{$IF Defined(MSWINDOWS)}
  result := isWindowsDown(Position);
{$ELSEIF Defined(IOS)}
{$ELSEIF Defined(MACOS)}
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.command, Position);
{$ELSEIF Defined(ANDROID)}
{$ELSEIF Defined(LINUX)}
{$ENDIF}
end;

class function tKeyboardSpecialKeyTest.isCtrlDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.Ctrl, Position);
end;

class function tKeyboardSpecialKeyTest.isShiftDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.Shift, Position);
end;

class function tKeyboardSpecialKeyTest.isWindowsDown
  (Position: tKeyboardSpecialKeyTestPosition): Boolean;
begin
{$IF Defined(MSWINDOWS)}
  result := isKeyDown(tKeyboardSpecialKeyTestKeycode.windows, Position);
{$ELSEIF Defined(IOS)}
{$ELSEIF Defined(MACOS)}
  result := isCommandDown(Position);
{$ELSEIF Defined(ANDROID)}
{$ELSEIF Defined(LINUX)}
{$ENDIF}
end;

end.
