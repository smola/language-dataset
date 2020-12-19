// Script header for module 'KeyboardMovement'

#define KeyboardMovement_VERSION 104

enum KeyboardMovementMode {
	eKeyboardMovementModeNone, 
	eKeyboardMovementModeTapping, 
	eKeyboardMovementModePressing
};

struct KeyboardMovementKeymap {
  eKeyCode KeyUp;
  eKeyCode KeyDown;
  eKeyCode KeyLeft;
  eKeyCode KeyRight;
};

managed struct Vector {
  int x;
  int y;
};

struct KeyboardMovementDirection {
  Vector* Up;
  Vector* Down;
  Vector* Left;
  Vector* Right; 
};

struct KeyboardMovement {
  import static attribute KeyboardMovementMode Mode;
  import static attribute eKeyCode KeyUp;
  import static attribute eKeyCode KeyDown;
  import static attribute eKeyCode KeyLeft;
  import static attribute eKeyCode KeyRight;
};
