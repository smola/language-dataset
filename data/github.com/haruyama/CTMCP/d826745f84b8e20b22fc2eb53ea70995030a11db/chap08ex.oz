% 1

% (nk)! / n * k!
% (nk)^(nk+1) * e^(-nk) / (n * k^(k+1) * e^(-k)
%  n^(nk) * k^(n(k-1)) * e^(-n)

% 2

declare
C={NewCell 0}
local X in {Exchange C X X+1} end
{Browse @C}

declare
C={NewCell 0}
local X X1 in {Exchange C X X1} X1=X+1 end
{Browse @C}

declare
C={NewCell 0}
L = {NewLock}
lock L then
   C:=@C+1
end
{Browse @C}

% 3

% 4

% http://ctm-himanshu.blogspot.jp/2009/03/ch8-ex4.html

% 5

% http://ctm-himanshu.blogspot.jp/2009/03/ch8-ex5.html

%  Size 1 の有界バッファでいいのでは

% 6

% http://ctm-himanshu.blogspot.jp/2009/03/ch8-ex6.html

