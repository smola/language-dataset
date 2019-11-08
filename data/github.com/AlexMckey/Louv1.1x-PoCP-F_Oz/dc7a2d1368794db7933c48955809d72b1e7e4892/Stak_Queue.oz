declare
class Stack
   attr sl
   meth init
      sl := nil
   end
   meth size($)
     {Length @sl}
   end
   meth isEmpty($)
      @sl==nil
   end
   meth top($)
      case @sl
      of H|_ then H
      else raise emptyStack end
      end
   end
   meth pop($)
      case @sl
      of H|T then
	 sl := T
	 H
      else
	 raise emptyStack end
      end
   end
   meth push(X)
      sl := X|@sl
   end
end
class Queue
   attr ql
   meth init
      ql := nil
   end
   meth size($)
      {Length @ql}
   end
   meth isEmpty($)
      @ql==nil
   end
   meth front($)
      case @ql
      of H|_ then H
      else raise emptyQueue end
      end
   end
   meth dequeue($)
      case @ql
      of H|T then
	 ql := T
	 H
      else
	 raise emptyQueue end
      end
   end
   meth enqueue(X)
      ql := {Reverse X|{Reverse @ql}}
   end
end

declare
N = {NewCell nil}
{Browse @N==nil}
S = {New Stack init}
{Browse {S isEmpty($)}}
{Browse {S size($)}}
try
   {Browse {S top($)}}
catch X then {Browse X} end
{S push(1)}
{Browse {S isEmpty($)}}
{Browse {S size($)}}
try
   {Browse {S top($)}}
catch X then {Browse X} end
{S push(2)}
{Browse {S isEmpty($)}}
{Browse {S size($)}}
try
   {Browse {S top($)}}
   {Browse {S pop($)}}
catch X then {Browse X} end
{Browse {S pop($)}}
{Browse {S isEmpty($)}}
{Browse {S size($)}}
try
   {Browse {S top($)}}
   {Browse {S pop($)}}
catch X then {Browse X} end