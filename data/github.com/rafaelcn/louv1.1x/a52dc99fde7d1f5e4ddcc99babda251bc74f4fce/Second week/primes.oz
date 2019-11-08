% Calculates if a given N number is prime.

declare
fun {Primes N}
   local PrimesAux in
      fun {PrimesAux N A}
         if N == 1 then
            false
         else
            if N == A then
               true
            elseif N mod A == 0 then
               false
            else
               {PrimesAux N (A+1)}
            end
         end
      end
      {PrimesAux N 2}
   end
end


{Browse {Primes 1}}
{Browse {Primes 2}}
{Browse {Primes 5}}
{Browse {Primes 7}}
{Browse {Primes 27}}
{Browse {Primes 39}}
{Browse {Primes 31}}
{Browse {Primes 79}}
{Browse {Primes 971}}
{Browse {Primes 1433}}
{Browse {Primes 1153}}
{Browse {Primes 241}}
{Browse {Primes 7919}}
   