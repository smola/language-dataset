local FromListToTree FromTreeToList in
   fun {FromListToTree L}
      local Insert Aux in
         fun {Insert T NewValue}
            case T
            of leaf then btree(1: NewValue left:leaf right:leaf)
            [] btree(1:Value left:Left right:Right) then
               if     NewValue < Value then btree(1:Value left:{Insert Left NewValue} right:Right)
               elseif NewValue > Value then btree(1:Value left:Left right:{Insert Right NewValue})
               else   T end
            end
         end

         fun {Aux L T}
            case L of nil then T
            else {Aux L.2 {Insert T L.1}} end
         end

         {Aux L leaf}
      end
   end

   {Browse {FromListToTree [42 24 12]}}

   fun {FromTreeToList T}
      case T of leaf then nil
      else
         {Append
          {Append {FromTreeToList T.left} [T.1]}
          {FromTreeToList T.right}
          }
      end
   end

   {Browse {FromTreeToList btree(
    1:42
    left:btree(
            1:24
            left:btree(1:12 left:leaf right: leaf)
            right:leaf)
    right:leaf)}}
end

