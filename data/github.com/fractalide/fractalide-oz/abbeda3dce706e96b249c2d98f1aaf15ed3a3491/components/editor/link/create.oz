functor
import
   Comp at '../../../lib/component.ozf'
export
   new: CompNewGen
define
   fun {CompNewGen Name}
      {Comp.new comp(
		   name:Name type:'components/editor/link/create'
		   inPorts(input)
		   outPorts(line outComp name)
		   procedure(proc{$ Ins Out Comp} Rec in
			       Rec = {Ins.input.get}
			       {Out.line create(Rec.x Rec.y Rec.x+100.0 Rec.y+100.0 arrow:last)}
			       {Out.line lower}
				{Out.outComp outComp(comp:Rec.outComp bPoint:Rec.entryPoint)}
				{Out.name create(text:"" Rec.x Rec.y)}
			    end)
		   )
      }
   end
end