       class-id COBOLCalc.App is partial
                 inherits type System.Windows.Application.

       working-storage section.
       
      *>> <summary>
      *>> This OnExit method is to prevent Visual Studio from raising a 
      *>> spurious MDA signalling a RaceOnRCW condition when 64-bit WPF 
      *>> applications exit. Please see the release notes for more information. 
      *>> </summary>
      *>> <param name="e">Event arguments.</param>        
       method-id OnExit protected override.
       
       procedure division using by value e as type ExitEventArgs.
           invoke super::OnExit(e) 
      *>   Insert any other closedown code before the stop run statement. 
       
           stop run. 
       
       end method.       
       
       end class.
