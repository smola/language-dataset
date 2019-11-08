:Namespace HW
(⎕IO ⎕ML ⎕WX)←0 1 3

 HelloWorld←{'Hello world, APL rocks!'} 
 
 Getting∆Started←{
  ⎕←''
  ⎕←'WELCOME TO THE APL-SKELETON!'
  ⎕←''
  ⎕←'You will want to do most of your work through the APL Session. Here are some useful'
  ⎕←'commands to run. Try them out and get some practice. Have fun!'
  ⎕←''
  ⎕←'⍝ Edit the HW namespace, escaping out of the edit window will prompt you to save the file.'
  ⎕←'      )ed HW'
  ⎕←''
  ⎕←'⍝ Run the tests in the tests directory.'
  ⎕←'      UT.run''.\tests'''
  ⎕←''
  ⎕←'⍝ Change the function that runs when starting the workspace.'
  ⎕←'      ⎕LX←''HW.Getting∆Started⍬''' 
  ⎕←''
  ⎕←'⍝ Save the workspace after you have made changes.'
  ⎕←'      )save'
  ⎕←''
}

:EndNamespace 
