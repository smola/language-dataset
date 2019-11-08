NB. contains simulations for the language development game.
NB. adapted from "The evolution of language", Nowak, Krakauer

NB. later will be defined global objects to use.

NB. definitions of object (id);(amt);(P,speaking);(Q,listening)
smat=: ;@:(2&{)"1
lmat=: ;@:(3&{)"1
amt =: >@:((1&{)"1)

update_amt=: 1}

NB. TODO: fix errors in scale.

NB. mutual payoff function for two speakers
sl_payoff=:+/@:,@((smat@:[)*|:"2@:lmat@:])
payoff=:(0.5*(amt@:[*amt@:])*(sl_payoff+sl_payoff~))"1 1
NB. returns total amount of payoff for each individual.
payoff_system=: amt%~+/"1 @:(payoff/~)

NB. TODO: write optimization to lower computational cost by removing duplicated comparisons.
NB. idea: new field for how many adjacent objects with same id.
NB. TODO: add misinterpretation error limit.

NB. parameters of new generation
NB. amt of offspring is determined by constant proprtion dividing the maximum
NB. TODO: floor tolerance might be an issue.
NB. TODO: fix final grouping operation
NB. duplicates each element of the list corresponding to the size of the list times a constant (2).
NB. new_generation=: (<.@:(]%(*&3)@:#)@:payoff_system)#]
new_generation=: ((<@:<.@:(]%(*&3)@:#)@:payoff_system)(1})])"1

NB. analysis of particular generations
NB. population average of P matrices
gen_sz=: +/@:amt
p_avg=: (+/@:]amt*smat) % gen_sz

r_candidate =: (3 : 'y;1;(5 5$(%&100)25?100);(5 5$(%&100)25?100)')"0
