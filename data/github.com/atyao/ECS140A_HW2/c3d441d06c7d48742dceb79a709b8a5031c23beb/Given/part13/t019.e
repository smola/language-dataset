var state
    ta tb tc td te
    na nb nc nd ne
rav

# (where X is a for 1, b for 2, etc.)
# tX is number of times to stay in state X
# nX is state to go to after state X
ta := 3    na := 4
tb := 3    nb := 1
tc := 2    nc := 5
td := 1    nd := 99
te := 1    ne := 2

state := 3 # initial state

do state = 1 -> print 100+state ta:=ta-1 if ta=0->state:=na fi
[] state = 2 -> print 200+state tb:=tb-1 if tb=0->state:=nb fi
[] state = 3 -> print 300+state tc:=tc-1 if tc=0->state:=nc fi
[] state = 4 -> print 400+state td:=td-1 if td=0->state:=nd fi
[] state = 5 -> print 500+state te:=te-1 if te=0->state:=ne fi
od
print state
