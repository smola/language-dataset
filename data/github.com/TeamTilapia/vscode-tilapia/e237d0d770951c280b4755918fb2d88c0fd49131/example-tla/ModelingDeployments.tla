\* By Hillel Wayne at
\*   https://www.hillelwayne.com/post/modeling-deployments/

---- MODULE ModelingDeployments ----
EXTENDS TLC, Integers
CONSTANT UPDATING

(* --algorithm deploy
variables
  servers = {"s1", "s2", "s3"},
  load_balancer = servers,
  update_flag = [s \in servers |-> FALSE],
  updated = [s \in servers |-> FALSE];

define
  SameVersion ==
    \A x, y \in load_balancer:
      updated[x] = updated[y]

  ZeroDowntime ==
    \E server \in load_balancer:
      updated[server] /= UPDATING
end define

fair process update_server \in servers
begin
  Update:
    await update_flag[self];
    updated[self] := UPDATING;
  FinishUpdate:
    updated[self] := TRUE;
end process;

fair process start_update = "start_update"
  begin
   StartUpdate:
     load_balancer := {"s1"};
     update_flag := [s \in servers |->
      IF s = "s1" THEN FALSE ELSE TRUE];
   Transition:
     await \A s \in (servers \ load_balancer):
       updated[s] = TRUE;
     load_balancer := servers \ load_balancer;
     update_flag["s1"] := TRUE;
   Finish:
     await updated["s1"] = TRUE;
     load_balancer := load_balancer \union {"s1"};
end process;

end algorithm; *)
\* BEGIN TRANSLATION
VARIABLES servers, load_balancer, update_flag, updated, pc

(* define statement *)
SameVersion ==
  \A x, y \in load_balancer:
    updated[x] = updated[y]

ZeroDowntime ==
  \E server \in load_balancer:
    updated[server] /= UPDATING


vars == << servers, load_balancer, update_flag, updated, pc >>

ProcSet == (servers) \cup {"start_update"}

Init == (* Global variables *)
        /\ servers = {"s1", "s2", "s3"}
        /\ load_balancer = servers
        /\ update_flag = [s \in servers |-> FALSE]
        /\ updated = [s \in servers |-> FALSE]
        /\ pc = [self \in ProcSet |-> CASE self \in servers -> "Update"
                                        [] self = "start_update" -> "StartUpdate"]

Update(self) == /\ pc[self] = "Update"
                /\ update_flag[self]
                /\ updated' = [updated EXCEPT ![self] = UPDATING]
                /\ pc' = [pc EXCEPT ![self] = "FinishUpdate"]
                /\ UNCHANGED << servers, load_balancer, update_flag >>

FinishUpdate(self) == /\ pc[self] = "FinishUpdate"
                      /\ updated' = [updated EXCEPT ![self] = TRUE]
                      /\ pc' = [pc EXCEPT ![self] = "Done"]
                      /\ UNCHANGED << servers, load_balancer, update_flag >>

update_server(self) == Update(self) \/ FinishUpdate(self)

StartUpdate == /\ pc["start_update"] = "StartUpdate"
               /\ load_balancer' = {"s1"}
               /\ update_flag' =               [s \in servers |->
                                 IF s = "s1" THEN FALSE ELSE TRUE]
               /\ pc' = [pc EXCEPT !["start_update"] = "Transition"]
               /\ UNCHANGED << servers, updated >>

Transition == /\ pc["start_update"] = "Transition"
              /\     \A s \in (servers \ load_balancer):
                 updated[s] = TRUE
              /\ load_balancer' = servers \ load_balancer
              /\ update_flag' = [update_flag EXCEPT !["s1"] = TRUE]
              /\ pc' = [pc EXCEPT !["start_update"] = "Finish"]
              /\ UNCHANGED << servers, updated >>

Finish == /\ pc["start_update"] = "Finish"
          /\ updated["s1"] = TRUE
          /\ load_balancer' = (load_balancer \union {"s1"})
          /\ pc' = [pc EXCEPT !["start_update"] = "Done"]
          /\ UNCHANGED << servers, update_flag, updated >>

start_update == StartUpdate \/ Transition \/ Finish

Next == start_update
           \/ (\E self \in servers: update_server(self))
           \/ (* Disjunct to prevent deadlock on termination *)
              ((\A self \in ProcSet: pc[self] = "Done") /\ UNCHANGED vars)

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in servers : WF_vars(update_server(self))
        /\ WF_vars(start_update)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION
====
