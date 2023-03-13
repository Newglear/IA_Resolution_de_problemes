
%*******************************************************************************

:- ['taquin.pl'].    % predicats definissant le systeme a etudier
:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche

%*******************************************************************************

main :-
	initial_state2(Ini),
	% Calcul de [F, H, G]
	heuristique2(Ini,H0),
	G0 is 0,
	F0 is H0 + G0,	
	% initialisations Pf, Pu et Q
 	empty(Pf0),
 	empty(Pu0),
 	empty(Q),
    insert([[F0,H0,G0],Ini],Pf0,Pf),
    insert([Ini,[F0,H0,G0],nil,nil],Pu0,Pu),
	% lancement de Aetoile
    aetoile(Pf,Pu,Q).



%*******************************************************************************

aetoile(nil,nil,_):- print("PAS de SOLUTION : L?ETAT FINAL N?EST PAS ATTEIGNABLE !"),!.
aetoile(Pf,Pu,Q):-
    final_state(Fin),
    suppress_min([_,Fin],Pf,_),
    suppress([Fin,_,Uf,Action],Pu,_),
    affiche_solution([_,_,Uf,Action],Q,Sol),
    writelist(Sol),
    !.
    %print(Sol).
    %Fin is F,
aetoile(Pf, Pu, Q) :-
    %print("Iteration normale "),
    suppress_min([C, U],Pf,Pf_Suiv),
    suppress([U,C,P,A],Pu,Pu_Suiv),
    expand([C, U],Childs),
    loop_successors(Childs,Pf_Suiv,Pu_Suiv,Q,Pf_Next,Pu_Next),
    %put_flat(Pu_Next),
    %put_flat(Q),
    insert([U,C,P,A],Q,Q_Next),
    %put_flat(Q_Next),
    aetoile(Pf_Next,Pu_Next,Q_Next).

expand([[_,_,Gu],Pere],Childs):-
    findall([State,[F,H,G],Pere,Action],(rule(Action,Cout,Pere,State),heuristique2(State,H), G is Gu+Cout, F is H+G),Childs).

loop_successors([],Pu,Pf,_,Pu,Pf).
loop_successors([Child|Rest],Pf,Pu,Q,Pf_Suiv,Pu_Suiv):-
    belongs(Child,Q),
    loop_successors(Rest,Pf,Pu,Q,Pf_Suiv,Pu_Suiv).
loop_successors([[State,[F,H,G],Pere,Action]|Rest],Pf,Pu,Q,Pf_Suiv,Pu_Suiv):-
    belongs([State,_,_,_],Pu),
    suppress([State,[Fi,_,_],_,_],Pu,Pu_Temp),
    suppress([[Fi,_,_],State],Pf,Pf_Temp),
    Fi > F,
    insert([State,[F,H,G],Pere,Action],Pu_Temp,Pu_Tempo),
    insert([[F,H,G],State],Pf_Temp,Pf_Tempo),
    loop_successors(Rest,Pf_Tempo,Pu_Tempo,Q,Pf_Suiv,Pu_Suiv).
loop_successors([[State,[F,_,_],_,_]|Rest],Pf,Pu,Q,Pf_Suiv,Pu_Suiv):-
    belongs([State,_,_,_],Pu),
    suppress([State,[Fi,_,_],_,_],Pu,_),
    suppress([[Fi,_,_],State],Pf,_),
    Fi =< F,
    loop_successors(Rest,Pf,Pu,Q,Pf_Suiv,Pu_Suiv).
loop_successors([[State,Costs,Pere,Action]|Rest],Pf,Pu,Q,Pf_Suiv,Pu_Suiv):-
    insert([Costs,State],Pf,Pf_Tempo),
    insert([State,Costs,Pere,Action],Pu,Pu_Tempo),
    loop_successors(Rest,Pf_Tempo,Pu_Tempo,Q,Pf_Suiv,Pu_Suiv).

affiche_solution([_,_,nil,_],_,[]).
affiche_solution([_,_,U,Action],Q,Sol):-
    suppress([U,_,Pere,Ac],Q,Q_Suiv),
    affiche_solution([_,_,Pere,Ac],Q_Suiv,Sol_Int),
    append(Sol_Int,[Action],Sol).

writelist([]).
writelist([X|R]):-
    write(X),
    nl,
    writelist(R).