grid_build(N,[],N).
grid_build(1,M,_):-
	length(M,1).
grid_build(N,[H|T],N1):-
	N1\=N,
	length(H,N),
	N2 is N1+1,
	grid_build(N,T,N2).
grid_build(N,M):-
	grid_build(N,M,0).

grid_gen(N,M):-
	grid_build(N,M),
	num_gen(1,N,L),
	getTransposedAccepted(M,O,L),
	M=O,
	fill(N,O).
	
num_gen(X,X,[X]).
num_gen(X,Y,L):-
		Y>X,
		Y1 is Y-1,
		num_gen(X,Y1,L1),
		append(L1,[Y],L).

check_num_grid(G):-
	length(G,X),
	check_num(G,[],X,S),
	max_list(S,Z),
	num_gen(1,Z,R),
	sort(S,A),
	R=A.

acceptable_distribution(G):-
	trans(G,GT),
	acceptable_distribution(G,GT).
acceptable_distribution(G,GT):-
	\+someRowEqual(G,GT).

row_col_match(M):-
	acceptable_distribution(M),
	trans(M,T),
	eachIsMember(M,T).

acceptable_permutation(I,L):-
	perm(I,L),
	checkL(I,L).

trans([], []).
trans([F|K], T) :-
	trans(F, [F|K], T).
trans([], _, []).
trans([_|Rs], M, [T|Ts]) :-
	helper(M, T, Ms),
	trans(Rs, Ms, Ts).

distinct_rows([_]).
distinct_rows([H|T]):-
	\+ member(H,T),
	distinct_rows(T).
		
distinct_columns(M):-
	trans(M,M1),
	distinct_rows(M1).
	
helsinki(N,M):-
	grid_gen(N,M),
	test_grid(M).			

			
			
%	HELPERS			

check_num([],N,_,N).
check_num([H|T],N,A,S):-
			insertIn(H,N,A,X),
			check_num(T,X,A,S).
insertIn([],N,_,N).
insertIn([H|T],N,A,X):-
			A>=H,
			\+member(H,N),
			insertIn(T,[H|N],A,X).
insertIn([H|T],N,A,X):-
			A>=H,
			member(H,N),
			insertIn(T,N,A,X).
				
someRowEqual([H1|_],[H1|_]).
someRowEqual([H1|T1],[H2|T2]):-
		H1\=H2,
		someRowEqual(T1,T2).
	
eachIsMember([],_).
eachIsMember([H|T],TR):-
	member(H,TR),
	eachIsMember(T,TR).
	
test_grid(M):-
	distinct_rows(M),
	distinct_columns(M),
	check_num_grid(M),
	row_col_match(M).	
			
getTransposedAccepted(G,NG,R):- %R list of desired order
	trans(G,NG2),
	acceptable_permutation(R,L),
	swap(NG2,L,NG).

swap(_,[],[]).
swap(G,[H|T],[K|Ks]):-
	getI(G,H,K),
	swap(G,T,Ks).

getI(G,H,K):-
	getI(G,H,K,1).
getI([K|_],H,K,AC):-
	H==AC.
getI([X|Y],H,K,AC):-
	H\=X,
	AC1 is AC+1,
	getI(Y,H,K,AC1).

checkL([],[]).
checkL([H|Hs],[T|Ts]):-
	H\=T,
	checkL(Hs,Ts).			

fill(_,[]).
fill(N,[H|T]):-
	fill_list(N,H),
	fill(N,T),
	\+member(H,T).

fill_list(_,[]).
fill_list(N,[H|T]):-
	num_gen(1,N,L),
	member(H,L),
	fill_list(N,T).

helper([], [], []).
helper([[F|O]|Ta], [F|K], [O|Os]) :-
	helper(Ta, K, Os).
		
perm([H|T],L) :- 
	perm(T,P),
	insert(H,P,L).
perm([],[]).
%either insert first
insert(X,L,[X|L]).
%or insert later in the tail
insert(X,[H|T],[H|T1]) :-
	insert(X,T,T1).
%evaluation
generate(N,G):-
	grid_build(N,G),
	helper_gen(N,G).

fill_ith_with_one(I,I,[1|T]).
fill_ith_with_one(I,C,[H|T]):-
	C<I,
	C1 is C+1,
	fill_ith_with_one(I,C1,T).
helper_gen(N,G):-
	helper_gen(1,N,G).
helper_gen(_,_,[]).
helper_gen(I,N,[H|T]):-
	I=<N,
	I1 is I+1,
	fill_ith_with_one(I,1,H),
	helper_gen(I1,N,T).