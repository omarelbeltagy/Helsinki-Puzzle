grid_build(N,M):-
    gbhelper(N,N,M).


gbhelper(_,0,[]).
gbhelper(N,C,[H|T]):-
    C\=0,
    length(L,N),
	H = L,
	C1 is C-1,
	gbhelper(N,C1,T).

grid_gen(N,M):-
   grid_build(N,M),
   gglhelper(N,N,M).


ggshelper(_,0,[]).

ggshelper(N,C,[H|T]):-
    C\=0,
	between(1,N,H),
   C1 is C-1,
   ggshelper(N,C1,T).
   
   
 gglhelper(_,0,[]).
 gglhelper(N,C,[H|T]):-
    ggshelper(N,N,H),
	C1 is C-1,
	gglhelper(N,C1,T).

num_gen(F,F,[F]).
num_gen(F,L,[H|T]):-
    F<L,
	H = F,
	F1 is F+1,
	num_gen(F1,L,T).
	

acceptable_distribution(M):-
    trans(M , TM),
	helper_acceptable_distribution1(M,TM),!.

helper_acceptable_distribution1([H] , [H1]):-
       helper_acceptable_distribution2(H , H1).      	
helper_acceptable_distribution1([H|T] , [H1|T1]):-
     helper_acceptable_distribution2(H,H1),
	 helper_acceptable_distribution1(T , T1).

helper_acceptable_distribution2([M] , [N]):-
      M \=N.	 
helper_acceptable_distribution2([H|T] , [H1|T1]):-
      H \= H1;
	  helper_acceptable_distribution2(T,T1).

distinct_rows([_]).
 distinct_rows([H|T]):-
    helper_distinct_rows1(H , T), 
	distinct_rows(T),!.
	
	
helper_distinct_rows1(H,[H1]):-
     helper_distinct_rows2(H,H1).
helper_distinct_rows1(H , [H1|T]):-
     helper_distinct_rows2(H , H1),
     helper_distinct_rows1(H , T).


helper_distinct_rows2([H1] , [H]):-
   H1 \=H.	 
helper_distinct_rows2([H|T] , [H1|T1]):-
   H \= H1;
   helper_distinct_rows2(T,T1).
 

trans([], []).
trans([H|H1], T) :-
    trans(H, [H|H1], T).

trans([], _, []).
trans([_|B], N, [T|T1]) :-
        lists_firsts_rests(N, T, N1),
        trans(B, N1, T1).

lists_firsts_rests([], [], []).
lists_firsts_rests([[H|K]|Rem], [H|H1], [K|K1]) :-
        lists_firsts_rests(Rem, H1, K1). 
		
		
distinct_columns(M):-
   trans(M ,TM),
   distinct_rows(TM).
   


my_max([], R, R). %end
my_max([X|Xs], WK, R):- X >  WK, my_max(Xs, X, R). %WK is Carry about
my_max([X|Xs], WK, R):- X =< WK, my_max(Xs, WK, R).
my_max([X|Xs], R):- my_max(Xs, X, R).

grid_max([] , 0).
grid_max([H|T] , M):-
    my_max(H , ML),
    grid_max(T , M1),
	ML >= M1,
	M = ML.
grid_max([H|T] , M):-
    my_max(H , ML),
    grid_max(T , M1),
	ML < M1,
	M = M1.	
	
check_num_grid(G):-
    grid_max(G,M),
	length(G,N),
	M =< N,
	M1 is M-1,
    helper_check_num_grid1(G , M1).

helper_check_num_grid1(_ , 0).    
helper_check_num_grid1(G , N):-
       helper_check_num_grid2(G , N),
	   N1 is N-1,
	   helper_check_num_grid1(G,N1),!.
	   
helper_check_num_grid2([H] ,N):-  member(H,N).	   
helper_check_num_grid2([H|T] , N):-
       member(N,H);
       helper_check_num_grid2(T , N). 	   


acceptable_permutation([H|T],L):-
   perm([H|T],L),
   acceptable_helper([H|T],L).
acceptable_helper([],_).   
acceptable_helper([H|T],[H1|T1]):-
    H\=H1,
	acceptable_helper(T,T1).


length_(Length, List) :- length(List, Length).

list2matrix(List, RowSize, Matrix) :-
    length(List, L),
    HowManyRows is L div RowSize,
    length(Matrix, HowManyRows),
    maplist(length_(RowSize), Matrix),
    append(Matrix, List).
	
	remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).


max([],0).
max([H|T],M):-
max(T,MT),
H>=MT,
M=H.


max([H|T],M):-
max(T,MT),
H<MT,
M=MT.



check_num(G):-
    grid_max(G,X),
	length(G,N),
	list2matrix(L,N,G),
    remove_duplicates(L ,U),
	X1 is X-1,
	helper_dementes(U , X1).
	
helper_dementes(_ , 0).
helper_dementes(L , X):-
    X \=0,
    member(X,L),
    X1 is X-1,
    helper_dementes(L ,X1). 

	
row_col_match(G):-
     acceptable_distribution(G),
     trans(G,T),
     row_col_match_helper1(G,T),!.
 
 
row_col_match_helper1([H] , [HT|TT]):-
    row_col_match_helper2(H , [HT|TT]). 
row_col_match_helper1([H|T] , [HT|TT]):-
    row_col_match_helper2(H , [HT|TT]),
    row_col_match_helper1(T , [HT|TT]).

row_col_match_helper2(R , [H]):-
    row_col_match_helper3(R , H).

row_col_match_helper2(R , [H|T]):-
     row_col_match_helper3(R , H);
	 row_col_match_helper2(R,T).

row_col_match_helper3([] ,[]).
row_col_match_helper3([H|T] , [H|T1]):-
    row_col_match_helper3(T , T1).	
	
	
	
helsinki(N,G):-
	grid_gen(N,G),
	check_num_grid(G),
	distinct_rows(G),
	distinct_columns(G),
	row_col_match(G).
	
perm([H|T],L):- perm(T,P),insert(H,P,L).
perm([],[]).

insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]):-
insert(X,T,T1).
    
	
	

	

	
	
	




  