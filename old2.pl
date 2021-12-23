%%%% -*- Mode: Prolog -*- 
:- set_prolog_flag(double_quotes, chars).

uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment), Output) :- 
    current_output(Orig),
    set_output(Output),
    uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)),
    close(Output),
    set_output(Orig). 

uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :- 
    writef("URI Display: ","%n"),
    (
        (Userinfo \= [], atomics_to_string([Userinfo, @], StrUserinfo) )
    ; atomics_to_string([], StrUserinfo)
    ),
    (
        (Host \= [], atomics_to_string([Host], StrHost) )
    ; atomics_to_string([], StrHost)
    ),
    (
        (Port \= 80, Port \= [], atomics_to_string([:, Port], StrPort) )
    ; atomics_to_string([],StrPort)
    ),
    (
        (Path \= [], atomics_to_string([Path], StrPath) )
    ; atomics_to_string([], StrPath)
    ),
    (
        (Query \= [], atomics_to_string([?, Query], StrQuery) )
    ; atomics_to_string([], StrQuery)
    ),
    (
        (Fragment \= [], atomics_to_string([#, Fragment], StrFragment) )
    ; atomics_to_string([], StrFragment)
    ),

    atomics_to_string(
        [
            Scheme,
            ://,
            StrUserinfo,
            StrHost,
            StrPort,
            StrPath,
            StrQuery,
            StrFragment
        ],
        Stringa
    ),

    write(Stringa), nl,

    writef("Scheme   ==> ","%n"), write(Scheme), nl,
    writef("Userinfo ==> ","%n"), write(Userinfo), nl,
    writef("Host     ==> ","%n"), write(Host), nl,
    writef("Port     ==> ","%n"), write(Port), nl,
    writef("Path     ==> ","%n"), write(Path), nl,
    writef("Query    ==> ","%n"), write(Query), nl,
    writef("Fragment ==> ","%n"), write(Fragment), nl, nl, !.

uri_parse(X,Y) :- uri(Y,X,[]).

uri(uri(mailto, Userinfo, Host, [], [], [], [])) -->
    "mailto:", !,
    identificatore(List),
    opzionale_speciale(Host),
    { atom_codes(Userinfo, List)},!.

uri(uri(Lower, [], Host, [], [], [], [])) -->
    news(Scheme),
    ":", !,
    host(Host),
    {string_chars(String, Scheme), string_lower(String,Lower), write(Scheme)},
    !.

uri(uri(tel, Userinfo, [], [], [], [], [])) -->
    ("tel";"TEL";"Tel"),
    ":", !,
    userinfo(Userinfo), !.

uri(uri(zos, Userinfo, Host, Port, Path, Query, Fragment)) -->
    "zos:", !,
    authorithy(Userinfo,Host,Port),
    opzionaleZos(Path, Query, Fragment), !.

uri(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) -->
    scheme(Scheme),
    [:],
    authorithy(Userinfo,Host,Port),
    opzionale(Path, Query, Fragment), !.

/*uri(uri(Scheme,Userinfo,Host)) --> scheme(Scheme), !, [:], [/], [/], userinfo(Userinfo), host(Host), !.*/

news([Str|X]) --> [Str|X].

authorithy(Userinfo,Host,Port) -->
    "//", bloccoUser(Userinfo), host(Host), port(Port).

authorithy([],[],[])--> [].

/****** ZOS *****/
opzionaleZos(Path, Query, Fragment) -->
    [/], pathZos(Path), query(Query), fragment(Fragment).

opzionaleZos(Path, [], [])--> [/], pathZos(Path).


pathZos(Str) -->
    id44(Str44), "(", id8(Str8) , ")",
    { atomic_list_concat([/,Str44,/,Str8],Str)}.

pathZos(Str) --> id44(Str).

/****** ID44 *****/
id44(Str) -->
    alfa(X), carMid44(_, List),
    {atomic_list_concat([X|List], Str)}.
id44(X) --> alfa(X).

carMid44(X, [A|Res]) -->
    alfaNumPunto(A), carMid44(N, Res),
    {X is N+1, X<44}.
carMid44(1,[A]) --> alfaNumPunto(A), {A \= '.'}.

/****** ID8 *****/
id8(Str) -->
    alfa(X), carMid8(_, List),
    {atomic_list_concat([X|List], Str)}.
id8(X)	 --> alfa(X).

carMid8(X, [A|Res]) -->
    alfaNum(A), carMid8(N, Res),
    {X is N+1, X<8}.
carMid8(1,[A])	    --> alfaNum(A).

alfa(X)		--> [X], {char_type(X, alpha)}.
alfaNum(X)	--> [X], {char_type(X, alnum)}.
alfaNumPunto(X) --> [X], {char_type(X, alnum); X == '.'}.


/****** SCHEME *****/
scheme(Stringa) -->
    identificatore(Lista),
    { atom_codes(Stringa, Lista) }.

/****** USER INFO *****/
bloccoUser(Stringa) --> userinfo(Stringa), [@].
bloccoUser([])	    --> [].

userinfo(Stringa) -->
    identificatore(Lista),
    { atom_codes(Stringa, Lista) }.

/****** HOST *****/
opzionale_speciale(Host) --> [@], host(Host).
opzionale_speciale([])	 --> [].

host(Stringa) --> indirizzoIP(Stringa).
host(Stringa) --> letterale(Stringa).

letterale(Res) -->
    identificatore_host(Lista), [.], letterale(R),
    {
        atom_codes(Stringa, Lista),
        atomic_list_concat([Stringa, ., R],Res)
    }.

letterale(Str) -->
    identificatore_host(Lista),
    { atom_codes(Str, Lista)}.

indirizzoIP(Res) -->
    checkBlocco(Primo), [.],
    checkBlocco(Secondo), [.],
    checkBlocco(Terzo), [.],
    checkBlocco(Quarto),
    { atomic_list_concat([Primo, ., Secondo, ., Terzo, ., Quarto], Res) }.

checkBlocco(Number) -->
    bloccoDigits(Stringa),
    {
        atom_number(Stringa, Number),
        Number<256, Number >= 0
    }.

bloccoDigits(Stringa) -->
    digit(X), bloccoDigits(Cs),
    {atomics_to_string([X,Cs],Stringa)}.
bloccoDigits(Stringa) -->
    digit(X),
    {atom_string(X,Stringa)}.

digit(Res) -->
    [X],
    {atom_number(X,Res)}.

/****** PORT *****/
port(Number) -->
    [:], num(Stringa),
    {atom_number(Stringa, Number)}.
port(80)     --> [].

num(Stringa) -->
    digit(X), num(Cs),
    {atomics_to_string([X,Cs],Stringa)}.
num(String)  -->
    digit(X),
    {atom_string(X,String)}.

opzionale(Path, Query, Fragment) -->
    [/], path(Path), query(Query), fragment(Fragment).
opzionale([], [], []) --> [].

/****** PATH *****/
path(Percorso) --> percorso(Percorso).
path([])       --> [].

percorso(Percorso) -->
    identificatore(Lista), [/], percorso(Str),
    {
        atom_codes(Stringa, Lista),
        atomic_list_concat([/, Stringa, Str], Percorso)
    }.
percorso(Percorso) -->
    identificatore(Lista),
    {
        atom_codes(Str, Lista),
        atomic_list_concat([/, Str], Percorso)
    }.

/****** QUERY *****/
query(Stringa) -->
    [?], caratteri_query(List),
    {atomic_list_concat(List, Stringa)}.
query([])      --> [].

caratteri_query([X|Cs]) -->
    [X], caratteri_query(Cs),
    {X \= '#'}.
caratteri_query([X])	-->
    [X],
    {X \= '#'}.


/****** FRAGMENT *****/
fragment(Stringa) -->
    [#], caratteri_frag(List),
    {atomic_list_concat(List, Stringa)}.
fragment([])	  --> [].

caratteri_frag([X|Cs]) -->
    [X], caratteri_frag(Cs).
caratteri_frag([X])    --> [X].

identificatore([X|Cs]) -->
    carattere(X), identificatore(Cs).
identificatore([X])    --> carattere(X).

identificatore_host([X|Cs]) -->
    carattere(X), {X \= '.'}, identificatore_host(Cs).
identificatore_host([X])    -->
    carattere(X), {X \= '.'}.

carattere(X) -->
    [X],
    { X \= '/', X \= '?', X \= '#', X \= '@', X \= ':'}.
