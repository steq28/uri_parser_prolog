/*
    Stefano Quaggio 866504
*/

%%%% -*- Mode: Prolog -*-
:- set_prolog_flag(double_quotes, chars).

uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment), Output) :- 
    current_output(Orig),
    set_output(Output),
    uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)),
    close(Output),
    set_output(Orig). 

uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :- 
    writef("URI Display: ","%n"), nl,
    writef("Scheme   ==> ","%n"), write(Scheme), nl,
    writef("Userinfo ==> ","%n"), write(Userinfo), nl,
    writef("Host     ==> ","%n"), write(Host), nl,
    writef("Port     ==> ","%n"), write(Port), nl,
    writef("Path     ==> ","%n"), write(Path), nl,
    writef("Query    ==> ","%n"), write(Query), nl,
    writef("Fragment ==> ","%n"), write(Fragment), nl, nl, !.

uri_parse(X,Y) :- uri(Y,X,[]).

/****** MAILTO SPECIAL SYNTAX *****/
uri(uri(Atom, Userinfo, Host, [], [], [], [])) -->
    schemeLower(Atom),
    {
        Atom == 'mailto'
    }, !,
    ":",
    mailto(Userinfo, Host), !.

/****** NEWS SPECIAL SYNTAX *****/
uri(uri(Atom, [], Host, [], [], [], [])) -->
    schemeLower(Atom),
    {
        Atom == 'news'
    }, !,
    ":",
    host(Host), !.

/****** TEL & FAX SPECIAL SYNTAX *****/
uri(uri(Atom, Userinfo, [], [], [], [], [])) -->
    schemeLower(Atom),
    {
        Atom == 'tel' ;
        Atom == 'fax'
    }, !,
    ":",
    userinfoOpzionale(Userinfo), !.

/****** ZOS SPECIAL SYNTAX *****/
uri(uri(Atom, Userinfo, Host, Port, Path, Query, Fragment)) -->
    schemeLower(Atom),
    {
        Atom == 'zos'
    }, !,
    ":",
    authorithy(Userinfo,Host,Port),
    opzionaleZos(Path, Query, Fragment), !.

/****** GENERAL SYNTAX *****/
uri(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) -->
    scheme(Scheme),
    ":",
    authorithy(Userinfo,Host,Port),
    opzionale(Path, Query, Fragment), !.

schemeLower(Atom) --> [Str|X],
    {
        atomics_to_string([Str|X], String),
        string_lower(String, Lower),
        atom_string(Atom, Lower)
    }.


/****** ZOS *****/
opzionaleZos(Path, Query, Fragment) -->
    [/], pathZos(Path), query(Query), fragment(Fragment).

opzionaleZos(Path, [], [])--> [/], pathZos(Path).
%opzionaleZos([], [], [])--> [].

pathZos(Str) -->
    id44(Str44), "(", id8(Str8) , ")",
    { atomic_list_concat([Str44, '(', Str8, ')'],Str)}.

pathZos(Str) --> id44(Str).
pathZos([]) --> [].

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

mailto(Userinfo, Host) --> userinfo(Userinfo), opzionale_speciale(Host).
mailto([], []) --> [], [].

/****** AUTHORITHY *****/
authorithy(Userinfo,Host,Port) -->
    "//", bloccoUser(Userinfo), host(Host), port(Port).

authorithy([],[],80)--> [].

/****** USER INFO *****/
bloccoUser(Stringa) --> userinfo(Stringa), [@].
bloccoUser([])	    --> [].

userinfo(Stringa) -->
    identificatore(Lista),
    { atom_codes(Stringa, Lista) }.

userinfoOpzionale(Userinfo) --> userinfo(Userinfo).
userinfoOpzionale([]) --> [].

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
        atomic_list_concat([Stringa, /, Str], Percorso)
    }.
percorso(Percorso) -->
    identificatore(Lista),
    {
        atom_codes(Str, Lista),
        atomic_list_concat([Str], Percorso)
    }.

/****** QUERY *****/
query(Stringa) -->
    [?], caratteri_query(List),
    {atomic_list_concat(List, Stringa)}.
query([])      --> [].

caratteri_query([X|Cs]) -->
    [X], caratteri_query(Cs),
    {X \= '#'}.
caratteri_query([X])    -->
    [X],
    {X \= '#'}.


/****** FRAGMENT *****/
fragment(Stringa) -->
    [#], caratteri_frag(List),
    {atomic_list_concat(List, Stringa)}.
fragment([])	  --> [].

caratteri_frag([X|Cs]) -->
    [X], caratteri_frag(Cs).

caratteri_frag([X])--> [X].

identificatore([X|Cs]) -->
    carattere(X), identificatore(Cs).
identificatore([X])    --> carattere(X).

identificatore_host([X|Cs]) -->
    carattere(X), {X \= '.'}, identificatore_host(Cs).
identificatore_host([X])-->
    carattere(X), {X \= '.'}.

carattere(X) -->
    [X],
    { X \= '/', X \= '?', X \= '#', X \= '@', X \= ':'}.
