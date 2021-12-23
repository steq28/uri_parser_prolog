
/*carattere_host(X) --> [X], {X \= '.', X \= '/', X \= '?', X \= '#', X \= '@', X \= ':'}.*/

/*
host(X) --> identificatore(X).
host(X|Cs) --> identificatore(X), "." , host(Cs).*/


/*identificatore(Codes) --> carattere(X), identificatore(Cs) , {atomics_to_string([X|Cs], Codes)}.*/
/*identificatore(Carattere) --> carattere(string(X)), {atom_codes(X, Carattere)}.*/
/*authorithy(User) --> [/], [/], userinfo(User).*/

/*
uri_parse(X,Y) :- phrase(uri(X),Y).

uri(uri(Scheme)) --> scheme(Scheme), [:] . //, authorithy.

scheme(X) --> identificatore(X).*/

/*authorithy --> "//", userinfo, host.


userinfo --> [].
userinfo --> identificatore, [@].

host(X) --> identificatore_host(X).
host(X|Cs) --> identificatore_host(X), "." , host(Cs).

identificatore_host([X]) --> carattere(X), !.
identificatore_host([X|Cs]) --> carattere(X), identificatore_host(Cs).
*/

/*
identificatore([X]) --> carattere(X), !.
identificatore([X|Cs]) --> carattere(X), identificatore(Cs).

carattere(X) --> [X], { X \= '/', X \= '?', X \= '#', X \= '@', X \= ':' }.


*/

