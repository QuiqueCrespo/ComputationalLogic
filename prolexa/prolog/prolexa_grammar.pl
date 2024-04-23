:-op(600,xfy,'=>').

utterance(C) --> sentence(C).
utterance(C) --> question(C).
utterance(C) --> command(C).
% Grammar
sentence(Q) --> [everyone], adjective(s,X=>S), predicate(s,X=>P), {Q=[(P:-S)]}.
sentence(Q) --> subject(s,S), predicate(s, S=>P), {Q=[(P:-true)]}.
sentence(Q) --> subject(p,X=>S), predicate(p,X=>P), {Q=[(P:-S)]}.
sentence(Q) --> determiner(N,S,P,Q), subject(N,S), predicate(N,P).
sentence(Q) --> subject(s,S), transitive_verb(s,S=>C=>P), direct_object(_,_=>C), {Q=[(P:-true)]}.
sentence(Q) --> subject(p,X=>S), transitive_verb(p,X=>C=>P), direct_object(_,_=>C), {Q=[(P:-S)]}.
sentence(Q) --> determiner(N,S,P,C,Q), subject(N,S),transitive_verb(N,P), direct_object(_,_=>C).
sentence(Q) --> subject(N,X),predicate(N,not(X=>L)), {Q=[(not(L):-true)]}.
%sentence(Q) --> determiner(N,S,P,C,Q),noun(N,S),predicate(N,not(P)).


subject(N, M) --> noun_phrase(N, M).
direct_object(N, M) --> noun_phrase(N, M).

predicate(N,M)  --> nominal_predicate(N,M).
predicate(N,M)   --> intransitive_verb(N,M).

noun_phrase(N, M) --> proper_noun(N, M).
noun_phrase(p, M) --> noun(p, M).
noun_phrase(s, M) --> [a], noun(s,M).
noun_phrase(N, M) --> article(N), noun(N, M).


nominal_predicate(N, M) --> nominal_verb(N), property(N,M).
nominal_predicate(N,not(M)) --> nominal_verb(N),[not],property(s,M). %add for negation -> works in terms of NLP but cannot interpret it=
property(N, M) --> adjective(N, M).
property(s, M) --> [a],noun(s, M).
property(p, M) --> noun(p, M).





% querstions
question((Q1,Q2)) --> [are,some],noun(p,X=>Q1),property(p,X=>Q2).
question((Q1,Q2)) --> [do,some],noun(p,X=>Q1),intransitive_verb(p,X=>Q2).
question((Q1,Q2)) --> [do,some],noun(p,X=>Q1),transitive_verb(p,X=>C=>Q2), direct_object(_,_=>C).
question(Q) --> [is], subject(s, X), property(s, X=>Q).
question(Q) --> [are], subject(p, X=>S), property(p, X=>P), {Q=[(P:-S)]}.
question(Q) --> [who], nominal_verb(N), property(N, _=>Q).

question(Q) --> [is], subject(s, X),[not], property(s, X=>P), {Q=not(P)}.
question(Q) --> [are], subject(p, X=>S),[not], property(p, X=>P), {Q=[(not(P):-S)]}.
question(Q) --> [who], nominal_verb(N),[not], property(N, _=>P), {Q=not(P)}.

question(Q) --> [does], subject(s, X),intransitive_verb(s, X=>Q).
question(Q) --> [do], subject(p, X=>S), intransitive_verb(p, X=>P), {Q=[(P:-S)]}.
question(Q) --> [who], intransitive_verb(s, _=>Q).

question(Q) --> [does], subject(s, X), transitive_verb(p, X=>Y=>Q), direct_object(_, _=>Y).
question(Q) --> [do], subject(p, X=>S), transitive_verb(p, X=>Y=>P), direct_object(_, _=>Y), {Q=[(P:-S)]}.
question(Q) --> [who], transitive_verb(s, _=>X=>Q), direct_object(_, X).



% Vocabulary
pred2gr(P,1,C/W,X=>Lit):-
	pred(P,1,L),
	member(C/W,L),
	Lit=..[P,X].

pred2gr(P,2,C/W,X=>Y=>Lit):- 
    pred(P,2,L), 
    member(C/W,L), 
    Lit=..[P,X,Y].

% Lexicon
adjective(_,M)		--> [Adj],    {pred2gr(_P,1,a/Adj, M)}.
noun(s,M)			--> [Noun],   {pred2gr(_P,1,n/Noun,M)}.
noun(p,M)			--> [Noun_p], {pred2gr(_P,1,n/Noun,M),noun_s2p(Noun,Noun_p)}.
nominal_verb(s)	--> [Verb_s], {pred2gr(_P,1,nv/Verb,_),verb_p2s(Verb,Verb_s)}.
nominal_verb(p)	--> [Verb],   {pred2gr(_P,1,nv/Verb,_)}.
transitive_verb(s,M)			--> [Verb_s], {pred2gr(_P,2,v/Verb,M),verb_p2s(Verb,Verb_s)}.
transitive_verb(p,M)			--> [Verb],   {pred2gr(_P,2,v/Verb,M)}.
intransitive_verb(s,M)		--> [Verb_s], {pred2gr(_P,1,iv/Verb,M),verb_p2s(Verb,Verb_s)}.
intransitive_verb(p,M)		--> [Verb],   {pred2gr(_P,1,iv/Verb,M)}.

noun_s2p(Noun_s,Noun_p):-
	( Noun_s=woman -> Noun_p=women
	; Noun_s=man -> Noun_p=men
    ; Noun_s=genius -> Noun_p=geniuses
	; atom_concat(Noun_s,s,Noun_p)
	).

verb_p2s(Verb_p,Verb_s):-
	( Verb_p=are -> Verb_s=is
    ;Verb_p=fly -> Verb_s=flies
	; 	atom_concat(Verb_p,s,Verb_s)
	).

pred(human,   1,[a/human,n/human]).
pred(mortal,  1,[a/mortal,n/mortal]).
pred(big,    1,[a/big]).
pred(fly,    1,[iv/fly]).
pred(bird,   1,[n/bird]).
pred(is,     1,[nv/are]).
pred(dog,    1,[n/dog]).
pred(animal, 1,[n/animal]).
pred(good,  1,[a/good]).
pred(genius,  1,[a/genius,n/genius]).
pred(win,  2,[v/win]).
pred(own,    2,[v/own]).
pred(prize,  1,[n/prize]).

pred(happy,  1,[a/happy]).
pred(teacher,  1,[n/teacher]).



%proper names
proper_noun(s,peter) --> [peter].
proper_noun(s, donald) --> [donald]. %add for negation
proper_noun(s, alice) --> [alice].
proper_noun(s, bob) --> [bob].
proper_noun(s, ex) --> [ex].
% Articles
article(_) --> [the].
% Determiners
determiner(p, _ex=>H1, _ex=>H2, [(H1:-true),(H2 :- true)]) --> [some].
determiner(s, X=>S, X=>P, [(P:-S)]) --> [every].
determiner(p, X=>S, X=>P, [(P:-S)]) --> [all].
determiner(p, _ex=>H1, _ex=>C=>H2,C, [(H1:-true),(H2 :- true)]) --> [some].
% determiner(s, X=>S, X=>C=>P,C, [(P:-S)]) --> [everyone].
determiner(s, X=>S, X=>C=>P,C, [(P:-S)]) --> [every].
determiner(p, X=>S, X=>C=>P,C, [(P:-S)]) --> [all].


%%% commands %%%

% These DCG rules have the form command(g(Goal,Answer)) --> <sentence>
% The idea is that if :-phrase(command(g(Goal,Answer)),UtteranceList). succeeds,
% it will instantiate Goal; if :-call(Goal). succeeds, it will instantiate Answer.
% See case C. in prolexa.pl
% Example: 
%	command(g(random_fact(Fact),Fact)) --> [tell,me,anything].
% means that "tell me anything" will trigger the goal random_fact(Fact), 
% which will generate a random fact as output for prolexa.

command(g(retractall(prolexa:stored_rule(_,C)),"I erased it from my memory")) --> forget,sentence(C). 
command(g(retractall(prolexa:stored_rule(_,_)),"I am a blank slate")) --> forgetall. 
command(g(all_rules(Answer),Answer)) --> kbdump. 
command(g(all_answers(PN,Answer),Answer)) --> tellmeabout,proper_noun(s,PN).
command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence([(Q:-true)]).
command(g(explain_question((Q,P),_,Answer),Answer)) --> [explain,why],sentence([(Q:-true),(P:-true)]).
command(g(random_fact(Fact),Fact)) --> getanewfact.

command(g(rr(A),A)) --> thanks.


command(g(true,"I can do a little bit of logical reasoning. You can talk with me about humans and birds.")) --> [what,can,you,do,for,me,minerva]. 

thanks --> [thank,you].
thanks --> [thanks].
thanks --> [great,thanks].

getanewfact --> getanewfact1.
getanewfact --> [tell,me],getanewfact1.

getanewfact1 --> [anything].
getanewfact1 --> [a,random,fact].
getanewfact1 --> [something,i,'don\'t',know].

kbdump --> [spill,the,beans].
kbdump --> [tell,me],allyouknow.

forget --> [forget].

forgetall --> [forget],allyouknow.

allyouknow --> all.
allyouknow --> all,[you,know].

all --> [all].
all --> [everything].

tellmeabout --> [tell,me,about].
tellmeabout --> [tell,me],all,[about].

rr(A):-random_member(A,["no worries","the pleasure is entirely mine","any time, peter","happy to be of help"]).

random_fact(X):-
	random_member(X,["walruses can weigh up to 1900 kilograms", "There are two species of walrus - Pacific and Atlantic", "Walruses eat molluscs", "Walruses live in herds","Walruses have two large tusks"]).
