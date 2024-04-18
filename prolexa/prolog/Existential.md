## Grammar

### Transitive predicates

We first have to define the syntactic and grammatical structure of the sentences. For this part we have defined sentences with transitive, intransitive and nominal predicates. The program handles sentences in this way:

- Nominal predicates:
  - Singular: Toby is a dog => dog(toby):-true.
  - Plural: Dogs are big => big(A):-dog(A).
- Transitive predicate:
  - Singular: Alice owns Toby => own(alice,toby):-true
  - Plural: Geniuses win prizes => win(prize,A) :- genius(A).
- Intransitive predicate: - Singular: Alice flys => fly(alice):-true. - Plural: birds fly => fly(A):-bird(A)
  By incorporating the ability to distinguish between general and particular statements the prolexa can learn and apply general rules.

  > % Grammar
  > sentence(Q) --> subject(s,S), predicate(s, S=>P), {Q=[(P:-true)]}.
  > sentence(Q) --> subject(p,X=>S), predicate(p,X=>P), {Q=[(P:-S)]}.
  > sentence(Q) --> subject(s,S), transitive*verb(s,S=>C=>P), direct_object(*,_=>C), {Q=[(P:-true)]}.
  > sentence(Q) --> subject(p,X=>S), transitive_verb(p,X=>C=>P), direct_object(_,\_=>C), {Q=[(P:-S)]}.


### Existential quantification

We must introduce the ability to interpret the existential determiner some. We simply add the following:

> sentence(Q) --> determiner(N,S,P,Q), subject(N,S), predicate(N,P).
> sentence(Q) --> determiner(N,S,P,C,Q), subject(N,S),transitive*verb(N,P), direct_object(*,\_=>C).

And the definitions of those determiners, along with the logic to interpret them. Here we also must introduce the distinction between transitive and intransitive predicates in order to include the direct object meaning into the logical interpretation of the sentence.

> % Determiners intransitive and nominal
> determiner(p, ex=>H1, ex=>H2, [(H1:-true),(H2 :- true)]) --> [some].
> % Determiners transitive
> determiner(p, ex=>H1, ex=>C=>H2,C, [(H1:-true),(H2 :- true)]) --> [some].

In this case Prolog interprets this sentences as: - Some humans are geniuses => (human(ex):-true, genius(ex):-true) - Some humans win prizes => (human(ex):-true, win(prize,ex):-true)

The reason for such a definition for the _some_ determinant is to introduce into the knowledge base at least one example of an atom that has both properties, in our case we call this atom _ex_.
To finalise the syntactical definitions we need to define the structure of the questions to allow us to query the knowledge base. Following the same logic we defined the following questions:


> % questions
> question((Q1,Q2)) --> [are,some],noun(p,X=>Q1),property(p,X=>Q2).
> question((Q1,Q2)) --> [do,some],noun(p,X=>Q1),intransitive*verb(p,X=>Q2).
> question((Q1,Q2)) --> [do,some],noun(p,X=>Q1),transitive_verb(p,X=>C=>Q2), direct_object(*,\_=>C).


## Meta-interpreter

We have enabled Prolexa to interpret existential quantifiers, now we have to write the logic to enable it to answer queries about them. Given the following knowledge base:


> peter is human.
> peter is genius.
> geniuses win prizes.
> birds fly.
> some animals are birds.

The program should be able to answer the queries:

> "do some humans win prizes".
> "do some animals fly".

Additionally, when an explanation is required for this need to be answers:

> "peter is human; peter is genius; therefore some humans are genius"
> "some animals are birds; birds fly; therefore some animals fly"

In order to archive this we have to edit the meta-interpreter:

> prove_rb((A,B),Rulebase,P0,P):-
> prove_rb(B,Rulebase,P0,P1),
> prove_rb(A,Rulebase,P1,P),!.

> prove*rb((A,B),Rulebase,P0,P):-
> find_clause((A,B),Rule,Rulebase),
> prove_rb(true,*,[p((A,B),Rule)|P0],P),!.

> prove_rb((A,B),Rulebase,P0,P):-
> find_clause((A,C),Rule,Rulebase),
> prove_rb((C,B),Rulebase,[p((A,B),Rule)|P0],P),!.

> prove_rb((A,B),Rulebase,P0,P):-
> find_clause((B:-C),Rule,Rulebase),
> prove_rb((A,C),Rulebase,[p((A,B),Rule)|P0],P),!.

> prove*rb([(A:-B)],Rulebase,P0,P):-
> find_clause((A:-B),Rule,Rulebase),
> prove_rb(true,*,[p((A:-B),Rule)|P0],P),!.

> prove_rb(A,Rulebase,P0,P):-
> find_clause((A:-B),Rule,Rulebase),
> prove_rb(B,Rulebase,[p((A),Rule)|P0],P),!.


### Limitations

While this programs has passed all our test we believe the meta-interpreter can be written in a more elegant and robust way, unifying both cases.
