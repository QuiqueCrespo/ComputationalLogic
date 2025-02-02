# Report 
Authors: Enrique Crespo and Omar Emara

Code: https://github.com/QuiqueCrespo/ComputationalLogic

## Grammar

We first have to define the syntactic and grammatical structure of the sentences, since they are the contact point between the user and prolexa. In order to enable the interaction to be meaningful we must define the main types of sentences and their respective logical representations. Therefore, we have defined sentences with transitive, intransitive and nominal predicates and their corresponding logical representations, as follows:

- Nominal predicates:
  - Singular: Toby is a dog => dog(toby):-true.
  - Plural: Dogs are big => big(A):-dog(A).
- Transitive predicate:
  - Singular: Alice owns Toby => own(alice,toby):-true
  - Plural: Geniuses win prizes => win(prize,A) :- genius(A).
- Intransitive predicate: - Singular: Alice flys => fly(alice):-true. - Plural: birds fly => fly(A):-bird(A)

By incorporating the ability to distinguish between general and particular statements the prolexa can learn and apply general rules.

```
% Grammar
sentence(Q) --> subject(s,S), predicate(s, S=>P), {Q=[(P:-true)]}.
sentence(Q) --> subject(p,X=>S), predicate(p,X=>P), {Q=[(P:-S)]}.

sentence(Q) --> subject(s,S), transitive_verb(s,S=>C=>P), direct_object(_,_=>C), {Q=[(P:-true)]}.
sentence(Q) --> subject(p,X=>S), transitive_verb(p,X=>C=>P), direct_object(_,_=>C), {Q=[(P:-S)]}.

predicate(N,M)  --> nominal_predicate(N,M).
predicate(N,M)   --> intransitive_verb(N,M).
```

### Existential quantification

Once the grammar is ready and prolexa is able to process the desired type of sentences, we are able to implement new reasoning features. In this part we will show how we have include the ability to interpret existential quatification. This is the ablility to undertand statemens such as: some humans are geniuses. And if the program also knows that "geniuses win prizes", then it should be able to infer that some humans win prizes.

In order to archive this, first need to implement the existential determiner "some". To this end we need to equip prolexa with the ability to interpret sentences with determiners:

```
sentence(Q) --> determiner(N,S,P,Q), subject(N,S), predicate(N,P).
sentence(Q) --> determiner(N,S,P,C,Q), subject(N,S),transitive_verb(N,P), direct_object(_,_=>C).
```

And the definitions of those determiners, along with the logic to interpret them. Here we also must introduce the distinction between transitive and intransitive predicates in order to include the direct object meaning into the logical interpretation of the sentence.

```
% Determiners intransitive and nominal
determiner(p, ex=>H1, ex=>H2, [(H1:-true),(H2 :- true)]) --> [some].
% Determiners transitive
determiner(p, ex=>H1, ex=>C=>H2,C, [(H1:-true),(H2 :- true)]) --> [some].

```

In this case Prolog interprets this sentences as:

- Some humans are geniuses => (human(ex):-true, genius(ex):-true)
- Some humans win prizes => (human(ex):-true, win(prize(_),ex):-true)

The reason for such a definition for the _some_ determinant is to introduce into the knowledge base at least one example of an atom that has both properties, in our case we call this atom _ex_.
To finalise the syntactical definitions we need to define the structure of the questions to allow us to query the knowledge base. Following the same logic we defined the following questions:

```
% questions
question((Q1,Q2)) --> [are,some],noun(p,X=>Q1),property(p,X=>Q2).
question((Q1,Q2)) --> [do,some],noun(p,X=>Q1),intransitive_verb(p,X=>Q2).
question((Q1,Q2)) --> [do,some],noun(p,X=>Q1),transitive_verb(p,X=>C=>Q2), direct_object(_,_=>C).
```

## Meta-interpreter

We have enabled Prolexa to interpret existential quantifiers, now we have to write the logic to enable it to answer queries about them. Given the following knowledge base:

```
some humans are geniuses.
geniuses win prizes.
birds fly.
some animals are birds.
```

The program should be able to answer the queries:

```
"do some humans win prizes".
"do some animals fly".
```

Additionally, when an explanation is required for this need to be answers:

```
"some humans are genius; geniuses win prizes; therefore some humans are genius"
"some animals are birds; birds fly; therefore some animals fly"
```

In order to archive this we have to edit the meta-interpreter:

```

% base case
prove_rb(true,_Rulebase,P,P):-!.

% conjunction
prove_rb((A,B),Rulebase,P0,P):-
	prove_rb(B,Rulebase,P0,P1),
	prove_rb(A,Rulebase,P1,P),!.

% direct proof
prove_rb([(A:-B)],Rulebase,P0,P):-
	find_clause((A:-B),Rule,Rulebase),
	prove_rb(true,_,[p((A:-B),Rule)|P0],P),!.

prove_rb((A,B),Rulebase,P0,P):-
	find_clause((A,B),Rule,Rulebase),
	prove_rb(true,_,[p((A,B),Rule)|P0],P),!.

% inference
prove_rb((A,B),Rulebase,P0,P):- 
    find_clause((B:-C),Rule,Rulebase),
    prove_rb((A,C),Rulebase,[p((A,B),Rule)|P0],P),!.

prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
    prove_rb(B,Rulebase,[p((A),Rule)|P0],P),!.
```

## Test

Given the following knowledge base:

```
some animals are birds.
birds fly.

peter is human.
peter is happy.
peter is genius.
geniuses win prizes.
```

This querys where tested

```
prolexa> "Explain why some animals fly".
*** utterance(Explain why some animals fly)
*** goal(explain_question((animal(_26630),fly(_26630)),_26586,_26372))
*** proof([p((animal(ex),bird(ex)),[(animal(ex):-true),(bird(ex):-true)]),p((animal(ex),fly(ex)),[(fly(_26936):-bird(_26936))])])
*** answer(some animals are birds; birds fly; therefore some animals fly)
some animals are birds; birds fly; therefore some animals fly


prolexa> "Explain why some humans are happy".
*** utterance(explain why some humans are happy)
*** goal(explain_question((human(_29730),happy(_29730)),_29686,_29446))
*** proof([p(human(peter),[(human(peter):-true)]),p(happy(peter),[(happy(peter):-true)])])
*** answer(peter is human; peter is happy; therefore some humans are happy)
peter is human; peter is happy; therefore some humans are happy

prolexa> "Explain why some humans win prizes".
*** utterance(explain why some humans win prizes)
*** goal(explain_question((human(_31610),win(_31610,prize(_31740))),_31564,_31324))
*** proof([p(human(peter),[(human(peter):-true)]),p(genius(peter),[(genius(peter):-true)]),p(win(peter,prize(_31740)),[(win(_31918,prize(_31924)):-genius(_31918))])])
*** answer(peter is human; peter is genius; geniuses win prizes; therefore some humans win prizes)
peter is human; peter is genius; geniuses win prizes; therefore some humans win prizes
```

We can therefore see the program is able to perform existential reasoning.

# Negation

To implement negation changes had to made to the following files: 1-prolexa.pl 2-prolexa_engine.pl 3-prolexa grammar. This section will detail and explain the changes required and document the testing done to evaluate the performance of the implementation. The negation was developed and tested in the context of happiness and teaching. In summary, we succeeded at enabling prolexa handle negation.

## Prolexa.pl - Rules

The changes to prolexa.pl included the changes in the rules which are defined below

```
stored_rule(1, [(teacher(X):-happy(X))]).
stored_rule(1,[(happy(peter):-true)]).
stored_rule(1, [(not(teacher(donald)):-true)]).
```

The first rules are based on prolog's syntax A:-B translating into if B is true then A is true. Therefore, the first rule implies that if you are happy then you are a teacher, the second rule implies that peter is happy and the third rule implies that donald is not a teacher.

## prolexa_grammar.pl - Grammar

The change in grammar was required to add the following features to prolexa: 1- understand negation in the question e.g. "explain why donald is not happy". 2-understand donald when being referred to in the sentence 3-respond with coherent sentences to the new required explaination. The changes can be found below:

```
pred(happy,  1,[a/happy]).
pred(teacher,  1,[n/teacher]).
```

The above defines happy as an adjective and teacher as a noun, both required for the handling of the new example.

```
proper_noun(s, donald) --> [donald]
```

The above define donald as a singular proper noun enabling prolexa to refer to him appropriately with the correct grammar, e.g. using is, where needed

```
sentence(Q) --> [everyone], adjective(s,X=>S), predicate(s,X=>P), {Q=[(P:-S)]}.
```

The above definiton of sentence is required to enable prolexa to understand sentences starting with "everyone" and enable it to produce sentence with similiar structure. The rest of the sentence definition is the same as explained above. 

```
nominal_predicate(N,not(M)) --> nominal_verb(N),[not],property(s,M).
```

The above is a modification of the definition of the nominal_predicate provided in the already developed prolexa code. The only different is that it allow a negation to be present following a nominal verb and before a property e.g. it can handle "is not happy" compared to the default only able to handle "is happy".

```
sentence(Q) --> subject(N,X),predicate(N,not(X=>L)), {Q=[(not(L):-true)]}.
question(Q) --> [is], subject(s, X),[not], property(s, X=>P), {Q=not(P)}.
question(Q) --> [are], subject(p, X=>S),[not], property(p, X=>P), {Q=[(not(P):-S)]}.
question(Q) --> [who], nominal_verb(N),[not], property(N, _=>P), {Q=not(P)}.
```

The above four definitions are extension of the definitions provided in prolexa's skeleton to allow prolexa to handle questions with "not" in it.

## prolexa_engine.pl - rule base proving

```
prove_rb(not(B),Rulebase,P0,P):- % Added for negation
	write_debug(B),nl,
    find_clause((A:-B),Rule,Rulebase),
	write_debug(A),nl,
    prove_rb(not(A),Rulebase,[p(not(B),Rule)|P0],P).
```

The above enables handling negation while proving. It uses a clause of what is being negated i.e. not B e.g. "not happy" in donalds example and then tries to find A:-B. The prove will look up if donald is a teacher or not in the rule base for A and proves not(teacher(donald)) which is true as per the rule base. Therefore, A:-B evaluates to true.  

## Testing

The testing will document the outputs as they are from the interactive prolexa terminal.

```
Welcome to SWI-Prolog (threaded, 64 bits, version 8.4.2)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- prolexa_cli.
prolexa> "Tell me everything you know".
*** utterance(Tell me everything you know)
*** goal(all_rules(_2706))
*** answer(everyone human is mortal. peter is human. some animals are birds. birds fly. donald is human. peter is genius. geniuses win prizes. everyone happy is a teacher. peter is happy. donald is not a teacher)
everyone human is mortal. peter is human. some animals are birds. birds fly. donald is human. peter is genius. geniuses win prizes. everyone happy is a teacher. peter is happy. donald is not a teacher
prolexa> "explain why peter is a teacher".
*** utterance(explain why peter is a teacher)
*** goal(explain_question(teacher(peter),_8328,_8084))
*** proof([p(happy(peter),[(happy(peter):-true)]),p(teacher(peter),[(teacher(_8562):-happy(_8562))])])
*** answer(peter is happy; everyone happy is a teacher; therefore peter is a teacher)
peter is happy; everyone happy is a teacher; therefore peter is a teacher
prolexa> "explain why donald is not happy".
*** utterance(explain why donald is not happy)
*** goal(explain_question(not(happy(donald)),_9690,_9450))
*** happy(donald)

*** teacher(donald)

*** proof([p(not(teacher(donald)),[(not(teacher(donald)):-true)]),p(not(happy(donald)),[(teacher(_9934):-happy(_9934))])])
*** answer(donald is not a teacher; everyone happy is a teacher; therefore donald is not happy)
donald is not a teacher; everyone happy is a teacher; therefore donald is not happy
```

The above testing shows that prolexa can now handle the rule teacher(X):-happy(X) as well as explain the negation of this rule.
