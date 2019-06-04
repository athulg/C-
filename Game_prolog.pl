:- dynamic  murderer/3.
:- dynamic  person/3.
:- dynamic  suspect/3.
:- dynamic  innocent/3.
:- use_module(library(random)).
:- use_module(library(lists)).

%Function to print information about the game.


game:- 	% starts the game i.e clears the database, generates the murderer and displays content of the menu.
    cleanDataBase,
    wanted,
    go.

go:-
    write('Choose an option from the below menu.'),
    nl,nl,
    write('------------MENU------------'),
    nl,
    write('1. Generate a new clue. '),
    nl,
    write('2. Check for the latest entry created. '),
    nl,
    write('3. Display the list of innocents so far. '),
    nl,
    write('4. Display the list of suspects so far?'),
    nl,
    write('5. Guess the murderer'),
    nl,
    write('6. Exit (In case you do not wish to continue guessing and want to exit )'),
    nl, nl, nl,
    write('Input your choice: '),
    nl, nl,
    read(Number),
    check_if_valid_input(Number).

check_if_valid_input(Number):- % To check if the input is within the expected range i.e 1-6, 42 is cheat code.
    integer(Number),
    ((Number>0,Number<7);(Number == 42)),
    play_game(Number).


check_if_valid_input(Number):- % To check if the input is within the expected range i.e 1-6, if not go back to the menu and try again.
    (atom(Number);
    (Number<1;Number>6));
    write('Invalid Input, try again!'),
    go.

member_of_age_gender(X):-  % To check for valid input of age while guessing the murderer.
    age_gender_list(Y),
    member(X,Y).

member_of_age_gender(X):- % To check for invalid input of age while guessing the murderer.
    age_gender_list(Y),
    \+member(X,Y), nl,
    write('Incorrect input!'), nl,
    write('Try again: '), nl,
    read(Z), nl,
    member_of_age_gender(Z).

member_of_color(X):- % To check for valid input of color while guessing the murderer.
    color_list(A),
    member(X,A).

member_of_color(X):- % To check for invalid input of color while guessing the murderer.
    color_list(A),
    \+member(X,A), nl,
    write('Incorrect input!'), nl,
    write('Try again: '), nl,
    read(Z), nl,
    member_of_color(Z).

member_of_weapon(X):- % To check for valid input of weapon while guessing the murderer.
    weapon_list(A),
    member(X,A).

member_of_weapon(X):- % To check for invalid input of weapon while guessing the murderer.
    weapon_list(A),
    \+member(X,A), nl,
    write('Incorrect input!'), nl,
    write('Try again: '), nl,
    read(Z), nl,
    member_of_weapon(Z).

play_game(1):- % Loop to run the gane as per user selected choices.
    get_person, nl,
    go.
play_game(2):-
    latest_person, nl,
    go.
play_game(3):-
    list_innocents, nl,
    go.
play_game(4):-
    list_suspects, nl,
    go.
play_game(5):-
    write('Time to guess who the murderer is!'), nl,
    write('The different options to choose from are: '),nl,
    listing(age_gender_list/1),
    write('What age and gender does the murderer have?'), nl,
    read(X), nl,
    member_of_age_gender(X),
    listing(color_list/1),
    write('What color of clothing does the murderer have?'), nl,
    read(Y),
    member_of_color(Y),
    listing(weapon_list/1),
    write('What weapon did the murderer use?'),
    read(Z),
    member_of_weapon(Z),
    guess_murderer(X,Y,Z),
    write('The game will now start over from the beginning.'), nl, nl, nl, nl,
    game, nl.
play_game(6):-
    write('Good bye, the game ends now!'),
    cleanDataBase.
play_game(42):-
    listing(murderer/3),
    go.

%The different age_gender attributes.
age_gender_list([youngman,oldman,youngwoman,oldwoman]).

%The different color attributes.
color_list([brown,yellow,blue]).

%The different weapon attributes
weapon_list([knife,poison,gun]).

%Predicate to get all innocents.
list_innocents:-
    listing(innocent/3).

%Predicate to det all the suspects
list_suspects:-
    listing(suspect/3).

%Predicate to get the last person called and if its a suspect or innocent.
latest_person:-
    murderer(A,B,C),
    person(X,Y,Z),
    (X \== A,Y \== B, Z \== C),
    write('Innocent: '),
    listing(person/3), nl.
    
%Predicate to get the last person called and if its a suspect or innocent.
latest_person:-
    murderer(A,B,C),
    person(X,Y,Z),
    (X == A; Y == B; Z == C),
    write('Suspect: '),
    listing(person/3), nl.

%Predicate to call the game to create a murderer to start the game.
wanted:-
    create_murderer,
    murderer(A,B,C).
   
%Predicate to call the game to create a new person.
get_person:-   
    create_person,
    latest_person.

%Predicate to reset the game.
cleanDataBase:-
    retractall(murderer(A,B,C)),
    retractall(suspect(X,Y,Z)),
    retractall(innocent(X,Y,Z)),
    retractall(person(X,Y,Z)).

%Function to calculate how many persons youve generated.
no_of_tries(Trials):-
    findall(suspect(X,Y,Z), suspect(X,Y,Z), Suspect_list),
    length(Suspect_list,Snum),
    findall(innocent(X,Y,Z), innocent(X,Y,Z), Innocent_list),
    length(Innocent_list,Inum),
    Trials is Snum+Inum.

%The function to guess the murderer and end the game. Tells you if you won or lost and how many persons you generated.
guess_murderer(A,B,C):-
    murderer(X,Y,Z),
    (A==X, B==Y, C==Z),
    write('Murderer caught, good job!'),
    nl,
    no_of_tries(Trials),
    write('You guessed it right within: '),
    write(Trials),
    write(' tries.'), nl,
    cleanDataBase.

%The function to guess the murderer and end the game. Tells you if you won or lost and how many persons you generated.
guess_murderer(A,B,C):-
    murderer(X,Y,Z),
    (A\==X; B\==Y; C\==Z),
    write('Wrong suspect, game over!'),nl,
    write('The murderer was: '),
    write(murderer(X,Y,Z)),
    no_of_tries(Trials),
    write('You exhausted: '),
    write(Trials),
    write(' trial.'), nl,
    cleanDataBase.

%Function to get a random element from a list.
randomElement(List,Element):-
    length(List,Length),
    Max is Length + 1,
    random(1,Max,Z),
    nth1(Z,List,Element).

%Function to figure out if a person should be considered to be innocent or suspect, and then assert said person to respective innocent/suspect list.
check_for_suspect_or_innocent(Element1,Element2,Element3):-
    murderer(A,B,C),
    (Element1 \== A,Element2 \== B, Element3 \== C),
    findall(innocent(X,Y,Z), innocent(X,Y,Z), Innocent_list),
    \+member(innocent(Element1,Element2,Element3),Innocent_list),
    assert(innocent(Element1,Element2,Element3)).

%Function to figure out if a person should be considered to be innocent or suspect, and then assert said person to respective innocent/suspect list.
check_for_suspect_or_innocent(Element1,Element2,Element3):-
    murderer(A,B,C),
    (Element1 == A; Element2 == B; Element3 == C),
    findall(suspect(X,Y,Z), suspect(X,Y,Z), Suspect_list),
    \+member(suspect(Element1,Element2,Element3), Suspect_list),
    assert(suspect(Element1,Element2,Element3)).

%The function to create the murderer called from the "wanted" predicate.
create_murderer:-
    age_gender_list(A_list),
    color_list(C_list),
    weapon_list(W_list),

    randomElement(A_list, A),
    randomElement(C_list, B),
    randomElement(W_list, C),
    assert(murderer(A,B,C)).

%The function to create a person called from the "get_person" predicate.
create_person:-
    retractall(person(F,G,H)),
    maximum_combinations(B),
    no_of_tries(Trials),
    Trials < B,
    age_gender_list(A1_list),
    color_list(C1_list),
    weapon_list(W1_list),

    randomElement(A1_list, A1),
    randomElement(C1_list, B1),
    randomElement(W1_list, C1),
    \+murderer(A1,B1,C1),
    check_for_suspect_or_innocent(A1,B1,C1),
    assert(person(A1,B1,C1)).  % Push it to the create_person_List.

%The function to create a person called from the "get_person" predicate.
create_person:-
    %I will check the combination if all possible combinations are reached.
    maximum_combinations(B),
    no_of_tries(Trials),
    Trials < B,
    create_person.

%The function to create a person called from the "get_person" predicate.
create_person:-
    %I will check the combination if all possible combinations are reached.
    maximum_combinations(B),
    no_of_tries(Trials),
    Trials >= B,
    write('No more available suspects').

%Sets the maximum number of combinations for persons based on the number of attributes. Real number is 36 but the list starts at 0 so we set max number to 35 here.
maximum_combinations(A):-
    color_list(X),
    length(X,Cnum),
    age_gender_list(Y),
    length(Y,Anum),
    weapon_list(Z),
    length(Z,Wnum),
    A is (Cnum*Anum*Wnum)-1.

%Debugging tool.    
test:-
    cleanDataBase,
    create_murderer,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    create_person,
    listing(person/3),
    listing(innocent/3),
    listing(suspect/3),
    listing(murderer/3).