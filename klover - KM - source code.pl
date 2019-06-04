:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- use_module(library(tcltk)).

%dynamic predicates for tcl GUI and verification of rule base: 
:- dynamic tcl/1, tcl_question/5, answer_options/1, num_answers/1, attr_cf/3, num_attr_cf/2, subsumed_rules/4, missing_rules/2, non_reached_rule/2, not_unique/1, redundant_rule/2, no_goal/1, missing_goal/1.

                                % Window 1 - INTERPRETER

% *******************************************
% ************* Top level **********************

%activate/deactivate tcl graphic user interface, change value "on" to something else to turn off tcl GUI 
tcl_GUI(on).   
%tcl_GUI(off).

%Clean window and displays Options menu (tcl).
expert:- 
		tcl_GUI(on),
		retractall(tcl(_)),
		delete_all_tcl,
		tk_new([top_level_events,name('Klöver')],Tcl),
		assert(tcl(Tcl)),
		Source = 'source k:/klöver/menu.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event),
		start_manipulate(Event),
        control_new_option(Event).
%Clean window and displays Options menu (SICStus).		
expert:-
		\+tcl_GUI(on),
		nl,
        write('Options:'),nl,nl,
        menue_write(['Consult' ,'Consult with given answers',
            'Save answers', 'Save session', 'Fetch old answers',
            'Fetch old session','List database', 'Verify rules', 'Quit']),
        read(Answer),
        start_manipulate(Answer),
        control_new_option(Answer).

start_manipulate(Answer):-
        run_option(Answer).

control_new_option(Answer):-
        Answer \== 'Quit',
        Answer \== 'Interrupt',
        new_menue(New_Answer),        
        start_manipulate(New_Answer),
        control_new_option(New_Answer).

control_new_option(New_Answer).

% Menu represented with the results presentation (Tcl).
new_menue(Event):-
		tcl_GUI(on),
		delete_all_tcl,
		Source = 'source k:/klöver/new_menu.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event).
% Menu represented with the results presentation (SICStus).
new_menue(Answer):-
          \+tcl_GUI(on),
		  nl,
          menue_write(['New session' ,'Consult with given answers',
             'Change answers','Save answers', 'Save session',
             'Fetch old answers','Fetch old session','List database', 'Verify rules', 
             'How explanation', 'Interrupt', 'Select an option, please! ']),
        read(Answer).		

menue_write([]):- nl.
menue_write([First|List]):-
        write(First),nl,
        menue_write(List).


% Gives consultation without old answers.
run_option('Consult'):-
                        retractall(answer(_,_)),
                        consultation.
% Consultation with already given answers.

run_option('Consult with given answers'):-
                        consultation.

run_option('New session'):-
        run_option('Consult').

run_option('Change answers'):-
		\+tcl_GUI(on),
        setof((Attribute,=,Value), Value^answer(Attribute,  Value), List_of_answers),
        menue_write(List_of_answers),        
        read_input(List_of_answers, To_change),
        delete_answers(To_change),
        ask_user(To_change).
run_option('Change answers'):-
		tcl_GUI(on),
        bagof((Attribute,Value), Value^answer(Attribute,  Value), List_of_answers),
        assert_each_option(List_of_answers),
        assert_num_options(List_of_answers),
		Source = 'source k:/klöver/change_answers.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event),
		tcl_list_to_prolog_list(Event,ASCII_Ans),
		get_answer_list(List_of_answers, ASCII_Ans, To_change), 
		convert_to_objects(To_change, New),
		delete_answers(New),
        ask_user(New).
run_option('Change answers'):-
		tcl_GUI(on).	
		
run_option('Change answers'):-
        \+tcl_GUI(on),
		write_no_answers.

% Saves answers and complete session on file.
run_option('Save answers'):-
        store_answers,!.
%if user choose to not save new file (i.e. store_answers fails)		
run_option('Save answers'):-
        tcl_GUI(on).
run_option('Save session'):-
        store_session,!.
%if store_session fails	
run_option('Save session'):-
        tcl_GUI(on).        
% Runs old answers and a session from file and makes a list of both. 
run_option('Fetch old answers') :-
        \+tcl_GUI(on),
		retractall(answer(_,_)),
        retractall(trigged(_,_,_)),
        retractall(how(_,_,_,_)),
        read_answerfile,
        write('Given answers'),nl,
        listing(answer).
run_option('Fetch old answers') :-
        tcl_GUI(on),
        read_answerfile,!,
		setof((Attribute,Value), Value^answer(Attribute,  Value), List_of_answers),
        assert_each_option(List_of_answers),
        assert_num_options(List_of_answers),
		Source = 'source k:/klöver/show_old.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event).
%if user choose to not open new file (i.e. read_answers fails)			
run_option('Fetch old answers') :-
		tcl_GUI(on).	
run_option('Fetch old session') :-
		\+tcl_GUI(on),
        retractall(answer(_,_)),
        retractall(trigged(_,_,_)),
        retractall(how(_,_,_,_)),
        read_answerfile,
        run_option('List database').		
run_option('Fetch old session') :-
		tcl_GUI(on),
        read_answerfile,!,
        run_option('List database').
run_option('Fetch old session') :-
		tcl_GUI(on).		
% Lists database content.
run_option('List database') :- 
        \+tcl_GUI(on),
		delete_garbage,
        nl,
        write('Given answers'), nl,
        (listing(answer); write('Exists no')), nl,
        write('Conclusions'), nl, 
        (listing(trigged); write('Exists no')), nl,
        write('The result of all rules'), nl,
        (listing(how); write('Exists no')).
%Assert database and display it in tcl file
run_option('List database') :- 
        tcl_GUI(on),
		delete_garbage,
		(
		setof((Attribute,Value), Value^answer(Attribute, Value), List_of_answers),
		assert_each_option(List_of_answers),
        assert_num_options(List_of_answers);
		assert(answer_options(('No answers exists',_)))),
		(
        setof(trigged(Object,Attribute,Value), trigged(Object,Attribute,Value), List_of_trigged),
		assert_each_option(List_of_trigged),
        assert_num_options(List_of_trigged);
		assert(answer_options(trigged('No conclusions drawn',_,_)))),
		(
		setof(how(FR,Object,Attribute,TF), how(FR,Object,Attribute,TF), List_of_how),
		assert_each_option(List_of_how),
        assert_num_options(List_of_how);
		assert(answer_options(how(_,'No how explanations exists',_,_)))),
		Source = 'source k:/klöver/show_result.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event).
	
 % Verification of rule-base:
 run_option('Verify rules'):-
		\+tcl_GUI(on),
		nl,
		write('*************************'),nl,
		write('VERIFICATION TOOL MENU'),nl,nl,
		write('Check:'), nl,nl,
		menue_write(['Redundancy','Subsumed',
             'Completeness', 'Help','Back']),
        read(Input),
		go_verify(Input).		
run_option('Verify rules'):-
		tcl_GUI(on),
		Source = 'source k:/klöver/verify.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event),
		go_verify(Event).
		
run_option('How explanation'):-
        answer_howquestion.

run_option('Interrupt'):-
        \+tcl_GUI(on),
		abort.
		
run_option('Interrupt'):-
        tcl_GUI(on),
		tcl(Tcl),
		tcl_delete(Tcl),
		abort.
		
%Case: Tcl GUI closed down by user - then Klöver stops.		
run_option([]):-  
		tcl_GUI(on),
		abort.

% Terminates Options.
run_option('Quit'):-
		\+tcl_GUI(on),
        abort.
		
run_option('Quit'):-
		tcl_GUI(on),
		tcl(Tcl),
		tcl_delete(Tcl),
        abort.

run_option(Answer):-
        write('That option does not exist, please try another one! '), nl.


read_input(List_of_answers, Answer):-
        write('Which answers do you want to change? Select the object, please! '),
        write('Please insert an object and finish the insertion with the word stop. '), nl,
        read_all_answers(_,Answer, start).

write_no_answers:-
        write('There are no answers!'),!, nl.

delete_answers([]).
delete_answers([stop]):- \+tcl_GUI(on).
delete_answers([Answer| Rest]):-
        retractall(answer(Answer,_)),
        delete_answers(Rest).

delete_garbage:-
        retractall(answer),
        retractall(trigged),
        retractall(how).

% ***************************************************
% ********** Predicates called by 'Options' *********

% Makes a consultation with or without questions.
consultation:-
        retractall(trigged(_,_,_)),
        retractall(how(_,_,_,_)),
        create_knowledgebase,                 % question generator
        call_goal_parameters,                   % examines all goal parameters
        presentation_result.             % presents the result
changed_consultation:-
        retractall(trigged(_,_,_)),
        retractall(how(_,_,_,_)),
        create_knowledgebase,                % question generatoer
        call_goal_parameters.                   % examines all goal parameters

% Reads a file with answers and gives possibilities to change answers.

% Saves answers and complete session on file.
store_answers:-
		\+tcl_GUI(on),
        write('Give a file name: '),
        read(Filename),
        tell(Filename),
        write(':-dynamic answer/2.'),nl,        
        listing(answer),nl,
        told.
%Built-in function in tcl/tk tk_getSaveFile that create a 
%window where the user can choose where to save a file on computer		
store_answers:-
		tcl_GUI(on),
		%Source = 'source k:/klöver/fetchfile.tcl',
		%run_tcl(Source, Filename),
		%retract_and_destroy(Filename),
		tcl(Tcl),
		tcl_eval(Tcl,['tk_getSaveFile -defaultextension ".pl" -filetypes {{{Perl Files} {.pl} TEXT} {{Text Files} {.txt}  TEXT} {{Text Files} {""} TEXT}}'],File),
		(
		File == [],!,
		fail
		;
		atom_codes(Filename, File),
        tell(Filename),
        write(':-dynamic answer/2.'),nl,        
        listing(answer),nl,
        told).
		%tcl_delete(Tcl).
		
store_session:-
		\+tcl_GUI(on),
        write('Give a file name: '),
        read(Filename),
        tell(Filename),
        write(':-dynamic how/4,answer/2,trigged/3.'),nl,        
        listing(answer),
        listing(trigged),
        listing(how),nl,
        told.
%Built-in function in tcl/tk tk_getSaveFile that create a 
%window where the user can choose where to save a file on computer			
store_session:-
        tcl_GUI(on),
		%Source = 'source k:/klöver/fetchfile.tcl',
		%run_tcl(Source, Filename),
		%retract_and_destroy(Filename),
        tcl(Tcl),
		tcl_eval(Tcl,['tk_getSaveFile -defaultextension ".pl" -filetypes {{{Perl Files} {.pl} TEXT} {{Text Files} {.txt}  TEXT} {{Text Files} {""} TEXT}}'],File),
		(
		File == [],!,
		fail
		;
		atom_codes(Filename, File),
		tell(Filename),
        write(':-dynamic how/4,answer/2,trigged/3.'),nl,        
        listing(answer),
        listing(trigged),
        listing(how),nl,
        told).
		%tcl_delete(Tcl).
		
read_answerfile:-
		\+tcl_GUI(on),
        write('The name of the file is: '),
        read(Filename),
        consult(Filename).
%Built-in function in tcl/tk tk_getOpenFile that create a 
%window where the user can choose a file on computer			
read_answerfile:-
        tcl_GUI(on),
		tcl(Tcl),
		tcl_eval(Tcl,['tk_getOpenFile -filetypes {{{Perl Files} {.pl} TEXT} {{Text Files} {.txt}  TEXT} {{Text Files} {""} TEXT}}'],File),
		(
		File == [],!,
		fail
		;
		atom_codes(Filename, File),
		retractall(answer(_,_)),
        retractall(trigged(_,_,_)),
        retractall(how(_,_,_,_)),
		%Source = 'source k:/klöver/fetchfile.tcl',
		%run_tcl(Source, Filename),
		%retract_and_destroy(Filename),
		consult(Filename)).
		
% Fetches list of goal parameters from KB and checks them.
call_goal_parameters:-
                goal_conclusions(List),     
                check_all(List).         

% Conditionless examination of all goal paramameters.
check_all([]).
check_all([Parameter|Rest]):-
                check(Parameter,_, goal_parameter,_),  
                check_all(Rest).
check_all([Parameter|Rest]):-
                assert(trigged(Parameter, Attribute, 0)),  
                check_all(Rest).        

% ***************************************************
% General predicates used for tcl_tk GUI

% abort tcl and sicstus if user closed the tcl/tk window
tcl_abort_case([]):-
		tcl(Tcl),
		tcl_delete(Tcl),
		abort,!.
tcl_abort_case(Var).

% load a new tcl window, and then waits for a user-event from tcl side
run_tcl(Source, Event):-
	tcl(Tcl),
	tcl_eval(Tcl,Source,_),
	tk_next_event(Tcl, Event).

%retract asserted facts, and remove tcl window 	
retract_and_destroy(Event):-
		tcl(Tcl),
		delete_tcl_facts,
		tcl_abort_case(Event),!,
		tcl_eval(Tcl,'destroy .t',_).

%total clean-up of asserted data used for tcl GUI		
delete_all_tcl:-
	retractall(answer_options(_)),
	retractall(num_answers(_)),
	retractall(tcl_question(_,_,_,_,_)),
	retractall(attr_cf(_,_,_)),
	retractall(num_attr_cf(_,_)).
	
delete_tcl_facts:-
	retractall(answer_options(_)),
	retractall(num_answers(_)),
	retractall(tcl_question(_,_,_,_,_)).
	
%converts a string to an integer
string_to_integer(Ans, Int):-
	atom_codes(Ans,ASCII_List),!, 
	name(Int,ASCII_List).

%A list in tcl is a string where elements are separated by empty space
%This predicate transforms the string to a list of ascii-numbers	
tcl_list_to_prolog_list(Atom,Answers):-
	atom_codes(Atom, ASCII),
	remove_spaces(ASCII, Answers).

%create new ascii-list without empty spaces (ascii nr 32) 	
remove_spaces([],[]).
remove_spaces([32|List], Rest):-
	remove_spaces(List,Rest).
remove_spaces([E|List], [E|Rest]):-
	remove_spaces(List,Rest).

%assert_each_option/1: assert each element in list 
%asserted facts are used by tcl_tk to retrieve answers from Prolog without deleting original answer.
assert_each_option([]).
assert_each_option([E|Rest]):-		
			assert(answer_options(E)), 
			assert_each_option(Rest).

%assert_num_options(+AnswerList): count elems in list and assert number
%this fact is used by tcl_tk 
assert_num_options(X):-
	count_elems(X,Num),!,
	assert(num_answers(Num)).

%retract_predicate/2 is needed to be able to retract temporarily facts in klöver from tcl_tk side
retract_answer(Num,M):-
	retract(answer_options(_)).
	
retract_number:-
	retract(num_answers(_)).
	
% For TCL: Assert all conclusions set to 'true' which means  
% they have propability > 100
assert_attr_list(_,[]).		
assert_attr_list(Object,[(Attribute, Value)|Rest]):-
        tcl_GUI(on),
		assert(attr_cf(Object,Attribute, Value)),
        assert_attr_list(Object,Rest).
		
retract_attr_list(Object):-
	retract(attr_cf(Object,_,_)).

assert_num_attributes(Object, AttrList):-	
		count_elems(AttrList,Num),!,
		assert(num_attr_cf(Object,Num)).

%get_single_answer/3 
%retrieve the element on "index place" in a list.
get_single_answer(0, [Ans|_], Ans).
get_single_answer(Num, [E|Rest], Ans):-
		Num1 is Num - 1,
		get_single_answer(Num1, Rest, Ans).

%get_answer_list/3 convert each ASCII-nr to integer, retrieve and store each object in new list 
get_answer_list(_, [], []).
get_answer_list(Options, [ASCII|Rest], [A|List]):-
	name(Int,[ASCII]),
	get_single_answer(Int, Options, A),
	get_answer_list(Options, Rest, List).

convert_to_objects([], []).
convert_to_objects([(A,V)|Rest], [A|New]):-
		convert_to_objects(Rest, New).
	
% **************************************************
% ************* Inference mechanism *****************


% Checks the control of all conditions in the knowledge base:
% (Not system related conditions.)

% when given answer exists
check(Object, Condition, Value):-
        answer(Object,Answer),
        check_condition(Object, Condition, Value).

% when the value is set by rules
check(Object, Attribute, Condition, CF):-
        trigged(Object, Attribute, CF2),
        check_condition(Object, Attribute, Condition, CF).

% Checks true and false on conclusions set by rules.
check(Object, Attribute, Truth_value) :-        
        (Truth_value = sant; Truth_value = falskt),
        trigged(Object, Attribute, CF),
        check_condition(Object, Attribute, Truth_value).

% Asks questions that have not been asked by the question generator and possible 
%follow-up questions. Main question conditionless and follow-up questions with conditions.

check(Object,Condition, Value):-
		tcl_GUI(on),
         \+ answer(Object,_),
        question_info(Object, Prompt, Meny, Type, Fragevillkor ),
                question_condition(Fragevillkor,Back),
		%assert(tcl_question(Object, Prompt, Meny, Type, _)),
        type_control(Type, Object),
		%retract(tcl_question(Object, Prompt, Meny, Type, _)),
		((question_sequence(Object, Foljdfragelista),
        ask_user(Foljdfragelista)) ; true), !,
        check_condition(Object, Condition, Value).
		
check(Object,Condition, Value):-
		\+ tcl_GUI(on),
         \+ answer(Object,_),
        question_info(Object, Prompt, Meny, Type, Fragevillkor ),nl,
                question_condition(Fragevillkor,Back),
        write(Object),nl,
        write(Prompt),nl,
        menue_write(Meny),nl,
        write('Enter Why if you want a text based explanation. Then answer the question!'),nl,nl,
        type_control(Type, Object),
        ((question_sequence(Object, Foljdfragelista),
        ask_user(Foljdfragelista)) ; true), !,
        check_condition(Object, Condition, Value).

% Goes through all rules for objects that can be determined by rules but have yet not
% been evalutated.
check(Object,  Attribute, Condition, Value):-
        \+ how(_,Object, Attribute, _),
        rule_info(Object, Rule_numbers),
        solve_all(Object, Rule_numbers,Partial_answer, Answer),!,
        check_condition(Object, Attribute,  Condition, Value).

% Checks rules for conditions with true and false.
check(Object,  Attribute,  Truth_value):-
        \+ how(_,Object, Attribute, _),
        rule_info(Object, Rule_numbers),
        solve_all(Object, Rule_numbers,Partial_answer, Answer),!,
        check_condition(Object, Attribute, Truth_value).


% No more rules for the object.
solve_all(Object, [], Answer, Answer).
%Ruels that set an object-attribute combination to a value
% that has proven completely true or completely false do not affect the truth value.
solve_all(Object, [FR|RR], Partial_answer, Answer):-
        (trigged(Object, Attribute, 1000);
        trigged(Object, Attribute, -1000)),
        clause(rule(FR,Object, Attribute, CF), Condition) ,
        solve_all(Object, RR, Partial_answer, Answer).
% Examines the first rule in the rules list, if succseeded CF is updated and 
% we move on to the rest of the list.
solve_all(Object, [FR|RR], Partial_answer, Answer):-
        rule(FR,Object, Attribute, CF),
         update_CF(FR,Object, Attribute, CF, Partial_answer, New_partial_answer),
        solve_all(Object, RR, New_partial_answer, Answer).
% If the rule failes the result is stored as rule rejected.
% this in order to inform that the object has been examined even if no rule succeeded.
solve_all(Object, [FR|RR], Partial_answer,Answer):-
        clause(rule(FR,Object,Attribute,CF),Premises), % to bind the attribute
        asserta(how(FR,Object,Attribute,rejected)), 
        solve_all(Object, RR, Partial_answer, Answer).
		
solve_all(Object, [FR|RR], Partial_answer,Answer):-
		\+ rule(FR,Object,_,_),
		solve_all(Object,RR,Partial_answer,Answer).

% check_condition controls that the condition in the check really is fullfilled 
% by the value that the object and attribute has been alocated.

% All goal parameters are to be checked.
check_condition(Object,_, goal_parameter,_).

% The followin is applicated on user given answers.
check_condition(Object, = , Value):-
        !,
        answer(Object,Value).
check_condition(Object, >=, Value1):-
        answer(Object, Value2), 
        number(Value2),
        Value2 >= Value1.
check_condition(Object,>,Value1):-
        answer(Object,Value2),
        number(Value2),
        Value2 > Value1.
check_condition(Object,=<,Value1):-
        answer(Object,Value2),
        number(Value2),
        Value2 =< Value1.
check_condition(Object,<, Value1):-
        answer(Object, Value2), 
        number(Value2),
        Value2 < Value1.
check_condition(Object,'<>',Value):-
        \+ answer(Object,Value).

% The followeing is applicated on conclusions drawn by the system.
% The following two cases are applied on conclusions and will compare their 
% certainty factors with a specific value.
check_condition(Object, Attribute,   'cf>=', CF1):-
        trigged(Object,Attribute,CF2),!,
        number(CF2),   
        CF2 >= CF1.
check_condition(Object, Attribute,   'cf<' , CF1):-
        trigged(Object,Attribute,CF2),!,
        number(CF2),
        CF2 < CF1.

% The following three will check the conclusions drawn by numeric rules for example  
% belopp_guld and compares given value with another value.
check_condition(Object, Attribute,   'Value>=', Value):-
        trigged(Object,Attribute,CF),        
        number(Attribute),
        Attribute >= Value.
check_condition(Object, Attribute,   'Value<', Value):-
        trigged(Object,Attribute,CF),
        number(Attribute),
        Attribute < Value.
check_condition(Object, Attribute,  'Value=', Value):-
        trigged(Object,Attribute,CF),
        number(Attribute),
        Attribute = Value.
% To make is easier to check if conclusions are completely true or false. 
check_condition(Object, Attribute, sant):-
        trigged(Object,Attribute,1000).
check_condition(Object, Attribute, falskt):-
        trigged(Object,Attribute,-1000).
% Takes care of conditions on the form "least n of the following" for conclusions.
check_condition(Object, Less_num,outof ,Attribute_list):-
        count_attribute(Object,Attribute_list,Num_in_list), 
        Num_in_list >= Less_num.
% Takes care of conditions on the form "least n of the following" for user given info.
check_condition(Object, Less_num, Attribute_list):-
        count_attribute(Object,Attribute_list,Num_in_list), 
        Num_in_list >= Less_num.

% Calculates number of attributes for an object that exist in a list.
% Called by check_condition for that type of condition.
count_attribute(Object,[],0).
count_attribute(Object,[Attribute| Rest],Number):-
        (check(Object, =, Attribute) ;
         check(Object,Attribute, 'cf>=',1000)),!,
        count_attribute(Object,Rest,Number1),
        Number is Number1 + 1.
count_attribute(Object,[Attribute|Rest],Number):-
        count_attribute(Object,Rest,Number).

% ***********************************************
% *********** Certainity handling ***************

% Saves CF for an attribute for the first time.
update_CF(Regelnr, Object, Attribute, CF, [], [(Attribute, CF)]):-
        asserta(trigged(Object, Attribute, CF)),
        asserta(how(Regelnr, Object, Attribute, CF)). 
% Keeps the rule that has been shown 
% and its CF for use with "how"-questions
% The attribute has not been shown by any former rule because it does not exist in the
% list Partial_answer. Therefore given CF for the attribute is saved.
update_CF(Regelnr,Object, Attribute, CF, Partial_answer,[(Attribute, CF)|Partial_answer] ):-
        not_memb((Attribute, X), Partial_answer),
        asserta(trigged(Object, Attribute, CF)),
        asserta(how(Regelnr, Object, Attribute, CF)).


% Attribute proven completely true or completely false before.
% Possible new rules do not affect this.
update_CF(Regelnr, Object, Attribute, CF, Partial_answer, Partial_answer ):-
        memb((Attribute, X), Partial_answer),
        (trigged(Object, Attribute, 1000);
                trigged(Object, Attribute, -1000)),!.

% Attribute given CF before. New CF is always to be 
% calculated as a contexture of the new and the old value.
update_CF(Regelnr, Object, Attribute, CF, Partial_answer, New_partial_answer ):-
        memb((Attribute, X), Partial_answer),
        calculate_CF((Attribute, CF), Partial_answer, New_partial_answer, New_CF),
        retract(trigged(Object, Attribute, Old_CF)),  % earlier CF deleted
        asserta(trigged(Object, Attribute, New_CF)), % combined CF lagras
        asserta(how(Regelnr, Object, Attribute, New_CF)). % info for "how"-questions%

% Calculates CF as a contexture of old and new value.
% All attributes and their CF are stored in a list as arguments.
% to the predcate. Current predicate first in list.
calculate_CF((Attribute, CF), [(Attribute, Old_CF)|Partial_answer],
           [(Attribute, New_CF)|Partial_answer], New_CF):-
        cf(CF, Old_CF, New_CF).

% Current predicate not first in list. 
% Continue to go through list Partial_answer to find it.
calculate_CF((Attribute, CF), [(X, Y)|Partial_answer], [(X,Y) | New_partial_answer], New_CF):-
        Attribute \== X,
        calculate_CF((Attribute, CF), Partial_answer, New_partial_answer, New_CF).

% Contexture of CF. Both new and old value for CF < 0.
cf(CF, Old_CF, New_CF):-
        Old_CF < 0, CF < 0, !,
        New_CF is Old_CF + ((CF * (1000 + Old_CF )) // 1000).
% Both new and old value for CF >= 0.
cf(CF, Old_CF, New_CF):-
        Old_CF >= 0, CF >= 0, !,
        New_CF is Old_CF + (( CF * (1000 - Old_CF)) // 1000).
% One of the values for old or new CF < 0.
cf(CF, Old_CF, New_CF):-
        min(CF, Old_CF, Min_CF),
        New_CF is (((CF + Old_CF) * 1000)// (1000 - Min_CF)).

% Minimum of two values for CF.
min(CF1, CF2, ACF1):-
        abs_value(CF1,ACF1),
        abs_value(CF2,ACF2),
        ACF1 < ACF2.
min(CF1, CF2, ACF2):-
        abs_value(CF1,ACF1),
        abs_value(CF2,ACF2),
        ACF2 =< ACF1.

abs_value(CF,CF):-
        CF >= 0.
abs_value(CF1,CF2):-
        CF1 < 0,
        CF2 is -1*CF1.

% *************************************************
% ************* Various predicates ****************

not_memb((Attribute, X), Partial_answer):- 
        memb((Attribute, X), Partial_answer),!,
        fail.

not_memb((Attribute, X), Partial_answer).

equal(Attribute,Solution):-
        Attribute == Solution.

memb(Elem,[Elem | List]):- !.
memb(Elem,[X | List]):-
        memb(Elem,List).

'<ERROR>'(2) :- !, fail.

clean:-
        retractall(answer(_,_)),
        retractall(trigged(_,_,_)).

%*****************************************************************        
%*****************************************************************        
% Window 2 - Generates questions


% **************************************************
% ************* Question generator *****************

:-dynamic goal_temp/1,how/4,question_temp/1,menu_temp/1,menu_temp/2,ok/1,redo/1,answer/2,temp/1,text/2,trigged/3.

% Examines all introducing questions.
create_knowledgebase:-
        question_order(List),
        ask_user(List).

% When going backwards in questioning or with "redo" as condition on a question.
create_database:-
        redo(true),
        retractall(redo(true)),
        create_database.

% Examins wether a question is to be asked and then asks the question.
ask_user([]).
ask_user([Object| Rest]):-
        answer(Object,Answer),!,
        ask_user(Rest).
%GUI deactivated
ask_user([Object| Rest]):-
        \+tcl_GUI(on),
		question_info(Object,Prompt,Meny,Type,Condition),
        question_condition(Condition,Back),
        write(Object), nl,
        write(Prompt),nl,
        menue_write(Meny),nl,
        write('Print Why if you want an explanation. Then answer the question!'),nl,nl,
        type_control(Type, Object),
        back_check(Back),!,
        ask_user(Rest).
%GUI activated		
ask_user([Object|Rest]):-
		tcl_GUI(on),
        question_info(Object, Prompt, Meny, Type, Condition),
        question_condition(Condition,Back),
		%assert(tcl_question(Object, Prompt, Meny, Type, _)),
        type_control(Type, Object),
		%retract(tcl_question(Object, Prompt, Meny, Type, _)),
		back_check(Back),!,
        ask_user(Rest).
%GUI activated
% single answer questions
type_control(s, Object):-
        tcl_GUI(on),
		question_info(Object,Prompt,Options,Type,Condition),
		assert(tcl_question(Object, Prompt, Options, Type, _)),
		assert_each_option(Options),
		assert_num_options(Options),
		Source = 'source k:/klöver/s_questions.tcl',
		run_tcl(Source, Meny_Index),
		retract_and_destroy(Meny_Index),
		(
		Meny_Index = change_answers,
		run_option('Change answers'),
		changed_consultation;
		%type_control(s, Object);
		get_single_answer(Meny_Index, Options, Answer),
		arrange_answer(Object,Answer)).
%GUI activated
% w-questions where user write an arbitrary answer
type_control(w, Object):-
		tcl_GUI(on),
		question_info(Object,Prompt,Options,Type,Condition),
		assert(tcl_question(Object, Prompt, Options, Type, _)),
		Source = 'source k:/klöver/w_questions.tcl',
		run_tcl(Source, Answer),
		retract_and_destroy(Answer),
		(
		Answer = change_answers,
		run_option('Change answers'),
		changed_consultation;
		%type_control(w, Object);
		(
		string_to_integer(Answer, Int),
		arrange_answer(Object,Int);
		arrange_answer(Object, Answer))).	
%GUI activated
% multiple answers questions
type_control(m, Object):-
		tcl_GUI(on),
        question_info(Object,Prompt,Options,Type,Condition),
		assert(tcl_question(Object, Prompt, Options, Type, _)),
		assert_each_option(Options),
		assert_num_options(Options),
		Source = 'source k:/klöver/m_questions.tcl',
		run_tcl(Source,Event),
		retract_and_destroy(Event),
		(
		Event = change_answers,
		run_option('Change answers'),
		changed_consultation;
		%type_control(m, Object);
		tcl_list_to_prolog_list(Event,ASCII_Ans),
		get_answer_list(Options, ASCII_Ans, Answers), 
        store_one_answer(Object, Answers)).

%GUI deactivated
type_control(X, Object):-
		\+tcl_GUI(on),
        (X == s ; X == w),
        read(Answer),nl,
        arrange_answer(Object,Answer).
%GUI deactivated
type_control(Type, Object):-
		\+tcl_GUI(on),
        write('This question can be answered with multiple answers! '),
        nl,
        write('Please insert an answer and finish the insertion with the word stop. '), nl,
        read(Ans),
        read_all_answers(Object,Answers, Ans),
        store_one_answer(Object, Answers).

	
read_all_answers(Object,Answers,'Why'):-
        why_explanation(Object),!,
        read(Ans),
        read_all_answers(Object,Answers,Ans).
read_all_answers(Object,[], stop):- \+tcl_GUI(on), !.
read_all_answers(Object,[Ans|Answer], Ans):-
        read(New_answer),
        read_all_answers(Object,Answer, New_answer).

% When going backwards it will fail and it starts from the beginning.
ask_user(_):-
        redo(true),!,
        fail.
ask_user([Object| Rest]):-
        ask_user(Rest).

arrange_answer(Object,'Why'):-
        !,why_explanation(Object),
        read(New_answer),
        arrange_answer(Object,New_answer).
arrange_answer(Object,Answer):-
        store_one_answer(Object, Answer).

% Saves user-given answers as separate relations.
store_one_answer(Object, []).
store_one_answer(Object, [stop]).
store_one_answer(Object, [First|Rest]) :-
        asserta(answer(Object, First)),
        store_one_answer(Object, Rest).
store_one_answer(Object, Answer):-
        asserta(answer(Object, Answer)).

% When going backwards in a dialogue or with "redo" as question condition.
back_check(t):-
        asserta(redo(true)),!,
        fail.
back_check(f).

% Takes back all conclusions and makes new ones when going backwards in the dialogue.
revise_ev_concl:-
        ((trigged(_,_,_), retractall(trigged(_,_,_))) ;
        true),
        ((how(_,_,_,_) , retractall(how(_,_,_,_))) ;
        true),
        goal_conclusions(Goal_param_list),
        check_all(Goal_param_list).

% Conditions that can be put on questions in the question generator.
% "redo" makes back true and  back_check 
% will assert redo in the databasen and the complete fraga_ev_anvandaren will fail
% and the whole question base will be examined again, beautiful dont you think? Ever seen it before?
question_condition([redo],t).
% no conditions
question_condition([],f).
% not-condition
question_condition([not(Condition)| Rest], Go_back):-
        \+ single_condition(Condition),
        question_condition(Rest, Go_back).
% or-condition
question_condition([or(Condition1,Condition2) | Rest],Go_back):-
        or_condition([Condition1,Condition2]),
        question_condition(Rest,Go_back).
% Only single questions. (No composed questions.)
question_condition([Condition | Rest],Go_back):-
        single_condition(Condition),
        question_condition(Rest, Go_back).

% Control of single question conditions.
or_condition([Condition | Rest]):-
        question_condition([Condition],_).

or_condition([Condition | Rest]):-
        or_condition(Rest).

single_condition((Object = Answer)):-
        answer(Object,Argument),
	equal(Argument,Answer).

single_condition((Object > Value)):-
        answer(Object, Answer),
        Answer > Value.

single_condition((Object < Value)):-
        answer(Object, Answer),
        Answer < Value.

single_condition((Object >= Value)):-
        answer(Object, Answer),
        Answer >= Value.

single_condition((Object =< Value)):-
        answer(Object, Answer),
        Answer =< Value.

single_condition((Object = Value)):-
        answer(Object, Answer),
        Answer == Value.

single_condition((Object > Value)):-
        answer(Object, Answer),
        Answer > Value.

%*****************************************************************        
%*****************************************************************        
% Window 3 - Question base



%************************INFO ABOUT QUESTIONS**********************
% question_info/5.


% question_info/5.
question_info(type_problem,'Which category does your problem belong to?',
	[value,webpage],s,[]).

question_info(first_level_value,'What is the issue related to?',
	['Value is missing','Negative or Zero Values reported','Value exceeds the range',none],s,[type_problem=value]).
	
question_info(second_level_value,'What is the issue related to?',
	['Sensor not functional','Error in sensor Configurations','None of the above'],s,[type_problem=value]).
	
question_info(third_level_value,'What is the issue related to?',
	['Problems with I/O readings observed with Maintenance software','Other Problems with I/O','None of the above'],s,[type_problem=value]).
	
question_info(fourth_level_value,'What is the issue related to?',
	['I/O Pingable','I/O not Pingable'],s,[third_level_value='Other Problems with I/O']).
	
question_info(fifth_level_value,'What is the issue related to?',
	['Problem with local inputs in Multimeter','Problem with Linux Firewall Auditing issue','Empty input at I/o reading observed with manufacturer software','None of the above'],s,[type_problem=value]).
	
question_info(seventh_level_value,'What is the issue related to?',
	['Problem with network settings or V-Lan configurations','Incorrect values observed in Barrier devices','None of the above'],s,[type_problem=value]).
	
question_info(eighth_level_value,'What is the issue related to?',
	['Problem with Analyser in the field','Cable problem with the LAN network','None of the above'],s,[type_problem=value]).

question_info(ninth_level_value,'Choose the appropriate?',
	['The is output ok',' Not Ok','Others'],s,[eighth_level_value='Problem with Analyser in the field']).
	
question_info(tenth_level_value,'What is the issue related to?',
	['Cabinet Power issues','Others','No issue'],s,[type_problem=value]).

question_info(eleventh_level_value,'What is the issue related to?',
	['Cabinet Power is off','Internal fuse is broken','Internal fuse works'],s,[tenth_level_value='Cabinet Power issues']).
	
question_info(twelfth_level_value,'Choose the appropriate?',
	['I/o gives readings in Local laptop','I/o gives incorrect readings in Local laptop'],s,[eleventh_level_value='Internal fuse works']).
	
	


% webpage

question_info(first_level_webpage,'What is the issue related to?',
     	 ['Webpage cannot be accessed internally','Problem with Virtual Machine',none],s,[type_problem=webpage]).

question_info(second_level_webpage,'What is the issue related to?',
      	['The Firewall settings are out of line','Problem with Apache Server','The Network settings are out of line',none],s,[type_problem=webpage,not(first_level_webpage=none)]).

question_info(third_level_webpage,'What is the issue related to?',
                  ['Problem with IP','Problem with Linux - firewall','Problem with Linux - Others', none],s,[type_problem=webpage, not(second_level_webpage='Problem with Apache Server'), not(first_level_webpage=none)]).

question_info(fourth_level_webpage,'What is the issue related to?',
                 ['Server configuration is offset','Storage persists','Server is turned off',others],s,[second_level_webpage='Problem with Apache Server']).

question_order([type_problem]).

question_sequence(_, []).




definition(first_level_webpage,'State which problem you are having.').
definition(second_level_webpage,'State which problem you are having.').
definition(third_level_webpage,'State which problem you are having.').
definition(fourth_level_webpage,'State which problem you are having.').

definition(first_level_value,'State which problem you are having.').
definition(second_level_value,'State which problem you are having.').
definition(third_level_value,'State which problem you are having.').
definition(fourth_level_value,'State which problem you are having.').
definition(fifth_level_value,'State which problem you are having.').
definition(sixth_level_value,'State which problem you are having.').
definition(seventh_level_value,'State which problem you are having.').
definition(eighth_level_value,'State which problem you are having.').
definition(ninth_level_value,'State which problem you are having.').
definition(tenth_level_value,'State which problem you are having.').
definition(eleventh_level_value,'State which problem you are having.').
definition(twelfth_level_value,'State which problem you are having.').
%*****************************************************************        
%*****************************************************************        
% Window 4 - Information about rules

% goal_conclusions/1.

goal_conclusions([filw,slw,tlw,flw,flv,sclv,tlv,folv,filv,selv,elv,nlv,telv,elelv,twelv,
		solutions_for_webpage_errors,solutions_for_value_errors]).

%fix_server_congiguration,clear_database,turn_on_server,fix_firewall,virtual_machine,contact_IP,fix_network,fix_linux,maintenance,

%change_ip_and_reconfigure_data_logger,enable_sensors,fix_firewall_audit_settings,fix_vlan_network_settings,fix_cabinet_power,fix_internal_fuse,reset_reconfigure_IO,fix_configuration,reset_reconfigure_datalogger,use_spare_IO,fix_barrier_device_readings,use_spare_cables,fix_power_issues

% to_present/1.

to_present([solutions_for_webpage_errors,solutions_for_value_errors]).

% information about conclusions drawn by rules.

% rule_info/2.

rule_info(filw,[30,31,32,33,34,35,36,37,38]).
rule_info(slw,[40,41,42,43,44,45,46,47,48]).
rule_info(tlw,[50,51,52,53,54,55,56]).
rule_info(flw,[70,71,72]).

rule_info(flv,[200,202,203,204,205,206,801,802,803,804,805,806,807]).
rule_info(sclv,[210,212,213,214,215,216,811,812,813,814,815,816,817]).
rule_info(tlv,[220,222,223,224,225,226,821,822,823,824,825,826,827]).
rule_info(folv,[230,231,232,233,831]).
rule_info(filv,[240,242,243,244,245,246,841,842,843,844,845,846,847]).
rule_info(selv,[260,262,263,264,265,266,851,852,853,854,855,856,857]).
rule_info(elv,[270,272,273,274,275,276,861,862,863,864,865,866,867]).
rule_info(nlv,[976,977]).
rule_info(telv,[290,870,871,872,873,874,875,876,877,878]).
rule_info(elelv,[295,880,991]).
rule_info(twelv,[992]).


% changed rule_info


%rule_info(fix_server_congiguration,[500,501]).
%rule_info(clear_database,[510,511]).
%rule_info(turn_on_server,[520,521]).
%rule_info(fix_firewall,[530,531,532]).
%rule_info(virtual_machine,[540,541,542]).
%rule_info(contact_IP,[550,551,552]).
%rule_info(fix_network,[560,561,562]).
%rule_info(fix_linux,[570,571,572]).
%rule_info(maintenance,[580,581,582,583,584,585,586,587,588,589,590,591,592,593]).

rule_info(solutions_for_webpage_errors,[500,501,510,511,520,521,530,531,532,540,541,542,
	550,551,552,560,561,562,570,571,572,580,581,582,583,585]).

%rule_info(change_ip_and_reconfigure_data_logger,[300,301]).
%rule_info(enable_sensors,[304,305]).
%rule_info(fix_firewall_audit_settings,[306,307]).
%rule_info(fix_vlan_network_settings,[308,309]).
%rule_info(fix_cabinet_power,[310,311]).

%rule_info(fix_internal_fuse,[312,313]).
%rule_info(reset_reconfigure_IO,[1001,1002]).
%rule_info(fix_configuration,[1003,1004]).

%rule_info(reset_reconfigure_datalogger,[1005,1006]).
%rule_info(use_spare_IO,[1007,1008]).
%rule_info(fix_barrier_device_readings,[1009,1010]).
%rule_info(use_spare_cables,[1011,1012]).
%rule_info(fix_power_issues,[1013,1014]).


rule_info(solutions_for_value_errors,[300,301,304,305,306,307,308,309,310,311,312,313,
	1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,2000]).



%*****************************************************************        
%*****************************************************************        
% Window 5 - Rules base


% rule/4.
:- dynamic rule/4.  %To be able to use clause on the rule.


%levels for webpage - _lw

% First level for webpage

%rules about fix firewall first level
rule(30,filw,fix_firewall,500):- 
    check(first_level_webpage,=,'Webpage cannot be accessed internally').

%rules about turn on virtual machine first level
rule(31,filw,virtual_machine,500):- 
    check(first_level_webpage,=,'Problem with Virtual Machine'). 

%rules about contacting internet provider first level
rule(32,filw,contact_IP,300):- 
    check(first_level_webpage,=,'Webpage cannot be accessed internally').

%rules about fixing network settings first level
rule(33,filw,fix_network,200):-
    check(first_level_webpage,=,'Problem with Virtual Machine').

%rules about fixing linux server first level
rule(34,filw,fix_linux,200):-
    check(first_level_webpage,=,'Problem with Virtual Machine').

%rules about contact hssd first level
rule(35,filw,maintenance,1000):- 
    check(first_level_webpage,=,none). 

%first level of fix server config
rule(36,filw,fix_server_congiguration,400):-
	check(first_level_webpage,=,'Problem with Virtual Machine').

%first level of clear database
rule(37,filw,clear_database,400):-
	check(first_level_webpage,=,'Problem with Virtual Machine').

%first level of turn on the server
rule(38,filw,turn_on_server,400):-
	check(first_level_webpage,=,'Problem with Virtual Machine').



%Second level for webpage

%Rules about fix firewall second level
rule(40,slw,fix_firewall,400):- 
    check(second_level_webpage,=,none).

%Rules about turn on virtual_machine second level
rule(41,slw,virtual_machine,400):- 
    check(second_level_webpage,=,none).

%Rules about contacting internet provider second level
rule(42,slw,contact_IP,300):- 
    check(second_level_webpage,=,'The Firewall settings are out of line'). 

%Rules about fixing network settings second level
rule(43,slw,fix_network,300):- 
    check(second_level_webpage,=,'The Network settings are out of line'). 

%Rules about fixing linux server second level
rule(44,slw,fix_linux,300):- 
    check(second_level_webpage,=,'The Network settings are out of line'). 

%rules about contact hssd second level
rule(45,slw,maintenance,400):- 
    check(second_level_webpage,=,none). 

%second level of fix server config
rule(46,slw,fix_server_congiguration,400):-
	check(second_level_webpage,=,'Problem with Apache Server').

%second level of clear database
rule(47,slw,clear_database,400):-
	check(second_level_webpage,=,'Problem with Apache Server').

%second level of turn on the server
rule(48,slw,turn_on_server,400):-
	check(second_level_webpage,=,'Problem with Apache Server').



%Third level

%Rules about fix firewall third level
rule(50,tlw,fix_firewall,400):- 
    check(third_level_webpage,=,none).
  
%Rules about turn on virtual_machine third level
rule(51,tlw,virtual_machine,400):- 
    check(third_level_webpage,=,none).

%Rules about contacting internet provider third level
rule(52,tlw,contact_IP,300):- 
    check(third_level_webpage,=,'Problem with IP'). 

%Rules about fixing network settings third level
rule(53,tlw,fix_network,300):- 
    check(third_level_webpage,=,none). 

%Rules about fixing linux server third level
rule(54,tlw,'fix_linux',300):- 
    check(third_level_webpage,=,'Problem with Linux - firewall'). 

%rules about contact hssd third level
rule(55,tlw,maintenance,800):- 
    check(third_level_webpage,=,'Problem with linux - Others'). 
rule(56,tlw,maintenance,400):- 
    check(third_level_webpage,=,none). 



%Fourth level

%fourth level of fix server config
rule(70,flw,fix_server_congiguration,400):-
	check(fourth_level_webpage,=,'Server configuration is offset').

%fourth level of clear database
rule(71,flw,clear_database,400):-
	check(fourth_level_webpage,=,'Storage persists').

%fourth level of turn on the server
rule(72,flw,turn_on_server,400):-
	check(fourth_level_webpage,=,'Server is turned off').












% levels for value - _lv



% First level for value

%first level of change_ip_and_reconfigure_data_logger
rule(200,flv,change_ip_and_reconfigure_data_logger,400):-
	check(first_level_value,=,'Value exceeds the range').

%first level of enable_sensors
rule(202,flv,enable_sensors,400):-
	check(first_level_value,=,'Value is missing').

%first level of fix_firewall_audit_settings
rule(203,flv,fix_firewall_audit_settings,400):-
	check(first_level_value,=,'Value is missing').

%first level of fix_vlan_network_settings
rule(204,flv,fix_vlan_network_settings,400):-
	check(first_level_value,=,'Value is missing').

%first level of fix_cabinet_power
rule(205,flv,fix_cabinet_power,400):-
	check(first_level_value,=,'Value is missing').

%first level of fix_internal_fuse - NEW
rule(206,flv,fix_internal_fuse,400):-
	check(first_level_value,=,'Value is missing').

%first level of reset_reconfigure_IO  Values rules
rule(801,flv,reset_reconfigure_IO,400):-
	check(first_level_value,=,'Value is missing').

%first level of fix_configuration Values rules
rule(802,flv,fix_configuration,400):-
	check(first_level_value,=,'Negative or Zero Values reported').

%first level of reset_reconfigure_datalogger Values rules
rule(803,flv,reset_reconfigure_datalogger,400):-
	check(first_level_value,=,'Negative or Zero Values reported').

%first level of use_spare_IO Values rules
rule(804,flv,use_spare_IO,400):-
	check(first_level_value,=,'Negative or Zero Values reported').

%first level of fix_barrier_device_readings Values rules
rule(805,flv,fix_barrier_device_readings,400):-
	check(first_level_value,=,'Negative or Zero Values reported').

%first level of use_spare_cables Values rules
rule(806,flv,use_spare_cables,400):-
	check(first_level_value,=,'Negative or Zero Values reported').

%first level of fix_power_issues Values rules
rule(807,flv,fix_power_issues,400):-
	check(first_level_value,=,'Negative or Zero Values reported').



% Second level for value

%second level of change_ip_and_reconfig_datalogger	
rule(210,sclv,change_ip_and_reconfigure_data_logger,400):-
	check(second_level_value,=,'Error in sensor Configurations').

%second level of Enable_sensors	
rule(212,sclv,enable_sensors,400):-
	check(second_level_value,=,'Sensor not functional').

%second level of Fix_firewall_audit_settings	
rule(213,sclv,fix_firewall_audit_settings,400):-
	check(second_level_value,=,'Sensor not functional').

%second level of Fix_vlan_network_settings	
rule(214,sclv,fix_vlan_network_settings,400):-
	check(second_level_value,=,'Sensor not functional').

%second level of fix_cabinet_power	
rule(215,sclv,fix_cabinet_power,400):-
	check(second_level_value,=,'Sensor not functional').

%second level of Fix_internal_fuse - NEW	
rule(216,sclv,fix_internal_fuse,400):-
	check(second_level_value,=,'Sensor not functional').

%second level of reset_reconfigure_IO Value rules
rule(811,sclv,reset_reconfigure_IO,400):-
	check(second_level_value,=,'Sensor not functional').

%second level of fix_configuration	
rule(812,sclv,fix_configuration,400):-
	check(second_level_value,=,'Error in sensor Configurations').	

%second level of reset_reconfigure_datalogger	
rule(813,sclv,reset_reconfigure_datalogger,400):-
	check(second_level_value,=,'Error in sensor Configurations').

%second level of use_spare_IO	
rule(814,sclv,use_spare_IO,400):-
	check(second_level_value,=,'Error in sensor Configurations').	

%second level of fix_barrier_device_readings 	
rule(815,sclv,fix_barrier_device_readings,400):-
	check(second_level_value,=,'Error in sensor Configurations').	

%second level of use_spare_cables	
rule(816,sclv,use_spare_cables,400):-
	check(second_level_value,=,'Error in sensor Configurations').

%second level of fix_power_issues	
rule(817,sclv,fix_power_issues,400):-
	check(second_level_value,=,'Error in sensor Configurations').	



% Third level for value

%Rules about Change_ip_and_reconfigure_data_logger third level
rule(220,tlv,change_ip_and_reconfigure_data_logger,400):- 
    check(third_level_value,=,'Problems with I/O readings observed with Maintenance software'). 

%Rules about Enable_sensors third level
rule(222,tlv,enable_sensors,400):- 
    check(third_level_value,=,'None of the above'). 

%Rules about Fix_firewall_audit_settings third level
rule(223,tlv,fix_firewall_audit_settings,300):- 
    check(third_level_value,=,'Other Problems with I/O').

%Rules about Fix_vlan_network_settings third level
rule(224,tlv,fix_vlan_network_settings,300):- 
    check(third_level_value,=,'Other Problems with I/O').

%Rules about fix_cabinet_power third level
rule(225,tlv,fix_cabinet_power,300):- 
    check(third_level_value,=,'Other Problems with I/O').

%Rules about fix_internal_fuse - NEW
rule(226,tlv,'fix_internal_fuse',300):- 
    check(third_level_value,=,'Other Problems with I/O').

%Rules about reset_reconfigure_IO  Values
rule(821,tlv,reset_reconfigure_IO,300):- 
    check(third_level_value,=,'Other Problems with I/O').

%Rules about fix_configuration,
rule(822,tlv,fix_configuration,300):- 
    check(third_level_value,=,'None of the above').

%Rules about reset_reconfigure_datalogger
rule(823,tlv,reset_reconfigure_datalogger,300):- 
    check(third_level_value,=,'Problems with I/O readings observed with Maintenance software').

%Rules about use_spare_IO
rule(824,tlv,use_spare_IO,300):- 
    check(third_level_value,=,'Problems with I/O readings observed with Maintenance software').

%Rules about fix_barrier_device_readings 
rule(825,tlv,fix_barrier_device_readings ,300):- 
    check(third_level_value,=,'Problems with I/O readings observed with Maintenance software').

%Rules about use_spare_cables
rule(826,tlv,use_spare_cables,300):- 
    check(third_level_value,=,'Problems with I/O readings observed with Maintenance software').

%Rules about fix_power_issues
rule(827,tlv,fix_power_issues,300):- 
    check(third_level_value,=,'Problems with I/O readings observed with Maintenance software').



% Fourth level for value

%fourth level of Fix_firewall_audit_settings
rule(230,folv,fix_firewall_audit_settings,400):-
	check(fourth_level_value,=,'I/O Pingable').

%fourth level of Fix_vlan_network_settings
rule(231,folv,fix_vlan_network_settings,400):-
	check(fourth_level_value,=,'I/O Pingable').

%fourth level of fix_cabinet_power
rule(232,folv,fix_cabinet_power,400):-
	check(fourth_level_value,=,'I/O Pingable').

%fourth level of Fix_internal_fuse - NEW
rule(233,folv,fix_internal_fuse,400):-
	check(fourth_level_value,=,'I/O Pingable').

%fourth level of reset_reconfigure_IO
rule(831,folv,reset_reconfigure_IO,400):-
	check(fourth_level_value,=,'I/O not Pingable').



%Fifth level for value

%fifth level of Change_ip_and_reconfigure_data_logger
rule(240,filv,change_ip_and_reconfigure_data_logger,400):-
	check(fifth_level_value,=,'Empty input at I/o reading observed with manufacturer software').

%fifth level of Enable_sensors
rule(242,filv,enable_sensors,400):-
	check(fifth_level_value,=,'None of the above').

%fifth level of Fix_firewall_audit_settings
rule(243,filv,fix_firewall_audit_settings,400):-
	check(fifth_level_value,=,'Problem with Linux Firewall Auditing issue').

%fifth level of Fix_vlan_network_settings
rule(244,filv,fix_vlan_network_settings,400):-
	check(fifth_level_value,=,'Problem with Linux Firewall Auditing issue').

%fifth level of Fix_cabinet_power
rule(245,filv,fix_cabinet_power,400):-
	check(fifth_level_value,=,'Problem with Linux Firewall Auditing issue').

%fifth level of Fix_internal_fuse - NEW
rule(246,filv,fix_internal_fuse,400):-
	check(fifth_level_value,=,'Problem with Linux Firewall Auditing issue').

%fifth level of reset_reconfigure_IO
rule(841,filv,reset_reconfigure_IO,400):-
	check(fifth_level_value,=,'Problem with Linux Firewall Auditing issue').

%fifth level of fix_configuration
rule(842,filv,fix_configuration,400):-
	check(fifth_level_value,=,'None of the above').	

%fifth level of reset_reconfigure_datalogger
rule(843,filv,reset_reconfigure_datalogger,400):-
	check(fifth_level_value,=,'None of the above').

rule(844,filv,use_spare_IO,400):-
	check(fifth_level_value,=,'Problem with local inputs in Multimeter').

%fifth level of fix_barrier_device_readings
rule(845,filv,fix_barrier_device_readings,400):-
	check(fifth_level_value,=,'Problem with local inputs in Multimeter').

%fifth level of use_spare_cables
rule(846,filv,use_spare_cables,400):-
	check(fifth_level_value,=,'Problem with local inputs in Multimeter').

%fifth level of fix_power_issues
rule(847,filv,fix_power_issues,400):-
	check(fifth_level_value,=,'Problem with local inputs in Multimeter').
	


%seven level for value

%seventh level of Change_ip_and_reconfigure_data_logger
rule(260,selv,change_ip_and_reconfigure_data_logger,400):-
	check(seventh_level_value,=,'None of the above').

%seventh level of Enable_sensors
rule(262,selv,enable_sensors,400):-
	check(seventh_level_value,=,'None of the above').

%seventh level of Fix_firewall_audit_settings
rule(263,selv,fix_firewall_audit_settings,400):-
	check(seventh_level_value,=,'None of the above').

%seventh level of Fix_vlan_network_settings
rule(264,selv,fix_vlan_network_settings,400):-
	check(seventh_level_value,=,'Problem with network settings or V-Lan configurations').

%seventh level of Fix_cabinet_power
rule(265,selv,fix_cabinet_power,400):-
	check(seventh_level_value,=,'Problem with network settings or V-Lan configurations').

%seventh level of Fix_internal_fuse - NEW
rule(266,selv,fix_internal_fuse,400):-
	check(seventh_level_value,=,'Problem with network settings or V-Lan configurations').

%seventh level of reset_reconfigure_IO Values
rule(851,selv,reset_reconfigure_IO,400):-
	check(seventh_level_value,=,'Problem with network settings or V-Lan configurations').

%seventh level of fix_configuration
rule(852,selv,fix_configuration,400):-
	check(seventh_level_value,=,'None of the above').

%seventh level of reset_reconfigure_datalogger
rule(853,selv,reset_reconfigure_datalogger,400):-
	check(seventh_level_value,=,'None of the above').

%seventh level of use_spare_IO
rule(854,selv,use_spare_IO,400):-
	check(seventh_level_value,=,'None of the above').

%seventh level of fix_barrier_device_readings
rule(855,selv,fix_barrier_device_readings,400):-
	check(seventh_level_value,=,'Incorrect values observed in Barrier devices').

%seventh level of use_spare_cables
rule(856,selv,use_spare_cables,400):-
	check(seventh_level_value,=,'Incorrect values observed in Barrier devices').

%seventh level of fix_power_issues
rule(857,selv,fix_power_issues,400):-
	check(seventh_level_value,=,'Incorrect values observed in Barrier devices').



% Eighth level for value

%eighth level of Change_ip_and_reconfigure_data_logger
rule(270,elv,change_ip_and_reconfigure_data_logger,400):-
	check(eighth_level_value,=,'None of the above').

%eighth level of Enable_sensors
rule(272,elv,enable_sensors,400):-
	check(eighth_level_value,=,'Cable problem with the LAN network').

%eighth level of Fix_firewall_audit_settings
rule(273,elv,fix_firewall_audit_settings,400):-
	check(eighth_level_value,=,'Cable problem with the LAN network').

%eighth level of Fix_vlan_network_settings
rule(274,elv,fix_vlan_network_settings,400):-
	check(eighth_level_value,=,'Cable problem with the LAN network').

%eighth level of Fix_cabinet_power
rule(275,elv,fix_cabinet_power,400):-
	check(eighth_level_value,=,'Cable problem with the LAN network').

%eighth level of Fix_internal_fuse - NEW
rule(276,elv,fix_internal_fuse,400):-
	check(eighth_level_value,=,'Cable problem with the LAN network').

%eighth level of reset_reconfigure_IO Values
rule(861,elv,reset_reconfigure_IO,400):-
	check(eighth_level_value,=,'Cable problem with the LAN network').

%eighth level of fix_configuration Values
rule(862,elv,fix_configuration,400):-
	check(eighth_level_value,=,'None of the above').

%eighth level of reset_reconfigure_datalogger Values
rule(863,elv,reset_reconfigure_datalogger,400):-
	check(eighth_level_value,=,'None of the above').

%eighth level of use_spare_IO
rule(864,elv,use_spare_IO,400):-
	check(eighth_level_value,=,'None of the above').

%eighth level of fix_barrier_device_readings Values
rule(865,elv,fix_barrier_device_readings,400):-
	check(eighth_level_value,=,'None of the above').

%eighth level of use_spare_cables Values
rule(866,elv,use_spare_cables,400):-
	check(eighth_level_value,=,'Problem with Analyser in the field').

%eighth level of fix_power_issues Values
rule(867,elv,fix_power_issues,400):-
	check(eighth_level_value,=,'Problem with Analyser in the field').



%Ninth level for value

%ninth level of use_spare_cables Values
rule(976,nlv,use_spare_cables,400):-
	check(ninth_level_value,=,'The is output ok').

rule(977,nlv,fix_power_issues,400):-
	check(ninth_level_value,=,'Others').



%tenth level Values

%tenth level of Fix_cabinet_power
rule(290,telv,fix_cabinet_power,400):-
	check(tenth_level_value,=,'Cabinet Power issues').

%tenth level of Fix_internal_fuse - NEW
rule(870,telv,fix_internal_fuse,400):-
	check(tenth_level_value,=,'Cabinet Power issues').

rule(871,telv,reset_reconfigure_IO,400):-
	check(tenth_level_value,=,'Cabinet Power issues').

rule(872,telv,fix_configuration,400):-
	check(tenth_level_value,=,'No issue').

%tenth level of reset_reconfigure_datalogger Values
rule(873,telv,reset_reconfigure_datalogger,400):-
	check(tenth_level_value,=,'No issue').

%tenth level of use_spare_IO Values
rule(874,telv,use_spare_IO,400):-
	check(tenth_level_value,=,'No issue').

%tenth level of fix_barrier_device_readings Values
rule(875,telv,fix_barrier_device_readings,400):-
	check(tenth_level_value,=,'No issue').

%tenth level of use_spare_cables Values
rule(876,telv,use_spare_cables,400):-
	check(tenth_level_value,=,'No issue').

rule(877,telv,fix_power_issues,400):-
	check(tenth_level_value,=,'No issue').

%tenth level of Fix_vlan_network_settings
rule(878,telv,fix_vlan_network_settings,400):-
	check(tenth_level_value,=,'No issue').



%eleventh level Values

%eleventh level of Fix_cabinet_power
rule(295,elelv,fix_cabinet_power,400):-
	check(eleventh_level_value,=,'Cabinet Power is off').

%eleventh level of Fix_internal_fuse - NEW
rule(880,elelv,fix_internal_fuse,400):-
	check(eleventh_level_value,=,'Internal fuse is broken').

rule(991,elelv,reset_reconfigure_IO,400):-
	check(eleventh_level_value,=,'Internal fuse works').



% Twelfth Level

rule(992,twelv,reset_reconfigure_IO,400):-
	check(twelfth_level_value,=,'I/o gives readings in Local laptop').




%conclusion of fix server config
rule(500,solutions_for_webpage_errors,fix_server_congiguration,1000):-
	check(filw,fix_server_congiguration,'cf>=',400),
	check(slw,fix_server_congiguration,'cf>=',400),
	check(flw,fix_server_congiguration,'cf>=',400).
rule(501,solutions_for_webpage_errors,fix_server_congiguration,600):-
	(\+check(filw,fix_server_congiguration,'cf>=',400),
	check(slw,fix_server_congiguration,'cf>=',400),
	check(flw,fix_server_congiguration,'cf>=',400));
	(check(filw,fix_server_congiguration,'cf>=',400),
	\+check(slw,fix_server_congiguration,'cf>=',400),
	check(flw,fix_server_congiguration,'cf>=',400));
	(check(filw,fix_server_congiguration,'cf>=',400),
	check(slw,fix_server_congiguration,'cf>=',400),
	\+check(flw,fix_server_congiguration,'cf>=',400)).


%conclusion of clear database
rule(510,solutions_for_webpage_errors,clear_database,1000):-
	check(filw,clear_database,'cf>=',400),
	check(slw,clear_database,'cf>=',400),
	check(flw,clear_database,'cf>=',400).
rule(511,solutions_for_webpage_errors,clear_database,600):-
	(check(filw,clear_database,'cf>=',400),
	check(slw,clear_database,'cf>=',400),
	\+check(flw,clear_database,'cf>=',400));
	(check(filw,clear_database,'cf>=',400),
	\+check(slw,clear_database,'cf>=',400),
	check(flw,clear_database,'cf>=',400));
	(\+check(filw,clear_database,'cf>=',400),
	check(slw,clear_database,'cf>=',400),
	check(flw,clear_database,'cf>=',400)).


%conclusion of turn_on_server
rule(520,solutions_for_webpage_errors,turn_on_server,1000):-
	check(filw,turn_on_server,'cf>=',400),
	check(slw,turn_on_server,'cf>=',400),
	check(flw,turn_on_server,'cf>=',400).
rule(521,solutions_for_webpage_errors,turn_on_server,600):-
	(\+check(filw,turn_on_server,'cf>=',400),
	check(slw,turn_on_server,'cf>=',400),
	check(flw,turn_on_server,'cf>=',400));
	(check(filw,turn_on_server,'cf>=',400),
	\+check(slw,turn_on_server,'cf>=',400),
	check(flw,turn_on_server,'cf>=',400));
	(check(filw,turn_on_server,'cf>=',400),
	check(slw,turn_on_server,'cf>=',400),
	\+check(flw,turn_on_server,'cf>=',400)).


%Rules about conclusions regarding "Fix firewall"
rule(530,solutions_for_webpage_errors,fix_firewall, 1000):-
	check(filw,fix_firewall, 'cf>=', 500),
	check(slw,fix_firewall, 'cf>=', 400),
	check(tlw,fix_firewall, 'cf>=', 400).
rule(531,solutions_for_webpage_errors,fix_firewall, 600):-
	(check(filw,fix_firewall, 'cf>=', 500),
	check(slw,fix_firewall, 'cf>=', 400),
	\+check(tlw,fix_firewall, 'cf>=', 400));
	(\+check(filw,fix_firewall, 'cf>=', 500),
	check(slw,fix_firewall, 'cf>=', 400),
	check(tlw,fix_firewall, 'cf>=', 400));
	(check(filw,fix_firewall, 'cf>=', 500),
	\+check(slw,fix_firewall, 'cf>=', 400),
	check(tlw,fix_firewall, 'cf>=', 400)).

rule(532,solutions_for_webpage_errors,fix_firewall, 400):-
	(\+check(filw,fix_firewall, 'cf>=', 500),
	\+check(slw,fix_firewall, 'cf>=', 400),
	check(tlw,fix_firewall, 'cf>=', 400));
	(\+check(filw,fix_firewall, 'cf>=', 500),
	check(slw,fix_firewall, 'cf>=', 400),
	\+check(tlw,fix_firewall, 'cf>=', 400));
	(check(filw,fix_firewall, 'cf>=', 500),
	\+check(slw,fix_firewall, 'cf>=', 400),
	\+check(tlw,fix_firewall, 'cf>=', 400)).

%Rules about conclusions regarding to "Turn on the virtual machine"

rule(540,solutions_for_webpage_errors,virtual_machine, 1000):-
	check(filw,virtual_machine, 'cf>=', 500),
	check(slw,virtual_machine, 'cf>=', 400),
	check(tlw,virtual_machine, 'cf>=', 400).

rule(541,solutions_for_webpage_errors,virtual_machine, 600):-
	(check(filw,virtual_machine, 'cf>=', 500),
	check(slw,virtual_machine, 'cf>=', 400),
	\+check(tlw,virtual_machine, 'cf>=', 400));
	(check(filw,virtual_machine, 'cf>=', 500),
	\+check(slw,virtual_machine, 'cf>=', 400),
	check(tlw,virtual_machine, 'cf>=', 400));
	(\+check(filw,virtual_machine, 'cf>=', 500),
	check(slw,virtual_machine, 'cf>=', 400),
	check(tlw,virtual_machine, 'cf>=', 400)).

rule(542,solutions_for_webpage_errors,virtual_machine, 400):-
	(\+check(filw,virtual_machine, 'cf>=', 500),
	\+check(slw,virtual_machine, 'cf>=', 400),
	check(tlw,virtual_machine, 'cf>=', 400));
	(\+check(filw,virtual_machine, 'cf>=', 500),
	check(slw,virtual_machine, 'cf>=', 400),
	\+check(tlw,virtual_machine, 'cf>=', 400));
	(check(filw,virtual_machine, 'cf>=', 500),
	\+check(slw,virtual_machine, 'cf>=', 400),
	\+check(tlw,virtual_machine, 'cf>=', 400)).

%Rules about conclusions regarding to contact the internet provider

rule(550,solutions_for_webpage_errors,contact_IP, 1000):-
	check(filw,contact_IP, 'cf>=', 300),
	check(slw,contact_IP, 'cf>=', 300),
	check(tlw,contact_IP, 'cf>=', 300).

rule(551,solutions_for_webpage_errors,contact_IP, 600):-
	(\+check(filw,contact_IP, 'cf>=', 300),
	check(slw,contact_IP, 'cf>=', 300),
	check(tlw,contact_IP, 'cf>=', 300));
	(check(filw,contact_IP, 'cf>=', 300),
	\+check(slw,contact_IP, 'cf>=', 300),
	check(tlw,contact_IP, 'cf>=', 300));
	(check(filw,contact_IP, 'cf>=', 300),
	check(slw,contact_IP, 'cf>=', 300),
	\+check(tlw,contact_IP, 'cf>=', 300)).

rule(552,solutions_for_webpage_errors,contact_IP, 300):-
	(\+check(filw,contact_IP, 'cf>=', 300),
	\+check(slw,contact_IP, 'cf>=', 300),
	check(tlw,contact_IP, 'cf>=', 300));
	(\+check(filw,contact_IP, 'cf>=', 300),
	check(slw,contact_IP, 'cf>=', 300),
	\+check(tlw,contact_IP, 'cf>=', 300));
	(check(filw,contact_IP, 'cf>=', 300),
	\+check(slw,contact_IP, 'cf>=', 300),
	\+check(tlw,contact_IP, 'cf>=', 300)).

%Rules about conclusions regarding to fix the network settings

rule(560,solutions_for_webpage_errors,fix_network, 1000):-
	check(filw,fix_network, 'cf>=', 200),
	check(slw,fix_network, 'cf>=', 300),
	check(tlw,fix_network, 'cf>=', 300).

rule(561,solutions_for_webpage_errors,fix_network, 600):-
	(\+check(filw,fix_network, 'cf>=', 200),
	check(slw,fix_network, 'cf>=', 300),
	check(tlw,fix_network, 'cf>=', 300));
	(check(filw,fix_network, 'cf>=', 200),
	\+check(slw,fix_network, 'cf>=', 300),
	check(tlw,fix_network, 'cf>=', 300));
	(check(filw,fix_network, 'cf>=', 200),
	check(slw,fix_network, 'cf>=', 300),
	\+check(tlw,fix_network, 'cf>=', 300)).

rule(562,solutions_for_webpage_errors,fix_network, 200):-
	(\+check(filw,fix_network, 'cf>=', 200),
	\+check(slw,fix_network, 'cf>=', 300),
	check(tlw,fix_network, 'cf>=', 300));
	(\+check(filw,fix_network, 'cf>=', 200),
	check(slw,fix_network, 'cf>=', 300),
	\+check(tlw,fix_network, 'cf>=', 300));
	(check(filw,fix_network, 'cf>=', 200),
	\+check(slw,fix_network, 'cf>=', 300),
	\+check(tlw,fix_network, 'cf>=', 300)).

%Rules about conclusions regarding to fixing the linux server

rule(570,solutions_for_webpage_errors,fix_linux, 1000):-
	check(filw,fix_linux, 'cf>=', 200),
	check(slw,fix_linux, 'cf>=', 300),
	check(tlw,fix_linux, 'cf>=', 300).

rule(571,solutions_for_webpage_errors,fix_linux, 600):-
	(\+check(filw,fix_linux, 'cf>=', 200),
	check(slw,fix_linux, 'cf>=', 300),
	check(tlw,fix_linux, 'cf>=', 300));
	(check(filw,fix_linux, 'cf>=', 200),
	\+check(slw,fix_linux, 'cf>=', 300),
	check(tlw,fix_linux, 'cf>=', 300));
	(check(filw,fix_linux, 'cf>=', 200),
	check(slw,fix_linux, 'cf>=', 300),
	\+check(tlw,fix_linux, 'cf>=', 300)).

rule(572,solutions_for_webpage_errors,fix_linux, 200):-
	(\+check(filw,fix_linux, 'cf>=', 200),
	\+check(slw,fix_linux, 'cf>=', 300),
	check(tlw,fix_linux, 'cf>=', 300));
	(\+check(filw,fix_linux, 'cf>=', 200),
	check(slw,fix_linux, 'cf>=', 300),
	\+check(tlw,fix_linux, 'cf>=', 300));
	(check(filw,fix_linux, 'cf>=', 200),
	\+check(slw,fix_linux, 'cf>=', 300),
	\+check(tlw,fix_linux, 'cf>=', 300)).
	

%Rules about conclusions regarding to contact HSSD

rule(580,solutions_for_webpage_errors,contact_HSSD, 1000):-
	check(type_problem,=,webpage),
	check(filw,maintenance, 'cf>=', 1000).

rule(581,solutions_for_webpage_errors,contact_HSSD, 800):-
	check(type_problem,=,webpage),
	check(tlw,maintenance, 'cf>=', 800).

rule(582,solutions_for_webpage_errors,contact_HSSD, 400):-
	check(type_problem,=,webpage),
	check(tlw,maintenance, 'cf>=', 400).

rule(583,solutions_for_webpage_errors,contact_HSSD, 200):-
	check(type_problem,=,webpage),
	check(slw,maintenance, 'cf>=', 400).

rule(585,solutions_for_webpage_errors,contact_HSSD, 200):-
	check(type_problem,=,webpage),
	(\+check(filw,fix_firewall, 'cf>=', 500),
	\+check(slw,fix_firewall, 'cf>=', 400),
	\+check(tlw,fix_firewall, 'cf>=', 400));

	(\+check(filw,virtual_machine, 'cf>=', 500),
	\+check(slw,virtual_machine, 'cf>=', 400),
	\+check(tlw,virtual_machine, 'cf>=', 400));

	(\+check(filw,contact_IP, 'cf>=', 300),
	\+check(slw,contact_IP, 'cf>=', 300),
	\+check(tlw,contact_IP, 'cf>=', 300));

	(\+check(filw,fix_network, 'cf>=', 200),
	\+check(slw,fix_network, 'cf>=', 300),
	\+check(tlw,fix_network, 'cf>=', 300));

	(\+check(filw,fix_linux, 'cf>=', 200),
	\+check(slw,fix_linux, 'cf>=', 300),
	\+check(tlw,fix_linux, 'cf>=', 300));

	((\+check(filw,fix_server_congiguration,'cf>=',400),
	\+check(slw,fix_server_congiguration,'cf>=',400),
	check(flw,fix_server_congiguration,'cf>=',400));
	(\+check(filw,fix_server_congiguration,'cf>=',400),
	check(slw,fix_server_congiguration,'cf>=',400),
	\+check(flw,fix_server_congiguration,'cf>=',400));
	(check(filw,fix_server_congiguration,'cf>=',400),
	\+check(slw,fix_server_congiguration,'cf>=',400),
	\+check(flw,fix_server_congiguration,'cf>=',400));
	(\+check(filw,fix_server_congiguration,'cf>=',400),
	\+check(slw,fix_server_congiguration,'cf>=',400),
	\+check(flw,fix_server_congiguration,'cf>=',400)));

	((\+check(filw,clear_database,'cf>=',400),
	\+check(slw,clear_database,'cf>=',400),
	\+check(flw,clear_database,'cf>=',400));
	(\+check(filw,clear_database,'cf>=',400),
	\+check(slw,clear_database,'cf>=',400),
	check(flw,clear_database,'cf>=',400));
	(\+check(filw,clear_database,'cf>=',400),
	check(slw,clear_database,'cf>=',400),
	\+check(flw,clear_database,'cf>=',400));
	(check(filw,clear_database,'cf>=',400),
	\+check(slw,clear_database,'cf>=',400),
	\+check(flw,clear_database,'cf>=',400)));

	((\+check(filw,turn_on_server,'cf>=',400),
	\+check(slw,turn_on_server,'cf>=',400),
	check(flw,turn_on_server,'cf>=',400));
	(\+check(filw,turn_on_server,'cf>=',400),
	check(slw,turn_on_server,'cf>=',400),
	\+check(flw,turn_on_server,'cf>=',400));
	(check(filw,turn_on_server,'cf>=',400),
	\+check(slw,turn_on_server,'cf>=',400),
	\+check(flw,turn_on_server,'cf>=',400));
	(\+check(filw,turn_on_server,'cf>=',400),
	\+check(slw,turn_on_server,'cf>=',400),
	\+check(flw,turn_on_server,'cf>=',400))).


%conclusion of Change_ip_and_reconfigure_data_logger
rule(300,solutions_for_value_errors,change_ip_and_reconfigure_data_logger,1000):-
	check(flv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(sclv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(tlv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(filv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(selv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(elv,change_ip_and_reconfigure_data_logger,'cf>=',400).
rule(301,solutions_for_value_errors,change_ip_and_reconfigure_data_logger,600):-
	check(flv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(sclv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(tlv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(filv,change_ip_and_reconfigure_data_logger,'cf>=',400),
	check(selv,change_ip_and_reconfigure_data_logger,'cf>=',400).


%conclusion of enable_sensors
rule(304,solutions_for_value_errors,enable_sensors,1000):-
	check(flv,enable_sensors,'cf>=',400),
	check(sclv,enable_sensors,'cf>=',400),
	check(tlv,enable_sensors,'cf>=',400),
	check(filv,enable_sensors,'cf>=',400),
	check(selv,enable_sensors,'cf>=',400),
	check(elv,enable_sensors,'cf>=',400).
rule(305,solutions_for_value_errors,enable_sensors,600):-
	check(flv,enable_sensors,'cf>=',400),
	check(sclv,enable_sensors,'cf>=',400),
	check(tlv,enable_sensors,'cf>=',400),
	check(filv,enable_sensors,'cf>=',400),
	check(selv,enable_sensors,'cf>=',400).

rule(306,solutions_for_value_errors,fix_firewall_audit_settings,1000):-
	check(flv,fix_firewall_audit_settings,'cf>=',400),
	check(sclv,fix_firewall_audit_settings,'cf>=',400),
	check(tlv,fix_firewall_audit_settings,'cf>=',300),
	check(folv,fix_firewall_audit_settings,'cf>=',400),
	check(filv,fix_firewall_audit_settings,'cf>=',400),
	check(selv,fix_firewall_audit_settings,'cf>=',400),
	check(elv,fix_firewall_audit_settings,'cf>=',400).
rule(307,solutions_for_value_errors,fix_firewall_audit_settings,600):-
	check(flv,fix_firewall_audit_settings,'cf>=',400),
	check(sclv,fix_firewall_audit_settings,'cf>=',400),
	check(tlv,fix_firewall_audit_settings,'cf>=',300),
	check(folv,fix_firewall_audit_settings,'cf>=',400),
	check(filv,fix_firewall_audit_settings,'cf>=',400),
	check(selv,fix_firewall_audit_settings,'cf>=',400).

%conclusion of fix_vlan_network_settings
rule(308,solutions_for_value_errors,fix_vlan_network_settings,1000):-
	check(flv,fix_vlan_network_settings,'cf>=',400),
	check(sclv,fix_vlan_network_settings,'cf>=',400),
	check(tlv,fix_vlan_network_settings,'cf>=',300),
	check(folv,fix_vlan_network_settings,'cf>=',400),
	check(filv,fix_vlan_network_settings,'cf>=',400),
	check(selv,fix_vlan_network_settings,'cf>=',400),
	check(elv,fix_vlan_network_settings,'cf>=',400),
	check(telv,fix_vlan_network_settings,'cf>=',400).
rule(309,solutions_for_value_errors,fix_vlan_network_settings,600):-
	check(flv,fix_vlan_network_settings,'cf>=',400),
	check(sclv,fix_vlan_network_settings,'cf>=',400),
	check(tlv,fix_vlan_network_settings,'cf>=',300),
	check(folv,fix_vlan_network_settings,'cf>=',400),
	check(filv,fix_vlan_network_settings,'cf>=',400),
	check(selv,fix_vlan_network_settings,'cf>=',400),
	check(elv,fix_vlan_network_settings,'cf>=',400).

%conclusion of fix_cabinet_power
rule(310,solutions_for_value_errors,fix_cabinet_power,1000):-
	check(flv,fix_cabinet_power,'cf>=',400),
	check(sclv,fix_cabinet_power,'cf>=',400),
	check(tlv,fix_cabinet_power,'cf>=',300),
	check(folv,fix_cabinet_power,'cf>=',400),
	check(filv,fix_cabinet_power,'cf>=',400),
	check(selv,fix_cabinet_power,'cf>=',400),
	check(elv,fix_cabinet_power,'cf>=',400),
	check(telv,fix_cabinet_power,'cf>=',400),
	check(elelv,fix_cabinet_power,'cf>=',400).
rule(311,solutions_for_value_errors,fix_cabinet_power,600):-
	check(flv,fix_cabinet_power,'cf>=',400),
	check(sclv,fix_cabinet_power,'cf>=',400),
	check(tlv,fix_cabinet_power,'cf>=',300),
	check(folv,fix_cabinet_power,'cf>=',400),
	check(filv,fix_cabinet_power,'cf>=',400),
	check(selv,fix_cabinet_power,'cf>=',400),
	check(elv,fix_cabinet_power,'cf>=',400),
	check(telv,fix_cabinet_power,'cf>=',400).


%conclusion of fix_internal_fuse
rule(312,solutions_for_value_errors,fix_internal_fuse,1000):-
	check(flv,fix_internal_fuse,'cf>=',400),
	check(sclv,fix_internal_fuse,'cf>=',400),
	check(tlv,fix_internal_fuse,'cf>=',300),
	check(folv,fix_internal_fuse,'cf>=',400),
	check(filv,fix_internal_fuse,'cf>=',400),
	check(selv,fix_internal_fuse,'cf>=',400),
	check(elv,fix_internal_fuse,'cf>=',400),
	check(telv,fix_internal_fuse,'cf>=',400),
	check(elelv,fix_internal_fuse,'cf>=',400).
rule(313,solutions_for_value_errors,fix_internal_fuse,600):-
	check(flv,fix_internal_fuse,'cf>=',400),
	check(sclv,fix_internal_fuse,'cf>=',400),
	check(tlv,fix_internal_fuse,'cf>=',300),
	check(folv,fix_internal_fuse,'cf>=',400),
	check(filv,fix_internal_fuse,'cf>=',400),
	check(selv,fix_internal_fuse,'cf>=',400),
	check(elv,fix_internal_fuse,'cf>=',400),
	check(telv,fix_internal_fuse,'cf>=',400).



% reset_reconfigure_IO
rule(1001,solutions_for_value_errors,reset_reconfigure_IO,1000):-
	check(flv,reset_reconfigure_IO,'cf>=',400),
	check(sclv,reset_reconfigure_IO,'cf>=',400),
	check(tlv,reset_reconfigure_IO,'cf>=',300),
	check(folv,reset_reconfigure_IO,'cf>=',400),
	check(filv,reset_reconfigure_IO,'cf>=',400),
	check(selv,reset_reconfigure_IO,'cf>=',400),
	check(elv,reset_reconfigure_IO,'cf>=',400),
	check(telv,reset_reconfigure_IO,'cf>=',400),
	check(elelv,reset_reconfigure_IO,'cf>=',400),
	check(twelv,reset_reconfigure_IO,'cf>=',400).

rule(1002,solutions_for_value_errors,reset_reconfigure_IO,600):-
	check(flv,reset_reconfigure_IO,'cf>=',400),
	check(sclv,reset_reconfigure_IO,'cf>=',400),
	check(tlv,reset_reconfigure_IO,'cf>=',300),
	check(folv,reset_reconfigure_IO,'cf>=',400),
	check(filv,reset_reconfigure_IO,'cf>=',400),
	check(selv,reset_reconfigure_IO,'cf>=',400),
	check(elv,reset_reconfigure_IO,'cf>=',400),
	check(telv,reset_reconfigure_IO,'cf>=',400),
	check(elelv,reset_reconfigure_IO,'cf>=',400).
	%check(twelv,reset_reconfigure_IO,'cf<',400).

%  fix_configuration conclusions
rule(1003,solutions_for_value_errors,fix_configuration,1000):-
	check(flv,fix_configuration,'cf>=',400),
	check(sclv,fix_configuration,'cf>=',400),
	check(tlv,fix_configuration,'cf>=',300),
	check(filv,fix_configuration,'cf>=',400),
	check(selv,fix_configuration,'cf>=',400),
	check(elv,fix_configuration,'cf>=',400),
	check(telv,fix_configuration,'cf>=',400).


rule(1004,solutions_for_value_errors,fix_configuration,600):-
	check(flv,fix_configuration,'cf>=',400),
	check(sclv,fix_configuration,'cf>=',400),
	check(tlv,fix_configuration,'cf>=',300),
	check(filv,fix_configuration,'cf>=',400),
	check(selv,fix_configuration,'cf>=',400),
	check(elv,fix_configuration,'cf>=',400).


% reset_reconfigure_datalogger
rule(1005,solutions_for_value_errors,reset_reconfigure_datalogger,1000):-
	check(flv,reset_reconfigure_datalogger,'cf>=',400),
	check(sclv,reset_reconfigure_datalogger,'cf>=',400),
	check(tlv,reset_reconfigure_datalogger,'cf>=',300),
	check(filv,reset_reconfigure_datalogger,'cf>=',400),
	check(selv,reset_reconfigure_datalogger,'cf>=',400),
	check(elv,reset_reconfigure_datalogger,'cf>=',400),
	check(telv,reset_reconfigure_datalogger,'cf>=',400).

rule(1006,solutions_for_value_errors,reset_reconfigure_datalogger,600):-
	check(flv,reset_reconfigure_datalogger,'cf>=',400),
	check(sclv,reset_reconfigure_datalogger,'cf>=',400),
	check(tlv,reset_reconfigure_datalogger,'cf>=',300),
	check(filv,reset_reconfigure_datalogger,'cf>=',400),
	check(selv,reset_reconfigure_datalogger,'cf>=',400),
	check(elv,reset_reconfigure_datalogger,'cf>=',400).

%use_spare_IO
rule(1007,solutions_for_value_errors,use_spare_IO,1000):-
	check(flv,use_spare_IO,'cf>=',400),
	check(sclv,use_spare_IO,'cf>=',400),
	check(tlv,use_spare_IO,'cf>=',300),
	check(filv,use_spare_IO,'cf>=',400),
	check(selv,use_spare_IO,'cf>=',400),
	check(elv,use_spare_IO,'cf>=',400),
	check(telv,use_spare_IO,'cf>=',400).
rule(1008,solutions_for_value_errors,use_spare_IO,600):-
	check(flv,use_spare_IO,'cf>=',400),
	check(sclv,use_spare_IO,'cf>=',400),
	check(tlv,use_spare_IO,'cf>=',300),
	check(filv,use_spare_IO,'cf>=',400),
	check(selv,use_spare_IO,'cf>=',400),
	check(elv,use_spare_IO,'cf>=',400).
	%check(telv,use_spare_IO,'cf<',400).

%fix_barrier_device_readings
rule(1009,solutions_for_value_errors,fix_barrier_device_readings,1000):-
	check(flv,fix_barrier_device_readings,'cf>=',400),
	check(sclv,fix_barrier_device_readings,'cf>=',400),
	check(tlv,fix_barrier_device_readings,'cf>=',300),
	check(filv,fix_barrier_device_readings,'cf>=',400),
	check(selv,fix_barrier_device_readings,'cf>=',400),
	check(elv,fix_barrier_device_readings,'cf>=',400),
	check(telv,fix_barrier_device_readings,'cf>=',400).
rule(1010,solutions_for_value_errors,fix_barrier_device_readings,600):-
	check(flv,fix_barrier_device_readings,'cf>=',400),
	check(sclv,fix_barrier_device_readings,'cf>=',400),
	check(tlv,fix_barrier_device_readings,'cf>=',300),
	check(filv,fix_barrier_device_readings,'cf>=',400),
	check(selv,fix_barrier_device_readings,'cf>=',400),
	check(elv,fix_barrier_device_readings,'cf>=',400).
	%check(telv,fix_barrier_device_readings,'cf<',400).

%use_spare_cables
rule(1011,solutions_for_value_errors,use_spare_cables,1000):-
	check(flv,use_spare_cables,'cf>=',400),
	check(sclv,use_spare_cables,'cf>=',400),
	check(tlv,use_spare_cables,'cf>=',300),
	check(filv,use_spare_cables,'cf>=',400),
	check(selv,use_spare_cables,'cf>=',400),
	check(elv,use_spare_cables,'cf>=',400),
	check(nlv,use_spare_cables,'cf>=',400),
	check(telv,use_spare_cables,'cf>=',400).
rule(1012,solutions_for_value_errors,use_spare_cables,600):-
	check(flv,use_spare_cables,'cf>=',400),
	check(sclv,use_spare_cables,'cf>=',400),
	check(tlv,use_spare_cables,'cf>=',300),
	check(filv,use_spare_cables,'cf>=',400),
	check(selv,use_spare_cables,'cf>=',400),
	check(elv,use_spare_cables,'cf>=',400),
	check(nlv,use_spare_cables,'cf>=',400).
	%check(telv,use_spare_cables,'cf<',400).

% fix_power_issues
rule(1013,solutions_for_value_errors,fix_power_issues,1000):-
	check(flv,fix_power_issues,'cf>=',400),
	check(sclv,fix_power_issues,'cf>=',400),
	check(tlv,fix_power_issues,'cf>=',300),
	check(filv,fix_power_issues,'cf>=',400),
	check(selv,fix_power_issues,'cf>=',400),
	check(elv,fix_power_issues,'cf>=',400),
	check(nlv,fix_power_issues,'cf>=',400),
	check(telv,fix_power_issues,'cf>=',400).
rule(1014,solutions_for_value_errors,fix_power_issues,600):-
	check(flv,fix_power_issues,'cf>=',400),
	check(sclv,fix_power_issues,'cf>=',400),
	check(tlv,fix_power_issues,'cf>=',300),
	check(filv,fix_power_issues,'cf>=',400),
	check(selv,fix_power_issues,'cf>=',400),
	check(elv,fix_power_issues,'cf>=',400),
	check(nlv,fix_power_issues,'cf>=',400).


%Rules about conclusions regarding to contact HSSD

rule(2000,solutions_for_value_errors,contact_HSSD,400):-
	check(type_problem,=,value),
	(\+check(change_ip_and_reconfigure_data_logger, change_ip_and_reconfigure_data_logger,'cf>=',600);
	\+check(enable_sensors, enable_sensors,'cf>=',600);
	\+check(fix_firewall_audit_settings, fix_firewall_audit_settings,'cf>=',600);
	\+check(fix_vlan_network_settings, fix_vlan_network_settings,'cf>=',600);
	\+check(fix_cabinet_power, fix_cabinet_power,'cf>=',600);
	\+check(fix_internal_fuse, fix_internal_fuse,'cf>=',600);
	\+check(reset_reconfigure_IO, reset_reconfigure_IO,'cf>=',600);
	\+check(fix_configuration, fix_configuration,'cf>=',600);
	\+check(reset_reconfigure_datalogger, reset_reconfigure_datalogger,'cf>=',600);
	\+check(use_spare_IO, use_spare_IO,'cf>=',600);
	\+check(fix_barrier_device_readings, fix_barrier_device_readings,'cf>=',600);
	\+check(use_spare_cables, use_spare_cables,'cf>=',600);
	\+check(fix_power_issues, change_ip_fix_power_issues,'cf>=',600)).
	



%*****************************************************************        
%*****************************************************************        
% Window 6 - Presentation to the user


% *********************************************
% ************* Presentation of results *******
                
% Displays the conclusions and a menu with options to save answers, get
% how-explaniation or new consultation etc.

presentation_result:-
		\+tcl_GUI(on),
        to_present(Presentationsparametrar),nl,nl,
  write('Result:'), nl,
        ((result_exists(Presentationsparametrar),
        presentation(Presentationsparametrar)) ;
        write_no_result).

presentation_result:-
		tcl_GUI(on),
        to_present(Presentationsparametrar),
        present_setup(Presentationsparametrar).

present_setup(Presentationsparametrar):-		
		tcl_GUI(on),
		result_exists(Presentationsparametrar),!,
		assert_num_options(Presentationsparametrar),
        presentation(Presentationsparametrar),
		Source = 'source k:/klöver/conclusion.tcl',
		run_tcl(Source,Event),
		retract_and_destroy(Event).
present_setup(Presentationsparametrar):-
		tcl_GUI(on),
		\+result_exists(Presentationsparametrar),
		write_no_result.

result_exists(Presentationsparametrar):-
        setof(Object, (trigged(Object,_,CF), CF =\= 0,
              memb(Object,Presentationsparametrar)), Pres_List).

write_no_result:-
		\+tcl_GUI(on),
        write(' No conclusions have been drawn '), nl.
		
write_no_result:-
		tcl_GUI(on),
		Source = 'source k:/klöver/no_conclusion.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event).

% Finds all conclusions that have been allocated a value.
presentation([]).
presentation([Object | Rest]) :-
		\+tcl_GUI(on),
        findall((Attribute,CF) , (trigged(Object, Attribute, CF), CF =\= 0), Resultlist),
        write_result(Object,Resultlist),
        presentation(Rest).
%Finds and assert all conclusions that has a value
presentation([Object | Rest]) :-
		tcl_GUI(on),
		assert(answer_options(Object)), 
        findall((Attribute,CF) , (trigged(Object, Attribute, CF), CF =\= 0), Resultlist),
		%remove redundant conclusions
		member_check(Resultlist, NewList),
        assert_attr_list(Object, NewList),
		assert_num_attributes(Object, NewList),
        presentation(Rest).
	
member_check([],[]).
member_check([E|Rest],[E|New]):-
		\+member(E,Rest),
		member_check(Rest,New).
member_check([E|Rest],New):-
		member(E,Rest),
		member_check(Rest,New).		
	
%member(E,[E|Rest]):-
%	member(E,Rest).
%member(E,[F|Rest]):-
%	E \= F,
%	member(E,Rest).
	
% Prints all conclusions saved in the knowledge base
% that are connected to the conclusion. Presented as text strings.
write_result(Object, []).
write_result(Object, Resultlist) :-
        text(Object,String),
        write(String),nl,
        write_attr_list(Resultlist), nl.

% Prints all conclusions set to 'true' which means  
% they have propability > 100
write_attr_list([]).
write_attr_list([(Attribute, Value)|Rest]):-
		evaluation(Value,Text),
        write(Attribute),
        write('     - evaluated as '),
        write(Text),nl,
        write_attr_list(Rest).

		
		
% Translates certainty factors to text.
evaluation(Value,'very probable'):-
        Value >= 800,!.
evaluation(Value,'probable'):-
        Value >= 600, !.
evaluation(Value,'possible'):-
        Value >= 400, !.
evaluation(Value,'slight possibility'):-
        Value >= 200, !.
evaluation(Value,'can not be excluded'):-
        Value >= 0,!.
evaluation(Value,'probably not'):-
        Value >= -500,!.
evaluation(Value,'definitely not').


% Annes code for dam construction work.
arrange_write_list([]).
arrange_write_list(Stringlista):-
        Stringlista = [List1,List2|Rest],
        write_list(List1),
        write_list(List2),
        arrange_write_list(Rest).
% Annes code.
arrange_write_list(Stringlista):-
        Stringlista = [List1|Rest],
        write_list(List1),
        arrange_write_list(Rest).

write_list([]).
write_list([First|Rest]):-
        write(First), write(' '),
        write_list(Rest).


store_answers:-
        new(Fil, Volym, 'On what file?'),
        create(Fil, Volym, 'TEXT'),
        open(Fil, Volym),
        tell(Fil),
        listing(answer),
        nl(Fil),
        told,
        close(Fil).

%*****************************************************************        
%*****************************************************************        
% Window 7 - Text base


% Text used at presentation of results.

% text/2.
%text(fix_server_congiguration,'Solution: See to server configuration').
%text(clear_database,'Solution: Clear the database.').
%text(turn_on_server,'Solution: Turn on the server.').
%text(fix_firewall,'Solution: See to firewall settings.').
%text(virtual_machine,'Solution: Turn on the virtual machine.').
%text(contact_IP,'Solution: Contact internet provider.').
%text(fix_network,'Solution: See to network settings.').
%text(fix_linux,'Solution: See to linux settings.').
%text(maintenance,'Solution: Contact HSSD').

text(solutions_for_webpage_errors, 'Solutions for webpage errors can be following:').

%text(change_ip_and_reconfigure_data_logger,'Solution: Change IP and reconfigure data logger').
%text(enable_sensors,'Solution: Enalbe the sensor').
%text(fix_firewall_audit_settings, 'Solution: Fix firewall audit setting').
%text(fix_vlan_network_settings, 'Solution: Fix VLAN network settings').
%text(fix_cabinet_power, 'Solution: Fix the cabinet power').
%text(fix_internal_fuse, 'Solution: Fix or replace the internal fuse.').
%text(reset_reconfigure_IO, 'Solution: Reset and reconfigure the IO.').
%text(fix_configuration, 'Solution: Fix configuration of.....').
%text(reset_reconfigure_datalogger, 'Solution: Reset and reconfigure the datalogger.').
%text(use_spare_IO, 'Solution: Install another IO.').
%text(fix_barrier_device_readings, 'Solution: Reset the barrier device readings.').
%text(use_spare_cables, 'Solution: Use another cable').
%text(fix_power_issues, 'Solution: See to issues with power').

text(solutions_for_value_errors, 'Solutions for value errors can be following:').





%*****************************************************************
%*****************************************************************
% Window 8 - Text base

not_check(Question,Condition,Answer):-
        check(Question,Condition,Answer),!,fail.


not_check(Object,Condition,Answer).

not_check(Object,Subject,CF,Amount):-
        check(Object,Subject,CF,Amount),
        !, fail.

not_check(Object,Subject,CF,Amount).


%*****************************************************************
%*****************************************************************
% Window 9 - Explanations

% ************ Why ? *****************
%Gives a text based explanation to why a certain question has been asked.

why_explanation(Object):-
        definition(Object,Text),!,
        write('Why explanation '),
        write(Object),nl,
        write(Text),nl.
why_explanation(Object):-
        write('No definition is given'),nl.


% *************  How ?  ******************

% Answers how-questions.
answer_howquestion:-
		\+tcl_GUI(on),
        read_which_how(Hur_object),
        collect_howansers(Hur_object,Hur_regler),
        present_how( Hur_regler).
answer_howquestion:-
		tcl_GUI(on),
        read_which_how(Hur_object), 
        collect_howansers(Hur_object,Hur_regler),
        present_how( Hur_regler).
		
% Asks what conclusion to be explained.
read_which_how(How_object):-
        findall(Object, rule_info(Object, Regellista), List),
        find_fired_objects(List, List_of_answers),
        arrange_how_list(List_of_answers, How_object).

arrange_how_list([Object], Object):-
		\+tcl_GUI(on),
        write('This conclusion exist:'),nl,
        menue_write([Object]).

arrange_how_list(List_of_answers, How_object):-
        \+tcl_GUI(on),
		write('These conclusions exists: '),nl,
        menue_write(List_of_answers),
        make_type_control(How_object).

%assert each conclusion and run a tcl window where user can pick a conclusion.
arrange_how_list(List_of_answers, How_object):-
        tcl_GUI(on),
		assert_each_option(List_of_answers),
		assert_num_options(List_of_answers),
		Source = 'source k:/klöver/pick_how.tcl',
		run_tcl(Source, Event),
		retract_and_destroy(Event),
		(
		Event = '', 		%if no conclusion was chosen
		How_object = 'no_valid_tcl_answer';	
		string_to_integer(Event, Int),
		get_single_answer(Int, List_of_answers,How_object)).
        		
make_type_control(Answer):-
        write('Which conclusion are you interested in? '),nl,
        read(Answer).

% Creates a list of all conclusions that have the value <> rejected.
find_fired_objects([],[]).
find_fired_objects([Object | List],[Object | List_of_answers]):-
        trigged(Object, Attribute, CF),
        CF =\= 0, !,
        find_fired_objects(List,List_of_answers).
find_fired_objects([Object | List],List_of_answers):-
        find_fired_objects(List,List_of_answers).

% Finds all rules used for an object and returns
% a list of rule numbers.
collect_howansers(How_object,Hur_regler):-
findall(Regelnr, (how(Regelnr,How_object,_,CF),CF \== rejected),Hur_regler).

% Prints out all rules that have put a value on how-object asked for.
present_how( []).
present_how( [Nr | Rest]):-
		nl,
        clause(rule(Nr,Object,Attribute,CF),Premise),
        how(Nr,Object,Attribute,CF2),
        CF2 \== rejected,
        write(rule(Nr,Object,Attribute,CF2)),
        write(' is true because: '), nl,
        create_prem_list(Premise, Premisslista),
        write_howpremises(Premisslista),nl,nl,
        present_how(Rest).

% Puts the premises in a rule as elements in a list.
% create_prem_list(+ premises from clause, - list of premises)
% For premises that are not compound (not in  parenthesis)
% No more premises.
create_prem_list(true, []).
% The first premis is followed by "and".
create_prem_list((A,B), [(A,and)|Rest]):-
        \+ connected(A),
        create_prem_list(B, Rest).
% The first premis is followed by an "or".
create_prem_list((A;B), [(A,or)|Rest]):-
        \+ connected(A),
        create_prem_list(B, Rest).
% Last premis.
create_prem_list(A, [(A, ' ')]):-
        \+ connected(A).
% If the premis is a compound it should be remade in the same manner.
% The first premis is followed by "and".
create_prem_list((A,B), [(A_List,and)|Rest]):-
        connected(A),
        create_prem_list(A, A_List),
        create_prem_list(B, Rest).
% The first premis is followed by an  "or".
create_prem_list((A;B),[(A_List, or)|Rest]):-
        connected(A),
        create_prem_list(A, A_List),
        create_prem_list(B, Rest).
% Last premis.
create_prem_list(A, [(A_List, ' ')]):-
        connected(A),
        create_prem_list(A, A_List).

%The premis is a nestled condition.
connected((A, B)).
connected((A; B)).


% Writes the premises to how and if they are given by the user or deduced.
write_howpremises([]).
write_howpremises([(Premise, Konnektor)| Rest]):-
		check_prem_type(Premise, Type),
        write_how_prem(Premise, Type), write(Konnektor), nl,
        write_howpremises(Rest).
		
/*
%Prints out a negated premis - r not implemented n.

write_how_prem(not Premise, no):-
        check_prem_type(Premise, Type),
        write_how_prem(Premise, Type),
        write('is not true').
*/
% Prints out a nestled (nastlad) premise.
write_how_prem(Premise, nested):-
		nl,
        write_howpremises(Premise).
% Prints out remaining premises.
write_how_prem(Premise, Type):-
        write(Premise), write(' is '), write(Type), write(' ').

% Checks the type a premis posesses.
check_prem_type(check(Object, Attribute, Condition, Value), concluded):-
        trigged(Object, _, _).
check_prem_type(not_check(Object, Attribute, Condition, Value), concluded).
check_prem_type(check(Object, Attribute, Truth_value), concluded):-
        (Truth_value = sant ; Truth_value = falskt),
        trigged(Object, _, _).
check_prem_type(check(Object, Condition, Attribute), 'given from the user'):-
        answer(Object, _).
check_prem_type(check(Object, Condition, Attribute), 'not answered'):-
        question_info(Object, Question, Alternativ, Type, Fragevillkor),
        \+ answer(Object, _).
%check_prem_type(not Premise, icke).
check_prem_type(Nested_premise, nested) :-
        list(Nested_premise).
check_prem_type(Premise, 'special conditions').

% Tests if the object is a list.
list([ ]).
list([A|B]). 



%*******************************************************************
%*******************************************************************
% Window 10 - Verification debugger tool

go_verify(Var):-
	var(Var),!,
	\+tcl_GUI(on),
    write('That option does not exist, please try again.'), nl,
	run_option('Verify rules').
	
%*************  Option 'Completeness' - rules not used and missing rules  ************
 
% 1. Unreachable rules: Checks that all rules (rule numbers) exists in specific clause of rule_info/2. 
% 2. Missing rules: Checks that all rule_info numbers got a connected rule (rule number).

go_verify('Completeness'):-
	newlines(20),
	non_reachable_rules,	%1. above
	missing_rules,			%2. above
	run_option('Verify rules').
	
missing_rules:-
		bagof(all(Object,Rules),rule_info(Object,Rules),RuleInfo_List),!, 
		nl,
		write('MISSING RULES'),nl,
		write('Check: if all numbers in rule_info/2 are represented as rule numbers in the rule base:'),nl,
		check_rule_no(RuleInfo_List),
		retractall(missing_rules(_,_)),
		write('**************************************'),nl.
	
missing_rules:-
		\+bagof(all(Object,Rules),rule_info(Object,Rules),RuleInfo_List),!, 
		write('rule_info/2 predicate are missing or is syntactically incorrect!'),nl,
		write('*************************************'),nl.
		%write('No rules can be reached'),nl.

%missing rule check		
% checks each rule_info(Object, List_of_numbers) if numbers have corresponding rules.		
	check_rule_no([]):- nl.
	
	check_rule_no([E|Rest]):-
		all(Object,Numbers) = E,
		check_no(Object,Numbers),
		output_missing_rule(Object),
		check_rule_no(Rest).

% check each number in rule_info/2 list if a corresponding rule exists (with that Oject and % Number), 
% if not - assert.
		check_no(_,[]).
		
		check_no(Object,[No|Rest]):-
			clause(rule(No,Object,_,_),_),
			check_no(Object,Rest).
		
		check_no(Object,[No|Rest]):-
			\+ clause(rule(No,Object,_,CF),_),
			assert(missing_rules(Object,No)),
			check_no(Object,Rest).

%**********************************************

non_reachable_rules:-
		findall((R_Num,Object),Y^Z^clause(rule(R_Num,Object,Y,Z), Body),Rule_Num), 
		Rule_Num \= [],
		nl,
		write('UNREACHABLE RULES'),nl,
		unique_check(Rule_Num),
		non_reachable_check(Rule_Num).
		
non_reachable_rules:-
		findall([R_Num,Object],Y^Z^clause(rule(R_Num,Object,Y,Z), Body),Rule_Num), 
		Rule_Num == [],
		write('No rules have been added to the rule base, or the format of the rules do not follow the format of rule/4'),nl,
		write('*************************************'),nl,nl.


%check if conclusion and rule numbers are unique, if not, 
%store and return specific rules in a list (unreachable check).	

	unique_check(Rule_Num):-
		unique(Rule_Num),
		write('Check 1: if all rules in the rule base have unique rule numbers'),nl,
		output_not_uniquenum,
		retractall(not_unique(_)),
		nl.
		
		unique([]).

		unique([Rule|Rest]):-
			members(Rule,Rest),!,
			assert(not_unique(Rule)),
			unique(Rest).

		unique([Rule|Rest]):-
			\+members(Rule,Rest),
			unique(Rest).	
	
	
%check of unreachable rules
	non_reachable_check(Rule_Num):-
		check_goal_conclusions,
		check_rinfo_no(Rule_Num),
		output_missingnum,
		output_missing_goal_conclusion,
		output_dead_end_goal,
		write('*************************************'),nl,nl.
	

		check_goal_conclusions:-
				findall(Object,List^rule_info(Object,List),Rule_infos), 
				Rule_infos \= [],
				check_goals(Rule_infos).
	
		check_goal_conclusions:-
				findall(Object,List^rule_info(Object,List),Rule_infos), 
				Rule_infos == [].
	
	
			check_goals([]).
			
			check_goals([Object|Rest]):-
				goal_conclusions(Goals),
				members(Object,Goals),
				check_goals(Rest).
				
			check_goals([Object|Rest]):-
				goal_conclusions(Goals),
				\+members(Object,Goals),
				assert(missing_goal(Object)),
				check_goals(Rest).
				
	
% check each rule if missing in rule_info/2	
		check_rinfo_no([]).

		check_rinfo_no([E|Rest]):-
			check_rule_info(E),!,
			check_rinfo_no(Rest).
		
		check_rinfo_no([(Num,Object)|Rest]):-
			assert(non_reached_rule(Num,Object)),
			check_rinfo_no(Rest).

% checks if a rule number is member of rule_info/2 		
			check_rule_info((Nr,Object)):-
				rule_info(Object,Numbers),
				members(Nr,Numbers).

%if rule_info/2 missing for object, then assert as a dead-end goal, and check as well if object is missing in goal_conclusions.	
			check_rule_info((Nr,Object)):-
				\+rule_info(Object,_),
				assert(no_goal(Object)),
				check_goals([Object]).


count_elems([],0):-!.

count_elems([H|T], Count):-
	count_elems(T, Sum),
	Count is Sum + 1.

members(Elem,[Elem|_]).

members(E,[F|Rest]):-
	\+ E = F,
	members(E,Rest).


%*****************************************************************************
%*************************  Redundancy check ***********************************		

go_verify('Redundancy'):-
	newlines(20),
	findall([rule(Num,Obj,Val,CF),Prem],clause(rule(Num,Obj,Val,CF),Prem),R_List),
	write('Redundant rules:'),nl,nl,
	redundant_check(R_List),
	run_option('Verify rules').

% redundant_check(List)
% List = all rules in rule base
% Goes through all rules and search for redundant rules in rest of the list
	redundant_check([]).

	redundant_check([[E,F|_]|Rest]):-
		count_premises(F,Number),
		check_redundancy(E,F,Number,Rest,New_Rest),
		output_redundancy(E,F),
		retractall(redundant_rule(_,_)),
		redundant_check(New_Rest).

%count_premises/2
% Counts number of premises for a rule,
% Example: Premises: (A,(B;C),D) equals 4.
% Main reason for this is to increase effectiveness of redundancy check
% - if two rules have different number of premises they can never be redundant!

		count_premises((P,P1),N):-
			count_premises(P,LP),
			count_premises(P1,LP1),
			!,
			N is LP + LP1.
		
		count_premises((P;P1),N):-
			count_premises(P,LP),
			count_premises(P1,LP1),
			!,
			N is LP + LP1.

%if current premise is instantiated (not end of premises) and not multiple premises then count 1.		
		count_premises(P,1):-
			\+ P = (_,_),
			\+ P = (_;_),
			ground(P),
			!.

% Base case: if current premise is uninstantiated (end of premises) then 0 and start backtracking.
		count_premises(P,0):-
			\+ P = (_,_),
			\+ P = (_;_),
			\+ ground(P),
			!.
		

% check_redundancy/5
% check_redundancy(Rule_N, Premises_N, Nr_of_premises_N, Rest_of_rules, New_Rest),
% compares one rule with all rules in Rest_of_rules.
% if redundant then remove rule from New_Rest -> efficiency and clean output.

			check_redundancy(_,_,_,[],[]).

%Add ruleT (rule + premises) to Answer if ruleN = ruleT AND nr_premises_N = nr_premises_T %AND premises_N redundant with premises_T.
%Does not add ruleT to Rest2.  		
			check_redundancy(rule(Nr,Obj,Val,CF),Clause,N1,[[E,F]|Rest],Rest2):-
				rule(_,Obj,Val,_)=E,
				count_premises(F,N2),
				N1 == N2,
				compare_clauses(Clause,F),
				assert(redundant_rule(E,F)),
				check_redundancy(rule(Nr,Obj,Val,CF),Clause,N1,Rest,Rest2).
		
			check_redundancy(rule(Nr,Obj,Val,CF),Clause,ClauseList,[[E,F]|Rest],[[E,F]|Rest2]):-
				check_redundancy(rule(Nr,Obj,Val,CF),Clause,ClauseList,Rest,Rest2).
		

% compare_clauses/2
% compare_clauses(Rule1_Premises, Rule2_Premises)
% Checks if premises of two rules are redundant, example: (A,(B;C);D) is redundant with (D;A,(B;C))
% compare_clauses/2 fails if premises are not redundant.

%called when last premise (P3) is multiple premises of structure: (A;B)
%Investigate first premise (P1), then rest of premises (P2) except last premise (P3)
%which is investigated last.
			compare_clauses(((P1);(P2,P3)), Body):-
				P3 = (A;B),!,
				find_structure_OR(((P1);(P2,P3)), Body,Body1,Body2),
				compare_clauses(P1, Body1),	
				compare_clauses(P2, Body2),
				find_structure_parenthes((A;B),Body2).

%if Body contains OR (;), check if P1 elements are members of either Body´s left side of OR %(;),
% or right side, do the same with P2.  
			compare_clauses(((P1);P2), Body):-
				find_structure_OR(((P1);P2),Body,Body1,Body2),
				compare_clauses(P1, Body1),
				compare_clauses(P2, Body2).	

%called when last premise (P3) is multiple premises of structure: (C;D)
% and current first premise (P1) is multiple premises (A;B)
%Investigate first premise (P1), then rest of premises (P2) except last premise (P3)
%which is investigated last. 
			compare_clauses((P1,(P2,P3)), Body):-
				P1 = (A;B),!,
				P3 = (C;D),!,
				find_structure_parenthes((A;B),Body),
				compare_clauses(P2, Body),
				find_structure_parenthes((C;D),Body).
		
%if P1 is a structure of premises in parentheses separated by or (;), then check if Body
% contain the same structure.
			compare_clauses((P1,P2), Body):-
				P1 = (A;B),!,
				find_structure_parenthes((A;B),Body),
				compare_clauses(P2, Body).

			compare_clauses((P1,(P2,P3)), Body):-
				P3 = (A;B),!,
				\+ P1 = (_,_),
				\+ P1 = (_;_),
					find_member(P1,Body), !,
				compare_clauses(P2, Body),
				find_structure_parenthes((A;B),Body).
		
%check if non-nested premise P1 is a member in Body		
			compare_clauses((P1,P2), Body):-
				\+ P1 = (_,_),
				\+ P1 = (_;_),
				find_member(P1,Body),
				compare_clauses(P2, Body).

%check if P (single premise) is member in Body
			compare_clauses(P, Body):-
				\+ P = (_,_),
				\+ P = (_;_),
				find_member(P, Body),!.
		
% if P is single premise and not member in body then compare_clauses fails 
% (rules are not redundant)
			compare_clauses(P, Body):-
				\+ P = (_,_),!,
				\+ P = (_;_),!,
				fail.


% find_structure_OR/4
% checks if structure (_;_) is member in premises of second rule 
% if member - return to compare_clauses left side premises of ; as Body1, return right side premises of ; as Body2,
% or if backtracking occurs - return left side of ; as Body2 and right side of ; as Body1
		
				find_structure_OR(((_);_), ((X);Y),X,Y).
				find_structure_OR(((_);_), (X;(Y)),Y,X).

% if current Body = (P1,P2) continue search for structure (_;_) in P2
				find_structure_OR(((_);_), (_,Y),_,_):-
					find_structure_OR(((_);_), Y,_,_).

% last single premise then fail (no premises connected by OR operator found)		
				find_structure_OR(((_);_), X,_,_):-
					\+ X = (_,_),
					\+ X = (_;_),
					fail,!.

% find_structure_parenthes/2
% search for structure ((_;_)) in Premises2.
% Example: Premises2 = (A,(B;C),D) -> structure ((_;_)) = ((P1;P2))
% if finding structure then continue search for P1 and P2 in this structure.
 
				find_structure_parenthes((P1;P2), (X,_)):-
					X = (_;_),!,
					find_structure_OR(((_);_), X,Body1,Body2),
					compare_clauses(P1, Body1),
					compare_clauses(P2, Body2).

				find_structure_parenthes((P1;P2), (X;_)):-
					X = (_;_),!,
					find_structure_OR(((_);_), X,Body1,Body2),
					compare_clauses(P1, Body1),
					compare_clauses(P2, Body2).

%structure not found in X then search Y				
				find_structure_parenthes((A;B), (X,Y)):-
					\+ X = (_;_),
					find_structure_parenthes((A;B), Y).

%structure not found in X then search Y
				find_structure_parenthes((A;B), (X;Y)):-
					\+ X = (_;_),
					find_structure_parenthes((A;B), Y).

%last single premise in Premise2 then fail. 		
				find_structure_parenthes((_;_), X):-
					\+ X = (_,_),
					\+ X = (_;_),
					fail,!.


% find_member/2
% no base case for scenario (P1, (P1;_)) because structure (_;_) is dealt with in find_structure2.
		
				find_member(P1, (P1,_)).

				find_member(P1,P1).

				find_member(P,(X,Y)):-
					\+ P = X,
					find_member(P,Y).

				find_member(P,(_;Y)):-
					find_member(P,Y).
						
				find_member(P,X):-
					\+ X = (_,_),
					\+ X = (_;_),
					\+ P = X,
					fail,!.
	
%***********************************************************
%*****************  Option 'Subsumed' - checks for subsumed rules  ***************

go_verify('Subsumption'):-
	newlines(20),
	findall([rule(Num,Obj,Val,CF),Prem],clause(rule(Num,Obj,Val,CF),Prem),R_List),
	nl,
	write('Subsumed rules:'),nl,nl,
	subsumed_check(R_List),
	run_option('Verify rules').


	subsumed_check([]).
	subsumed_check([[E,F]|Rest]):-
		count_premises(F,N1),
		check_subsumed(E,F,N1,Rest),
		output_subsumed,
		retractall(subsumed_rules(_,_,_,_)),
		subsumed_check(Rest).

		check_subsumed(_,_,_,[]).
		%when CF of both rules are positive		
		check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,[[rule(Nr2,Obj2,Val2,CF2),F]|Rest]):-
			rule(_,Obj,Val,_)= rule(_,Obj2,Val2,_),
			CF1 >= 0,
			CF2 >= 0,
			CF1 =< CF2,
			count_premises(F,N2),
			N1 > N2,
			compare_clauses(F,Clause),
			assert(subsumed_rules(rule(Nr,Obj,Val,CF1),Clause,rule(Nr2,Obj2,Val2,CF2),F)),
			%output_subsumed(rule(Nr,Obj,Val,CF1),Clause,rule(Nr2,Obj2,Val2,CF2),F),
			check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,Rest).

		%when CF of both rules are positive	
		check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,[[rule(Nr2,Obj2,Val2,CF2),F]|Rest]):-
			rule(_,Obj,Val,_)= rule(_,Obj2,Val2,_),
			CF1 >= 0,
			CF2 >= 0,
			CF1 >= CF2,
			count_premises(F,N2),
			N1 < N2,
			compare_clauses(Clause,F),
			assert(subsumed_rules(rule(Nr2,Obj2,Val2,CF2),F,rule(Nr,Obj,Val,CF1),Clause)),
			%output_subsumed(rule(Nr,Obj,Val,CF1),Clause,rule(Nr2,Obj2,Val2,CF2),F),
			check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,Rest).
	
		%when CF of both rules are negative
		check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,[[rule(Nr2,Obj2,Val2,CF2),F]|Rest]):-
			rule(_,Obj,Val,_)= rule(_,Obj2,Val2,_),
			CF1 < 0,
			CF2 < 0,
			CF1 >= CF2,
			count_premises(F,N2),
			N1 > N2,
			compare_clauses(F,Clause),
			assert(subsumed_rules(rule(Nr,Obj,Val,CF1),Clause,rule(Nr2,Obj2,Val2,CF2),F)),
			%output_subsumed(rule(Nr,Obj,Val,CF1),Clause,rule(Nr2,Obj2,Val2,CF2),F),
			check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,Rest).

		%when CF of both rules are negative	
		check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,[[rule(Nr2,Obj2,Val2,CF2),F]|Rest]):-
			rule(_,Obj,Val,_)= rule(_,Obj2,Val2,_),
			CF1 < 0,
			CF2 < 0,
			CF1 =< CF2,
			count_premises(F,N2),
			N1 < N2,
			compare_clauses(Clause,F),
			assert(subsumed_rules(rule(Nr2,Obj2,Val2,CF2),F,rule(Nr,Obj,Val,CF1),Clause)),
			%output_subsumed(rule(Nr,Obj,Val,CF1),Clause,rule(Nr2,Obj2,Val2,CF2),F),
			check_subsumed(rule(Nr,Obj,Val,CF1),Clause,N1,Rest).
		
		check_subsumed(rule(Nr,Obj,Val,CF),Clause,ClauseList,[[E,F]|Rest]):-
			check_subsumed(rule(Nr,Obj,Val,CF),Clause,ClauseList,Rest).

%********************************************************
%Help - explanation of automated verification tool

go_verify('Help'):-
	\+tcl_GUI(on),
	write('This is a beta version of an automated verification tool customized for the rule-based system shell Klöver.'),nl,
	write('The purpose of this tool is to help the rule-base developer test his/her rules for consistency and completeness.'),nl,
	write('It automatically detect and present errors and anomalies found in the rule-base.'),nl,nl,
	write('IMPORTANT! The verification tool does not alter the developers rule-base in any way!'),nl,nl,
	write('This verification tool is not fully implemented to check for all possible completeness and consistency errors.'),nl,nl,nl,
	go_help('Start'),
	run_option('Verify rules').
go_verify('Help'):-
	tcl_GUI(on),
	Source = 'source k:/klöver/v_help.tcl',
	run_tcl(Source, Event),
	retract_and_destroy(Event),
	go_help(Event).

%**********
%go_help/1
%**********
	
go_help(Var):-
	var(Var),!,
	go_help('invalid').
	
go_help('Start'):-
	write('************************************************'),nl,nl,
	write('HELP MENU'),nl,nl,
	write('The options below give you more information of the specific implemented error checkers:'),nl,nl,
	menue_write(['Redundancy','Subsumed','Completeness','Quit help']),
	read(Input),
	newlines(20),
	go_help(Input).
	

go_help('Redundancy'):-
	\+tcl_GUI(on),
	write('The "Redundancy" option detects:'),nl,nl,
	write('Syntactical redundant rules in the rule-base'),nl,
	write('   -Two syntactical redundant rules are two identical rules with identical premises and identical conclusions, belonging to the same rule class.'),nl,nl,nl,
	write('Options:'),nl,
	menue_write(['See example','Back']),
	read(Input),
	%newlines(20),
	go_more_info(Input,'Redundancy').
go_help('Redundancy'):-
	tcl_GUI(on),
	Source = 'source k:/klöver/r_help.tcl',
	run_tcl(Source, Event),
	retract_and_destroy(Event),
	go_help(Event).

go_help('Subsumed'):-
	\+tcl_GUI(on),
	write('Klöver is a rule-based system utilizing inexact reasoning by implementing Certainty Factors (CF).'),nl,
	write('When using Certainty Factors, two subsumed rules are two rules with identical conclusions in the same rule class, where the conditions of the'),nl, 
	write('first rule is a subset of the conditions of the second rule, and where the CF of the first rule is greater than than the CF of the second rule.'),nl,nl,
	write('Subsumed rules with CF can sometimes be utilized by purpose. However, according to the definition above, subsumed rules may be an indication of non-intended developer-induced errors, or misinterpretation of knowledge.'),nl,nl,	
	write('Options:'),nl,
	menue_write(['See example','Back']),
	read(Input),
	%newlines(20),
	go_more_info(Input,'Subsumed').
go_help('Subsumed'):-
	tcl_GUI(on),
	Source = 'source k:/klöver/s_help.tcl',
	run_tcl(Source, Event),
	retract_and_destroy(Event),
	go_help(Event).	
	
go_help('Completeness'):-
	\+tcl_GUI(on),
	write('The "Completeness" option detects:'),nl, 
	write('1. Unreachable rules.'),nl,
	write('2. Missing rules.'),nl,nl,
	write('The tool checks:'),nl,
	write('(1)if there exist rules with identical rule numbers and objects in the rule base.'),nl,
	write('(1)if there are rule numbers that does not exist in rule_info/2.'),nl,
	write('(1)if each object in the rule-base are represented in a clause of rule_info/2.'),nl,
	write('(1)if each object in the rule-base are represented in the list of goal_conclusions/1.'),nl,
	write('(2)if rule numbers present in rule_info/2 does not have a correspondent rule in the rule base.'),nl,nl,nl,  
	write('Options:'),nl,
	menue_write(['See example','Back']),
	read(Input),
	%newlines(20),
	go_more_info(Input,'Completeness').
go_help('Completeness'):-
	tcl_GUI(on),
	Source = 'source k:/klöver/c_help.tcl',
	run_tcl(Source, Event),
	retract_and_destroy(Event),
	go_help(Event).	
	
go_help('Back'):-
	\+tcl_GUI(on),
	newlines(20),
	go_help('Start').
go_help('Back'):-
	tcl_GUI(on),
	go_verify('Help').
	
go_help('Quit help'):-
	\+tcl_GUI(on),
	newlines(20).
go_help('Quit help'):-
	tcl_GUI(on),
	run_option('Verify rules').

go_help(Error):-
	\+tcl_GUI(on),
	write('That option does not exist.'),nl,nl,
	go_help('Start').

%**************
%go_more_info/2
%**************	

	go_more_info(Var,Option):-
		var(Var),!,
		write('That option does not exist.'),nl,nl,
		go_help(Option).
	
	go_more_info('See example','Redundancy'):-
		write('%rule(Rule_Number, Rule_Class, Rule_Attribute, Certainty_factor)'),nl,nl,
		write('Example of two syntactical redundant rules in Klöver:'),nl,nl,
		write('rule(1, Rule_Class2, Attribute1, 400):-'),nl,
		write('premise(A),'),nl,
		write('premise(B).'),nl,nl,
		write('redundant with:'),nl,nl,
		write('rule(2, Rule_Class2, Attribute1, 600):-'),nl,
		write('premise(B),'),nl,
		write('premise(A).'),nl,nl,nl,
		write('Options:'),nl,
		menue_write(['Back']),
		read(Input),
		newlines(20),
		go_more_info(Input,'Redundancy').
	
	go_more_info('See example','Subsumed'):-
		write('%rule(Rule_Number, Rule_Class, Rule_Attribute, Certainty_factor)'),nl,nl,
		write('Example of two subsumed rules in Klöver:'),nl,nl,
		write('rule(1, Rule_Class1, Attribute1, 400):-'),nl,
		write('premise(B),'),nl,
		write('premise(A).'),nl,nl,
		write('subsumed by:'),nl,nl,
		write('rule(2, Rule_Class1, Attribute1, 600):-'),nl,
		write('premise(A).'),nl,nl,nl,
		write('Options:'),nl,
		menue_write(['Back']),
		read(Input),
		newlines(20),
		go_more_info(Input,'Subsumed').
	
	go_more_info('See example','Completeness'):-
		write('%rule(Rule_Number, Object1, Attribute, Certainty_factor)'),nl,nl,
		write('Example of two rules with duplicated rule numbers and conclusions in Klöver:'),nl,nl,
		write('rule(10, Object1, Attribute1, 400):-'),nl,
		write('premise(A),'),nl,
		write('premise(B).'),nl,nl,
		write('has the same rule number and object as:'),nl,nl,
		write('rule(10, Object1, Attribute2, 600):-'),nl,
		write('premise(C).'),nl,nl,
		write('The second rule will never be reached when the first rule succeeds.'),nl,nl,
		write('*****************************************************'),nl,nl,
		write('Example of an unreachable rule in Klöver:'),nl,nl,
		write('rule_info(Object1,[1,3,4,5,6,7,8,9]).'),nl,nl,
		write('rule(2,Object1, Attribute1,600):-'),nl,
		write('premise(A).'),nl,nl,
		write('The rule number 2 is not represented in the clause of rule_info/2.'),nl,nl,
		write('***************************************************************'),nl,nl,
		write('Example of a missing clause of rule_info/2 in Klöver:'),nl,nl,
		write('rule_info(Object2,[3]).'),nl,nl,
		write('rule(1,Object1, Attribute1,600):-'),nl,
		write('premise(A).'),nl,nl,
		write('rule(3,Object2, Attribute1,200):-'),nl,
		write('premise(B).'),nl,
		write('.......................................'),nl,nl,
		write('A clause of rule_info/2 for Object1 is missing.'),nl,nl,
		write('***************************************************************'),nl,nl,
		write('Example of a missing object in goal_conclusions/1 in Klöver:'),nl,nl,
		write('goal_conclusions([Object1])'),nl,nl,
		write('rule(1,Object1, Attribute1,600):-'),nl,
		write('premise(A).'),nl,nl,
		write('rule(3,Object2, Attribute1,200):-'),nl,
		write('premise(B).'),nl,nl,
		write('Object2 is missing in goal_conclusions/1.'),nl,nl,
		write('***************************************************************'),nl,nl,
		write('Example of a missing rule in Klöver:'),nl,nl,
		%write('(when a number in rule_info\2 does not have a correspondent rule in the rule base)'),nl,nl,
		write('rule_info(Rule_Class2,[10,20,30,]).'),nl,nl,
		write('%Rule base:'),nl,
		write('rule(10,Rule_Class2, Attribute1,600):-'),nl,
		write('premise(A).'),nl,nl,
		write('rule(30,Rule_Class2, Attribute1,600):-'),nl,
		write('premise(B).'),nl,nl,
		write('There is no rule in the rule base with the rule number 20.'),nl,nl,nl,
		write('Options:'),nl,
		menue_write(['Back']),
		read(Input),
		newlines(20),
		go_more_info(Input,'Completeness').
	
	go_more_info('Back',_):-
		newlines(20),
		go_help('Start').
		
	go_more_info(Error,Option):-
		write('That option does not exist.'),nl,nl,
		go_help(Option).
		


%*************************************************************
%Quit to main menu

go_verify('Back').
	
%*************************************************************
%Misspelled input

go_verify(Answer):-
		\+tcl_GUI(on),
        write('That option does not exist, please try again.'), nl,
		run_option('Verify rules').
		

%**************************************************************
%******************  Output debugger  ******************

newlines(0) :- !.
newlines(N) :-
    N > 0,
    nl,
    N1 is N - 1,
    newlines(N1).

%*****************************************************
%Output results for Completeness check:

% output_missing_rule/1
% Output of numbers and object in rule_info/2 that do not have a connected rule
output_missing_rule(Object):-
	\+missing_rules(Object,_),
	nl,
	write('No '),
	write(Object),
	write(' rules are missing in the rule base.'),nl,nl.
	
output_missing_rule(Object):-
	nl,
	write('The following '),
	write(Object),
	write(' rules are missing in the rule base:'),nl,
	\+output_missing_asserted(Object).
				

output_missing_asserted(Object):-
	missing_rules(Object,Number),
	write('rule '),
	write(Number), nl, fail.
	
%******************************
% Output rules that have the same rulenumber and conclusion. 		

output_not_uniquenum(_):-
	\+not_unique(_),!,
	nl,
	write('All rule numbers are unique.'),nl,nl,
	write('*************************************'),nl,nl.

output_not_uniquenum:-
	nl,
	write('Rule numbers must be unique.'), nl,nl,
	write('These rules have not unique rule numbers: '), nl,
	\+output_not_unique_asserted,
	nl,
	write('*************************************'),nl.

output_not_unique_asserted:-
	not_unique(Rule),
	write('rule '),
	write(Rule), nl, fail.
	
%*********************************
% Output rules which numbers does not exist in corresponding rule_info/2.

output_missingnum:-		
	bagof(Object,Rules^rule_info(Object,Rules),Objects),
	write('Check 2: if all rule numbers in the rule base are represented in rule_info/2'),nl,nl,
	write('Rule numbers missing in rule_info/2: '),nl,nl,
	output_each_rule_info(Objects),nl,
	retractall(non_reached_rule(_,_)).
	
output_missingnum:-
	\+bagof(Object,Rules^rule_info(Object,Rules),Objects),!,
	nl.
	
output_each_rule_info([]).
output_each_rule_info([Object|Rest]):-
	\+non_reached_rule(_,Object),!,
	write('rule_info('),
	write(Object),write(', '),
	write('No numbers missing.'),
	write(')'),nl,nl,
	output_each_rule_info(Rest).
	
output_each_rule_info([Object|Rest]):-
	write('rule_info('),
	write(Object),
	write(', ['),
	non_reached_rule(Number,Object), %get first asserted anomalie
	write(Number),
	retract(non_reached_rule(Number,Object)), %delete first asserted anomalie
	\+output_reachable_asserted(Object),	%output rest of asserted anomalies
	write('])'),nl,nl,
	output_each_rule_info(Rest).
	
output_reachable_asserted(Object):-
		non_reached_rule(Number,Object),
		write(','),write(Number), fail.

%********************************************		
% Output missing Objects in list of goal_conclusions/1:	
output_missing_goal_conclusion:-
	\+missing_goal(_),!.
	
output_missing_goal_conclusion:-
	setof(Object,missing_goal(Object),Missing_Goals),
	output_missing_goals(Missing_Goals),
	retractall(missing_goal(_)),nl.

output_missing_goals([]).
output_missing_goals([Object|Rest]):-
	write('Object: "'),
	write(Object),
	write('" is missing in list of goal_conclusions/1!'),nl,nl,
	output_missing_goals(Rest).

%************************************************
% Output missing clauses of rule_info/2:	
output_dead_end_goal:-
	\+no_goal(_),!.
	
output_dead_end_goal:-
	setof(Object,no_goal(Object),Dead_Ends),
	output_dead_ends(Dead_Ends),
	retractall(no_goal(_)).

output_dead_ends([]).
output_dead_ends([Object|Rest]):-
	write('clause: rule_info('),
	write(Object),
	write(',[Numbers'),
	write(']) does not exist!'),nl,nl,
	output_dead_ends(Rest).

%**************************************************	
%**************************************************
%Output results for Redundancy check:
	
% Output information about redundant rules
output_redundancy(_,_):-
	\+redundant_rule(_,_),!.
	
output_redundancy(E,F):-
	write(E),
	write(':-'),nl,
	write(F),nl,nl,
	\+output_redundant_asserted,
	nl,
	write('*************************************'),nl,nl.
	
output_redundant_asserted:-
		redundant_rule(Conclusion,Premises),
		write(Conclusion),
		write(':-'),nl,
		write(Premises),nl,nl, fail.
			
output_rule([]):-
	nl,
	write('*************************************'),nl,nl.
output_rule([E|Rest]):-
	output_each_rule(E),
	output_rule(Rest).
		
output_each_rule([E,F]):-
	write(E),
	write(':-'),nl,
	write(F),nl,nl.
		
output_each([]):- nl.
output_each([E|Rest]):-
	members(E,Rest),!,
	output_each(Rest).
output_each([E|Rest]):-
	write('rule '),
	write(E), nl,
	output_each(Rest).

%*********************************************
%*********************************************
%Output results for Subsumed check:

output_subsumed:-
	\+subsumed_rules(_,_,_,_).
	
output_subsumed:-
	\+output_subsumed_asserted.
	
output_subsumed_asserted:-
	subsumed_rules(Conclusion1,Prem1,Conclusion2,Prem2),
	write(Conclusion1),
	write(':-'),nl,
	write(Prem1),nl,nl,
	write('are subsumed by'),nl,nl,
	write(Conclusion2),
	write(':-'),nl,
	write(Prem2),nl,nl,
	write('***************************'),nl,nl,fail.
	
%*********************************************
%prints out elements in a list
test_print([]).
test_print([E|Rest]):-
	write(E), nl,
	test_print(Rest).
