
% grid size
size(7,7).

% grid walls 
% :-dynamic wall/2.

% ! note: either GRID 1 or GRID 2 must be commented at the same time

% ? GRID NUMBER (1).
% wall(1,3).
% wall(3,3).
% wall(3,5).
% wall(3,7).
% wall(5,1).
% wall(5,3).
% wall(5,5).
% wall(7,5).

% ? GRID NUMBER (2).
wall(1,3).
wall(1,4).
wall(3,3).
wall(3,5).
wall(3,7).
wall(4,1).
wall(4,7).
wall(5,1).
wall(5,3).
wall(5,5).
wall(7,4).
wall(7,5).

% grid numered walls
% :-dynamic wall_num/3.

% ? GRID NUMBER (1).
% wall_num(1,3,3).
% wall_num(3,3,1).
% wall_num(5,3,1).
% wall_num(5,5,2).

% ? GRID NUMBER (2).
wall_num(1,4,1).
wall_num(3,3,3).
wall_num(4,7,2).
wall_num(5,3,0).
wall_num(7,4,0).

:-dynamic light/2.

% ? GRID NUMBER (1).
% fr assigned initially and should be removed after a while
 light(1,2).
 light(1,4).
 light(2,3).
 light(3,1).
 light(4,5).
 light(5,6).
 light(6,3).
 light(7,1).
 light(7,7).

% ? GRID NUMBER (2).
 light(1,1).
 light(1,5).
 light(2,3).
 light(3,2).
 light(3,4).
 light(4,6).
 light(5,7).
 light(6,5).
 light(7,1).

% the cell definition 
cell(X,Y):-
    X>0,
    X<8,
    Y>0,
    Y<8.

% top neighbor
neighbor_top(X,Y,[[NX,Y]]):-
    NX is X-1,
    cell(NX,Y),!.

neighbor_top(_,_,[]).


% bottom neighbor
neighbor_bottom(X,Y,[[NX,Y]]):-
    NX is X+1,
    cell(NX,Y),!.

neighbor_bottom(_,_,[]).

% left neighbor
neighbor_left(X,Y,[[X,NY]]):-
    NY is Y-1,
    cell(X,NY),!.

neighbor_left(_,_,[]).

% right neighbor
neighbor_right(X,Y,[[X,NY]]):-
    NY is Y+1,
    cell(X,NY),!.

neighbor_right(_,_,[]).
% ? procedure to get all neighbors of a cell(X,Y)

neighbors(X,Y,L) :-
    neighbor_top(X,Y,T),
    neighbor_bottom(X,Y,B),
    neighbor_left(X,Y,LE),
    neighbor_right(X,Y,R),
    append(T,B,TB),
    append(LE,R,LER),
    append(LER,TB,L).

% cells on the same row to the right
hor_right_cells(_,7,[]).

hor_right_cells(X,Y,L):-
    NY is Y + 1,
    cell(X,NY),
    not(wall(X,NY)),!,
    append([[X,NY]],L1,L),
    hor_right_cells(X,NY,L1).

hor_right_cells(X,Y,[]):-
    NY is Y + 1,
    cell(X,NY).

% cells on the same row to the left
hor_left_cells(_,1,[]).

hor_left_cells(X,Y,L):-
    NY is Y - 1,
    cell(X,NY),
    not(wall(X,NY)),!,
    append([[X,NY]],L1,L),
    hor_left_cells(X,NY,L1).

hor_left_cells(X,Y,[]):-
    NY is Y - 1,
    cell(X,NY).

% ? get cells on same row
hor_cells(X,Y,L):-
    hor_left_cells(X,Y,L1),
    hor_right_cells(X,Y,L2),
    append(L1,L2,L).

% ? cells on the same col to the bottom
ver_bottom_cells(7,_,[]).

ver_bottom_cells(X,Y,L):-
    NX is X + 1,
    cell(NX,Y),
    not(wall(NX,Y)),!,
    append([[NX,Y]],L1,L),
    ver_bottom_cells(NX,Y,L1).

ver_bottom_cells(X,Y,[]):-
    NX is X + 1,
    cell(NX,Y).

% ? cells on the same row to the top
ver_top_cells(1,_,[]).

ver_top_cells(X,Y,L):-
    NX is X - 1,
    cell(NX,Y),
    not(wall(NX,Y)),!,
    append([[NX,Y]],L1,L),
    ver_top_cells(NX,Y,L1).

ver_top_cells(X,Y,[]):-
    NX is X - 1,
    cell(NX,Y).

ver_cells(X,Y,L):-
    ver_bottom_cells(X,Y,L1),
    ver_top_cells(X,Y,L2),
    append(L1,L2,L).


% ? get path
cell_path(X,Y,L) :-
    hor_cells(X,Y,L1),
    ver_cells(X,Y,L2),
    append(L1,L2,L).



%? get number of lights in list of cells
num_lights([],0).

num_lights([[X,Y]|T],A):-
    light(X,Y),!,
    num_lights(T,Z),
    A is Z + 1.

num_lights([[X,Y]|T],Z):-
    num_lights(T,Z).

% ? is the cell which specified with (X,Y) is lighted
is_lighted(X,Y):-
    light(X,Y),!.

is_lighted(X,Y):-
    cell_path(X,Y,L),
    num_lights(L,A),
    A > 0,!.

is_lighted(X,Y):-
    wall(X,Y),!.

check_num_lights(X,Y,N):-
    neighbors(X,Y,L),
    num_lights(L,N).


% ? make sure all cells in the row are lighted
is_row_lighted(_,8).

is_row_lighted(Row,InitialColumn):-
    is_lighted(Row,InitialColumn),
    I1 is InitialColumn + 1,
    is_row_lighted(Row,I1),!.
    
is_col_lighted(8).

is_col_lighted(InitialRow):-
    is_row_lighted(InitialRow,1),
    R1 is InitialRow + 1,
    is_col_lighted(R1),!.


% ? check that all lights in the row are unique in their path
check_lights_row(_,8).

check_lights_row(InitialRow,Column):-
    light(InitialRow,Column),!,
    cell_path(InitialRow,Column,L),
    num_lights(L,A),
    A is 0,
    C1 is Column + 1,
    check_lights_row(InitialRow,C1),!.

check_lights_row(InitialRow,Column):-
    C1 is Column + 1,
    check_lights_row(InitialRow,C1).


check_light_col(8).

check_light_col(InitialRow):-
    check_lights_row(InitialRow,1),
    R1 is InitialRow + 1,
    check_light_col(R1),!.



get_wall_num_in_row(_,8).

get_wall_num_in_row(InitialRow,Column):-
    wall_num(InitialRow,Column,A),!,
    check_num_lights(InitialRow,Column,A),
    C1 is Column + 1,
    get_wall_num_in_row(InitialRow,C1),!.

get_wall_num_in_row(InitialRow,Column):-
    C1 is Column + 1,
    get_wall_num_in_row(InitialRow,C1),!.

get_wall_num_in_col(8).

get_wall_num_in_col(InitialRow):-
    get_wall_num_in_row(InitialRow,1),
    R1 is InitialRow + 1,
    get_wall_num_in_col(R1),!.



% ! procedure(1): make sure all the cells are lighted
all_cells_lighted:-
    is_col_lighted(1).

% ! procedure(2): check that foreach light on the grid there is no another light
% ! in its path
no_double_light:-
    check_light_col(1).

% ! procedure(3): check all lights around a numbered wall and make sure that the count
% ! of lights equal the number written on the wall.
lights_count_correct:-
    get_wall_num_in_col(1).

% ? question 1 
solved:-
    all_cells_lighted,
    no_double_light,
    lights_count_correct.



% ? helper method to loop through the grid
print_row(_,8).

print_row(Row,InitialColumn):-
    wall_num(Row,InitialColumn,A),!,
    write('  '),
    write(A),
    write('  '),
    C1 is InitialColumn + 1,
    print_row(Row,C1).

print_row(Row,InitialColumn):-
    wall(Row,InitialColumn),!,
    write('  #  '),
    C1 is InitialColumn + 1,
    print_row(Row,C1).

print_row(Row,InitialColumn):-
    light(Row,InitialColumn),!,
    write('  L  '),
    C1 is InitialColumn + 1,
    print_row(Row,C1).

print_row(Row,InitialColumn):-
    cell(Row,InitialColumn),!,
    write('  _  '),
    C1 is InitialColumn + 1,
    print_row(Row,C1).

print_col(8).

print_col(InitialRow):-
    print_row(InitialRow,1),
    nl,
    nl,
    R1 is InitialRow + 1, 
    print_col(R1).


% ? print the grid on the terminal
print_grid:-
    print_col(1),!.

% ? clear the memory of the previous assignments of the lights
initialize_game:-
    retractall(light(_,_)),
    print_grid.

% ! here we will implement a solving alogrithm



:- dynamic implaceable/2.
implaceable(X,Y):-
    wall(X,Y).

% ? make a list of coordinates implaceable
make_cells_implaceable([]).

make_cells_implaceable([[X,Y]|T]):-
    not(implaceable(X,Y)),!,
    assert(implaceable(X,Y)),
    make_cells_implaceable(T).

make_cells_implaceable([[X,Y]|T]):-
    make_cells_implaceable(T).

% ? look for numbered wall cells with 0 number. 

make_row_implaceable(_,8).

make_row_implaceable(Row,InitialColumn):-
    wall_num(Row,InitialColumn,0),!,
    neighbors(Row,InitialColumn,L),
    make_cells_implaceable(L),
    I1 is InitialColumn + 1,
    make_row_implaceable(Row,I1),!.

make_row_implaceable(Row,InitialColumn):-
    I1 is InitialColumn + 1,
    make_row_implaceable(Row,I1),!.
    
make_col_implacebale(8).

make_col_implacebale(InitialRow):-
    make_row_implaceable(InitialRow,1),
    R1 is InitialRow + 1,
    make_col_implacebale(R1),!.

% ! step number 1
make_zeros_neighbors_implaceable:-
    make_col_implacebale(1),!.

get_placeable_neighbors(X,Y,L):-
    neighbors(X,Y,L1),
    get_placeable_coords(L1,L),!.


get_placeable_coords([],[]).

get_placeable_coords([[X,Y]|T],[[X,Y]|T1]):-
    not(implaceable(X,Y)),!,
    not(light(X,Y)),!,
    get_placeable_coords(T,T1).

get_placeable_coords([[X,Y]|T],T1):-
    get_placeable_coords(T,T1).
    

% add light to a list of cell coords
add_light([]).
add_light([[X,Y]|T]):-
    assert(light(X,Y)),
    add_light(T).

list_length

placement_of_trivial_row(_,8).

placement_of_trivial_row(Row,InitialColumn):-
    wall_num(Row,InitialColumn,A),!,
    get_placeable_neighbors(Row,InitialColumn,L),
    length(L, A),
    make_cells_implaceable(L),
    add_light(L),
    I1 is InitialColumn + 1,
    placement_of_trivial_row(Row,I1).

placement_of_trivial_row(Row,InitialColumn):-
    I1 is InitialColumn + 1,
    placement_of_trivial_row(Row,I1),!.
    
placement_of_trivial_col(8).

placement_of_trivial_col(InitialRow):-
    placement_of_trivial_row(InitialRow,1),
    R1 is InitialRow + 1,
    placement_of_trivial_col(R1),!.

trivial_solver:-
    make_zeros_neighbors_implaceable,
    placement_of_trivial_col(1).
