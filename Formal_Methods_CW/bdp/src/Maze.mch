/* Maze
 * Author: Thaveesha Dheerasekera
 * Creation date: 2023-01-07
 */

MACHINE
    Maze

CONCRETE_CONSTANTS
    width ,
    height ,
    x_axis_range ,
    y_axis_range ,
    maze ,
    internal_walls ,
    entrance_square ,
    exit_square

PROPERTIES
    width = 7 &
    height = 5 &
    // The range of the x axis (1 - 7)
    x_axis_range <: NATURAL1 & x_axis_range = 1 .. width &
    // The range of the y axis (1 - 5)
    y_axis_range <: NATURAL1 & y_axis_range = 1 .. height &
    // Difining the maze 
    maze : x_axis_range <-> y_axis_range & maze = ( x_axis_range * y_axis_range ) &
    // The internal walls in the maze 
    internal_walls = { ( 1 |-> 3 ) , ( 2 |-> 1 ) , ( 2 |-> 3 ) , ( 2 |-> 5 ) , ( 3 |-> 3 ) , ( 4 |-> 2 ) , ( 4 |-> 3 ) , ( 4 |-> 4 ) , ( 6 |-> 1 ) , ( 6 |-> 2 ) , ( 6 |-> 4 ) , ( 7 |-> 4 ) } &
    // Entrance square coordination
    entrance_square = { ( 1 |-> 1 ) } &
    // Exit square coordination
    exit_square = { ( 1 |-> 5 ) }

END
