/* Robot
 * Author: Thaveesha Dheerasekera
 * Creation date: 2023-01-07
 */

MACHINE
    Robot

SEES
    Maze

DEFINITIONS
    // GUI animation functions
    ANIMATION_FUNCTION == ( { row , column , image | column : x_axis_range & row : y_axis_range & images = 0 & ( column , 6 - row ) /: internal_walls } ) ;
    ANIMATION_FUNCTION1 == ( { row , column , image | column : x_axis_range & row : y_axis_range & images = 1 & ( column , 6 - row ) : internal_walls } ) ;
    ANIMATION_FUNCTION2 == ( { row , column , image | column : x_axis_range & row : y_axis_range & images = 2 & column = robot_x_position & row = robot_y_position } ) ;


    ANIMATION_IMG0 == "images/empty_cell.gif" ;
    ANIMATION_IMG1 == "images/internal_wall.gif" ;
    ANIMATION_IMG2 == "images/robot.gif" ;

SETS
    OUTPUTS = { Moved_North , Moved_East , Moved_South , Moved_West , Exceeding_Maze_Boundary , Maze_Wall_Crash , Cannot_Teleport_Immediately , Cannot_Teleport_To_The_Same_Square , Teleported , yes , no }

ABSTRACT_VARIABLES
    robot_x_position ,
    robot_y_position ,
    current_position ,
    visited_squares

INVARIANT
    // Robot's x-axis position is within the Maze's x-axis range (1-7)
    robot_x_position : x_axis_range &
    // Robot's y-axis position is within the Maze's y-axis range (1-5)
    robot_y_position : y_axis_range &
    // current position of the Robot (maplet (x |-> y)) is an element of the maze
    current_position : maze &
    // squares which have been visited by the Robot is stored as a sequence of Maze maplets 
    visited_squares : seq ( maze )

INITIALISATION
    // Initializes all the position related variables by assigning (1, 1) as the initial suqare
    robot_x_position := 1 ||
    robot_y_position := 1 ||
    current_position := ( 1 |-> 1 ) ||
    visited_squares := [ ( 1 |-> 1 ) ]

OPERATIONS
    /*
     * MoveNorth:
     * Moves the robot 1 square towards North by increasing the current y axis value by 1. (y+1)
     */
    output <-- MoveNorth =
    BEGIN
        // Checks whether the square above the current position is within the maze 
        IF ( robot_y_position + 1 > max ( y_axis_range ) )
        THEN
            output := Exceeding_Maze_Boundary
        // Checks whether the square above the current position is not an internal wall
        ELSIF ( ( robot_x_position |-> robot_y_position + 1 ) : internal_walls )
        THEN
            output := Maze_Wall_Crash
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position |-> robot_y_position + 1 ) ) ||
            current_position := ( robot_x_position |-> robot_y_position + 1 ) ||
            robot_y_position := robot_y_position + 1 ||
            output := Moved_North
        END
    END ;

    /*
     * MoveEast:
     * Moves the robot 1 square towards East by increasing the current x axis value by 1. (x+1)
     */
    output <-- MoveEast =
    BEGIN
        // Checks whether the square to the right of the current position is within the maze 
        IF ( robot_x_position + 1 > max ( x_axis_range ) )
        THEN
            output := Exceeding_Maze_Boundary
        // Checks whether the square to the right of the current position is not an internal wall
        ELSIF ( ( robot_x_position + 1 |-> robot_y_position ) : internal_walls )
        THEN
            output := Maze_Wall_Crash
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position + 1 |-> robot_y_position ) ) ||
            current_position := ( robot_x_position + 1 |-> robot_y_position ) ||
            robot_x_position := robot_x_position + 1 ||
            output := Moved_East
        END
    END ;

    /*
     * MoveSouth:
     * Moves the robot 1 square towards South by decreasing the current y axis value by 1. (y-1)
     */
    output <-- MoveSouth =
    BEGIN
        // Checks whether the square below the current position is within the maze
        IF ( robot_y_position - 1 < min ( y_axis_range ) )
        THEN
            output := Exceeding_Maze_Boundary
        // Checks whether the square below the current position is not an internal wall
        ELSIF ( ( robot_x_position |-> robot_y_position - 1 ) : internal_walls )
        THEN
            output := Maze_Wall_Crash
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position |-> robot_y_position - 1 ) ) ||
            current_position := ( robot_x_position |-> robot_y_position - 1 ) ||
            robot_y_position := robot_y_position - 1 ||
            output := Moved_South
        END
    END ;

    /*
     * MoveWest:
     * Moves the robot 1 square towards West by decreasing the current x axis value by 1. (x-1)
     */
    output <-- MoveWest =
    BEGIN
        // Checks whether the square to the left of the current position is within the maze 
        IF ( robot_x_position - 1 < min ( x_axis_range ) )
        THEN
            output := Exceeding_Maze_Boundary
        // Checks whether the square to the left of the current position is not an internal wall
        ELSIF ( ( robot_x_position - 1 |-> robot_y_position ) : internal_walls )
        THEN
            output := Maze_Wall_Crash
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position - 1 |-> robot_y_position ) ) ||
            current_position := ( robot_x_position - 1 |-> robot_y_position ) ||
            robot_x_position := robot_x_position - 1 ||
            output := Moved_West
        END
    END ;

    /*
     * Teleport:
     * Teleports the robot from one square to another square where the squares are both empty (Non Internal Walls).
     * Teleport only works after an initial movement has been made. (E.g:- After the first MoveNorth Operation)
     */
    output <-- Teleport ( teleport_x_position , teleport_y_position ) =
    PRE output : OUTPUTS & teleport_x_position : NATURAL1 & teleport_y_position : NATURAL1
    THEN
        // Makes sure that this is not the first move
        IF ( size ( visited_squares ) = 1 )
        THEN
            output := Cannot_Teleport_Immediately
        // Makes sure that the robot doesn't teleport to an internal wall
        ELSIF ( ( teleport_x_position |-> teleport_y_position ) : internal_walls )
        THEN
            output := Maze_Wall_Crash
        // Makes sure that the robot doesn't teleport to the same current square
        ELSIF ( teleport_x_position = robot_x_position & teleport_y_position = robot_y_position )
        THEN
            output := Cannot_Teleport_To_The_Same_Square
        // Makes sure that the robot doesn't teleport itself out of the maze
        ELSIF ( ( teleport_x_position |-> teleport_y_position ) /: maze )
        THEN
            output := Exceeding_Maze_Boundary
        ELSE
            visited_squares := ( visited_squares <- ( teleport_x_position |-> teleport_y_position ) ) ||
            current_position := ( teleport_x_position |-> teleport_y_position ) ||
            robot_x_position := teleport_x_position ||
            robot_y_position := teleport_y_position ||
            output := Teleported
        END
    END ;

    /*
     * GetPosition:
     * Returns the current position of the robot (x |-> y)
     */
    position <-- GetPosition =
        BEGIN
            position := current_position
        END ;

    /*
     * FoundExit:
     * Returns yes/no depending on whether the robot has reached the exit square or not.
     */
    enquiry <-- FoundExit =
    BEGIN
        // Checks if the robot's current position is the exit square
        IF ( current_position : exit_square )
        THEN
            enquiry := yes
        ELSE
            enquiry := no
        END
    END ;

    /*
     * VisitedSquare:
     * Returns whether the robot has visited a given square already (yes/no)
     */
    visited <-- VisitedSquare ( visited_x_position , visited_y_position ) =
    PRE visited : OUTPUTS & visited_x_position : NATURAL1 & visited_y_position : NATURAL1
    THEN
        // Makes sure that the visited square x,y coordinates are within the maze
        IF ( ( visited_x_position |-> visited_y_position ) : maze )
        THEN
            // Checks if the robot has already visited a particular square (Doesn't include the current square, but all the previouse squares)
            IF ( ( visited_x_position |-> visited_y_position ) : ran ( front ( visited_squares ) ) )
            THEN
                visited := yes
            ELSE
                visited := no
            END
        ELSE
            visited := Exceeding_Maze_Boundary
        END
    END ;

    /*
     * RobotsRoute:
     * Returns the sequence of the path that the robot has gone through ({(1 |-> (x1 |-> y1)), (2 |-> (x2 |-> y2))}))
     */
    route <-- RobotsRoute =
    BEGIN
        route := visited_squares
    END

END
