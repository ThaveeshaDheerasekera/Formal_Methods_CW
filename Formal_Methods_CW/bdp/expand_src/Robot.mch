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
    ANIMATION_FUNCTION == ( { row , column , image | column : x_axis_range & row : y_axis_range & image = 0 & ( column , 6 - row ) /: internal_walls } ) ;
    ANIMATION_FUNCTION1 == ( { row , column , image | column : x_axis_range & row : y_axis_range & image = 1 & ( column , 6 - row ) : internal_walls } ) ;
    ANIMATION_FUNCTION2 == ( { row , column , image | column : x_axis_range & row : y_axis_range & image = 2 & column = robot_x_position & row = ( 6 - robot_y_position ) } ) ;


    ANIMATION_IMG0 == "./images/empty_cell.gif" ;
    ANIMATION_IMG1 == "./images/internal_wall.gif" ;
    ANIMATION_IMG2 == "./images/robot.gif" ;

SETS
    OUTPUTS = { MOVED_NORTH , MOVED_EAST , MOVED_SOUTH , MOVED_WEST , EXCEEDING_MAZE_BOUNDARY , WALL_CRASH , CANNOT_TELEPORT_IMMEDIATELY , CANNOT_TELEPORT_TO_THE_SAME_SQUARE , TELEPORTED , YES , NO }

ABSTRACT_VARIABLES
    robot_x_position ,
    robot_y_position ,
    current_position ,
    visited_squares

INVARIANT
    // Robot's x-axis position is within the Maze's x-axis range (1-7)
    robot_x_position : NATURAL1 & robot_x_position : x_axis_range &
    // Robot's y-axis position is within the Maze's y-axis range (1-5)
    robot_y_position : NATURAL1 & robot_y_position : y_axis_range &
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
            output := EXCEEDING_MAZE_BOUNDARY
        // Checks whether the square above the current position is not an internal wall
        ELSIF ( ( robot_x_position |-> robot_y_position + 1 ) : internal_walls )
        THEN
            output := WALL_CRASH
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position |-> robot_y_position + 1 ) ) ||
            current_position := ( robot_x_position |-> robot_y_position + 1 ) ||
            robot_y_position := robot_y_position + 1 ||
            output := MOVED_NORTH
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
            output := EXCEEDING_MAZE_BOUNDARY
        // Checks whether the square to the right of the current position is not an internal wall
        ELSIF ( ( robot_x_position + 1 |-> robot_y_position ) : internal_walls )
        THEN
            output := WALL_CRASH
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position + 1 |-> robot_y_position ) ) ||
            current_position := ( robot_x_position + 1 |-> robot_y_position ) ||
            robot_x_position := robot_x_position + 1 ||
            output := MOVED_EAST
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
            output := EXCEEDING_MAZE_BOUNDARY
        // Checks whether the square below the current position is not an internal wall
        ELSIF ( ( robot_x_position |-> robot_y_position - 1 ) : internal_walls )
        THEN
            output := WALL_CRASH
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position |-> robot_y_position - 1 ) ) ||
            current_position := ( robot_x_position |-> robot_y_position - 1 ) ||
            robot_y_position := robot_y_position - 1 ||
            output := MOVED_SOUTH
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
            output := EXCEEDING_MAZE_BOUNDARY
        // Checks whether the square to the left of the current position is not an internal wall
        ELSIF ( ( robot_x_position - 1 |-> robot_y_position ) : internal_walls )
        THEN
            output := WALL_CRASH
        ELSE
            visited_squares := ( visited_squares <- ( robot_x_position - 1 |-> robot_y_position ) ) ||
            current_position := ( robot_x_position - 1 |-> robot_y_position ) ||
            robot_x_position := robot_x_position - 1 ||
            output := MOVED_WEST
        END
    END ;

    /*
     * Teleport:
     * Teleports the robot from one square to another square where the squares are both empty (Non Internal Walls).
     * Teleport only works after an initial movement has been made. (E.g:- After the first MoveNorth Operation)
     */
    output <-- Teleport ( teleport_x_position , teleport_y_position ) =
    PRE ( teleport_x_position : NATURAL1 ) & ( teleport_y_position : NATURAL1 ) & ( teleport_x_position : x_axis_range ) & ( teleport_y_position : y_axis_range ) & ( ( teleport_x_position |-> teleport_y_position ) : maze ) & ( ( teleport_x_position |-> teleport_y_position ) /= ( robot_x_position |-> robot_y_position ) )
    THEN
        IF ( ( teleport_x_position |-> teleport_y_position ) : internal_walls ) // Check if the square the robot is trying to teleport to, is an internal wall 
        THEN
            output := WALL_CRASH
        ELSIF ( ( size ( visited_squares ) = 1 ) ) // It is not acceptable to immediately teleport the robot, to the exit square
        THEN
            output := CANNOT_TELEPORT_IMMEDIATELY
        ELSE
            robot_x_position , robot_y_position , output := teleport_x_position , teleport_y_position , TELEPORTED
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
     * Returns YES/NO depending on whether the robot has reached the exit square or not.
     */
    enquiry <-- FoundExit =
    BEGIN
        // Checks if the robot's current position is the exit square
        IF ( current_position : exit_square )
        THEN
            enquiry := YES
        ELSE
            enquiry := NO
        END
    END ;

    /*
     * VisitedSquare:
     * Returns whether the robot has visited a given square already (YES/NO)
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
                visited := YES
            ELSE
                visited := NO
            END
        ELSE
            visited := EXCEEDING_MAZE_BOUNDARY
        END
    END ;

    /*
     * RobotsRoute:
     * Returns the sequence of the path that the robot has gone through ({(1 |-> (x1 |-> y1)), (2 |-> (x2 |-> y2))}))
     */
    route <-- RobotsRoute =
    BEGIN
        route := visited_squares
    END ;

    /*
     * ResetRobot:
     * Reset the robot to it's original position
     */
    ResetRobot =
    BEGIN
        robot_x_position , robot_y_position := 1 , 1
    END

END
