Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Robot))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Robot))==(Machine(Robot));
  Level(Machine(Robot))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Robot)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Robot))==(Maze)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Robot))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Robot))==(?);
  List_Includes(Machine(Robot))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Robot))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Robot))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Robot))==(?);
  Context_List_Variables(Machine(Robot))==(?);
  Abstract_List_Variables(Machine(Robot))==(?);
  Local_List_Variables(Machine(Robot))==(visited_squares,current_position,robot_y_position,robot_x_position);
  List_Variables(Machine(Robot))==(visited_squares,current_position,robot_y_position,robot_x_position);
  External_List_Variables(Machine(Robot))==(visited_squares,current_position,robot_y_position,robot_x_position)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Robot))==(?);
  Abstract_List_VisibleVariables(Machine(Robot))==(?);
  External_List_VisibleVariables(Machine(Robot))==(?);
  Expanded_List_VisibleVariables(Machine(Robot))==(?);
  List_VisibleVariables(Machine(Robot))==(?);
  Internal_List_VisibleVariables(Machine(Robot))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Robot))==(btrue);
  Gluing_List_Invariant(Machine(Robot))==(btrue);
  Expanded_List_Invariant(Machine(Robot))==(btrue);
  Abstract_List_Invariant(Machine(Robot))==(btrue);
  Context_List_Invariant(Machine(Robot))==(btrue);
  List_Invariant(Machine(Robot))==(robot_x_position: x_axis_range & robot_y_position: y_axis_range & current_position: maze & visited_squares: seq(maze))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Robot))==(btrue);
  Abstract_List_Assertions(Machine(Robot))==(btrue);
  Context_List_Assertions(Machine(Robot))==(btrue);
  List_Assertions(Machine(Robot))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Robot))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Robot))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Robot))==(robot_x_position,robot_y_position,current_position,visited_squares:=1,1,1|->1,[1|->1]);
  Context_List_Initialisation(Machine(Robot))==(skip);
  List_Initialisation(Machine(Robot))==(robot_x_position:=1 || robot_y_position:=1 || current_position:=1|->1 || visited_squares:=[1|->1])
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Robot))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Robot),Machine(Maze))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Robot))==(btrue);
  List_Constraints(Machine(Robot))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Robot))==(MoveNorth,MoveEast,MoveSouth,MoveWest,Teleport,GetPosition,FoundExit,VisitedSquare,RobotsRoute);
  List_Operations(Machine(Robot))==(MoveNorth,MoveEast,MoveSouth,MoveWest,Teleport,GetPosition,FoundExit,VisitedSquare,RobotsRoute)
END
&
THEORY ListInputX IS
  List_Input(Machine(Robot),MoveNorth)==(?);
  List_Input(Machine(Robot),MoveEast)==(?);
  List_Input(Machine(Robot),MoveSouth)==(?);
  List_Input(Machine(Robot),MoveWest)==(?);
  List_Input(Machine(Robot),Teleport)==(teleport_x_position,teleport_y_position);
  List_Input(Machine(Robot),GetPosition)==(?);
  List_Input(Machine(Robot),FoundExit)==(?);
  List_Input(Machine(Robot),VisitedSquare)==(visited_x_position,visited_y_position);
  List_Input(Machine(Robot),RobotsRoute)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Robot),MoveNorth)==(output);
  List_Output(Machine(Robot),MoveEast)==(output);
  List_Output(Machine(Robot),MoveSouth)==(output);
  List_Output(Machine(Robot),MoveWest)==(output);
  List_Output(Machine(Robot),Teleport)==(output);
  List_Output(Machine(Robot),GetPosition)==(position);
  List_Output(Machine(Robot),FoundExit)==(enquiry);
  List_Output(Machine(Robot),VisitedSquare)==(visited);
  List_Output(Machine(Robot),RobotsRoute)==(route)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Robot),MoveNorth)==(output <-- MoveNorth);
  List_Header(Machine(Robot),MoveEast)==(output <-- MoveEast);
  List_Header(Machine(Robot),MoveSouth)==(output <-- MoveSouth);
  List_Header(Machine(Robot),MoveWest)==(output <-- MoveWest);
  List_Header(Machine(Robot),Teleport)==(output <-- Teleport(teleport_x_position,teleport_y_position));
  List_Header(Machine(Robot),GetPosition)==(position <-- GetPosition);
  List_Header(Machine(Robot),FoundExit)==(enquiry <-- FoundExit);
  List_Header(Machine(Robot),VisitedSquare)==(visited <-- VisitedSquare(visited_x_position,visited_y_position));
  List_Header(Machine(Robot),RobotsRoute)==(route <-- RobotsRoute)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Robot),MoveNorth)==(btrue);
  List_Precondition(Machine(Robot),MoveEast)==(btrue);
  List_Precondition(Machine(Robot),MoveSouth)==(btrue);
  List_Precondition(Machine(Robot),MoveWest)==(btrue);
  List_Precondition(Machine(Robot),Teleport)==(output: OUTPUTS & teleport_x_position: NATURAL1 & teleport_y_position: NATURAL1);
  List_Precondition(Machine(Robot),GetPosition)==(btrue);
  List_Precondition(Machine(Robot),FoundExit)==(btrue);
  List_Precondition(Machine(Robot),VisitedSquare)==(visited: OUTPUTS & visited_x_position: NATURAL1 & visited_y_position: NATURAL1);
  List_Precondition(Machine(Robot),RobotsRoute)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Robot),RobotsRoute)==(btrue | route:=visited_squares);
  Expanded_List_Substitution(Machine(Robot),VisitedSquare)==(visited: OUTPUTS & visited_x_position: NATURAL1 & visited_y_position: NATURAL1 | visited_x_position|->visited_y_position: maze ==> (visited_x_position|->visited_y_position: ran(front(visited_squares)) ==> visited:=yes [] not(visited_x_position|->visited_y_position: ran(front(visited_squares))) ==> visited:=no) [] not(visited_x_position|->visited_y_position: maze) ==> visited:=Exceeding_Maze_Boundary);
  Expanded_List_Substitution(Machine(Robot),FoundExit)==(btrue | current_position: exit_square ==> enquiry:=yes [] not(current_position: exit_square) ==> enquiry:=no);
  Expanded_List_Substitution(Machine(Robot),GetPosition)==(btrue | position:=current_position);
  Expanded_List_Substitution(Machine(Robot),Teleport)==(output: OUTPUTS & teleport_x_position: NATURAL1 & teleport_y_position: NATURAL1 | size(visited_squares) = 1 ==> output:=Cannot_Teleport_Immediately [] not(size(visited_squares) = 1) ==> (teleport_x_position|->teleport_y_position: internal_walls ==> output:=Maze_Wall_Crash [] not(teleport_x_position|->teleport_y_position: internal_walls) ==> (teleport_x_position = robot_x_position & teleport_y_position = robot_y_position ==> output:=Cannot_Teleport_To_The_Same_Square [] not(teleport_x_position = robot_x_position & teleport_y_position = robot_y_position) ==> (teleport_x_position|->teleport_y_position/:maze ==> output:=Exceeding_Maze_Boundary [] not(teleport_x_position|->teleport_y_position/:maze) ==> visited_squares,current_position,robot_x_position,robot_y_position,output:=visited_squares<-(teleport_x_position|->teleport_y_position),teleport_x_position|->teleport_y_position,teleport_x_position,teleport_y_position,Teleported))));
  Expanded_List_Substitution(Machine(Robot),MoveWest)==(btrue | robot_x_position-1<min(x_axis_range) ==> output:=Exceeding_Maze_Boundary [] not(robot_x_position-1<min(x_axis_range)) ==> (robot_x_position-1|->robot_y_position: internal_walls ==> output:=Maze_Wall_Crash [] not(robot_x_position-1|->robot_y_position: internal_walls) ==> visited_squares,current_position,robot_x_position,output:=visited_squares<-(robot_x_position-1|->robot_y_position),robot_x_position-1|->robot_y_position,robot_x_position-1,Moved_West));
  Expanded_List_Substitution(Machine(Robot),MoveSouth)==(btrue | robot_y_position-1<min(y_axis_range) ==> output:=Exceeding_Maze_Boundary [] not(robot_y_position-1<min(y_axis_range)) ==> (robot_x_position|->robot_y_position-1: internal_walls ==> output:=Maze_Wall_Crash [] not(robot_x_position|->robot_y_position-1: internal_walls) ==> visited_squares,current_position,robot_y_position,output:=visited_squares<-(robot_x_position|->robot_y_position-1),robot_x_position|->robot_y_position-1,robot_y_position-1,Moved_South));
  Expanded_List_Substitution(Machine(Robot),MoveEast)==(btrue | robot_x_position+1>max(x_axis_range) ==> output:=Exceeding_Maze_Boundary [] not(robot_x_position+1>max(x_axis_range)) ==> (robot_x_position+1|->robot_y_position: internal_walls ==> output:=Maze_Wall_Crash [] not(robot_x_position+1|->robot_y_position: internal_walls) ==> visited_squares,current_position,robot_x_position,output:=visited_squares<-(robot_x_position+1|->robot_y_position),robot_x_position+1|->robot_y_position,robot_x_position+1,Moved_East));
  Expanded_List_Substitution(Machine(Robot),MoveNorth)==(btrue | robot_y_position+1>max(y_axis_range) ==> output:=Exceeding_Maze_Boundary [] not(robot_y_position+1>max(y_axis_range)) ==> (robot_x_position|->robot_y_position+1: internal_walls ==> output:=Maze_Wall_Crash [] not(robot_x_position|->robot_y_position+1: internal_walls) ==> visited_squares,current_position,robot_y_position,output:=visited_squares<-(robot_x_position|->robot_y_position+1),robot_x_position|->robot_y_position+1,robot_y_position+1,Moved_North));
  List_Substitution(Machine(Robot),MoveNorth)==(IF robot_y_position+1>max(y_axis_range) THEN output:=Exceeding_Maze_Boundary ELSIF robot_x_position|->robot_y_position+1: internal_walls THEN output:=Maze_Wall_Crash ELSE visited_squares:=visited_squares<-(robot_x_position|->robot_y_position+1) || current_position:=robot_x_position|->robot_y_position+1 || robot_y_position:=robot_y_position+1 || output:=Moved_North END);
  List_Substitution(Machine(Robot),MoveEast)==(IF robot_x_position+1>max(x_axis_range) THEN output:=Exceeding_Maze_Boundary ELSIF robot_x_position+1|->robot_y_position: internal_walls THEN output:=Maze_Wall_Crash ELSE visited_squares:=visited_squares<-(robot_x_position+1|->robot_y_position) || current_position:=robot_x_position+1|->robot_y_position || robot_x_position:=robot_x_position+1 || output:=Moved_East END);
  List_Substitution(Machine(Robot),MoveSouth)==(IF robot_y_position-1<min(y_axis_range) THEN output:=Exceeding_Maze_Boundary ELSIF robot_x_position|->robot_y_position-1: internal_walls THEN output:=Maze_Wall_Crash ELSE visited_squares:=visited_squares<-(robot_x_position|->robot_y_position-1) || current_position:=robot_x_position|->robot_y_position-1 || robot_y_position:=robot_y_position-1 || output:=Moved_South END);
  List_Substitution(Machine(Robot),MoveWest)==(IF robot_x_position-1<min(x_axis_range) THEN output:=Exceeding_Maze_Boundary ELSIF robot_x_position-1|->robot_y_position: internal_walls THEN output:=Maze_Wall_Crash ELSE visited_squares:=visited_squares<-(robot_x_position-1|->robot_y_position) || current_position:=robot_x_position-1|->robot_y_position || robot_x_position:=robot_x_position-1 || output:=Moved_West END);
  List_Substitution(Machine(Robot),Teleport)==(IF size(visited_squares) = 1 THEN output:=Cannot_Teleport_Immediately ELSIF teleport_x_position|->teleport_y_position: internal_walls THEN output:=Maze_Wall_Crash ELSIF teleport_x_position = robot_x_position & teleport_y_position = robot_y_position THEN output:=Cannot_Teleport_To_The_Same_Square ELSIF teleport_x_position|->teleport_y_position/:maze THEN output:=Exceeding_Maze_Boundary ELSE visited_squares:=visited_squares<-(teleport_x_position|->teleport_y_position) || current_position:=teleport_x_position|->teleport_y_position || robot_x_position:=teleport_x_position || robot_y_position:=teleport_y_position || output:=Teleported END);
  List_Substitution(Machine(Robot),GetPosition)==(position:=current_position);
  List_Substitution(Machine(Robot),FoundExit)==(IF current_position: exit_square THEN enquiry:=yes ELSE enquiry:=no END);
  List_Substitution(Machine(Robot),VisitedSquare)==(IF visited_x_position|->visited_y_position: maze THEN IF visited_x_position|->visited_y_position: ran(front(visited_squares)) THEN visited:=yes ELSE visited:=no END ELSE visited:=Exceeding_Maze_Boundary END);
  List_Substitution(Machine(Robot),RobotsRoute)==(route:=visited_squares)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Robot))==(?);
  Inherited_List_Constants(Machine(Robot))==(?);
  List_Constants(Machine(Robot))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Robot),OUTPUTS)==({Moved_North,Moved_East,Moved_South,Moved_West,Exceeding_Maze_Boundary,Maze_Wall_Crash,Cannot_Teleport_Immediately,Cannot_Teleport_To_The_Same_Square,Teleported,yes,no});
  Context_List_Enumerated(Machine(Robot))==(?);
  Context_List_Defered(Machine(Robot))==(?);
  Context_List_Sets(Machine(Robot))==(?);
  List_Valuable_Sets(Machine(Robot))==(?);
  Inherited_List_Enumerated(Machine(Robot))==(?);
  Inherited_List_Defered(Machine(Robot))==(?);
  Inherited_List_Sets(Machine(Robot))==(?);
  List_Enumerated(Machine(Robot))==(OUTPUTS);
  List_Defered(Machine(Robot))==(?);
  List_Sets(Machine(Robot))==(OUTPUTS)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Robot))==(?);
  Expanded_List_HiddenConstants(Machine(Robot))==(?);
  List_HiddenConstants(Machine(Robot))==(?);
  External_List_HiddenConstants(Machine(Robot))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Robot))==(btrue);
  Context_List_Properties(Machine(Robot))==(width = 7 & height = 5 & x_axis_range <: NATURAL1 & x_axis_range = 1..width & y_axis_range <: NATURAL1 & y_axis_range = 1..height & maze: x_axis_range <-> y_axis_range & maze = x_axis_range*y_axis_range & internal_walls = {1|->3,2|->1,2|->3,2|->5,3|->3,4|->2,4|->3,4|->4,6|->1,6|->2,6|->4,7|->4} & entrance_square = {1|->1} & exit_square = {1|->5});
  Inherited_List_Properties(Machine(Robot))==(btrue);
  List_Properties(Machine(Robot))==(OUTPUTS: FIN(INTEGER) & not(OUTPUTS = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Robot),Machine(Maze))==(?);
  Seen_Context_List_Enumerated(Machine(Robot))==(?);
  Seen_Context_List_Invariant(Machine(Robot))==(btrue);
  Seen_Context_List_Assertions(Machine(Robot))==(btrue);
  Seen_Context_List_Properties(Machine(Robot))==(btrue);
  Seen_List_Constraints(Machine(Robot))==(btrue);
  Seen_List_Operations(Machine(Robot),Machine(Maze))==(?);
  Seen_Expanded_List_Invariant(Machine(Robot),Machine(Maze))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Robot),MoveNorth)==(?);
  List_ANY_Var(Machine(Robot),MoveEast)==(?);
  List_ANY_Var(Machine(Robot),MoveSouth)==(?);
  List_ANY_Var(Machine(Robot),MoveWest)==(?);
  List_ANY_Var(Machine(Robot),Teleport)==(?);
  List_ANY_Var(Machine(Robot),GetPosition)==(?);
  List_ANY_Var(Machine(Robot),FoundExit)==(?);
  List_ANY_Var(Machine(Robot),VisitedSquare)==(?);
  List_ANY_Var(Machine(Robot),RobotsRoute)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Robot)) == (OUTPUTS,Moved_North,Moved_East,Moved_South,Moved_West,Exceeding_Maze_Boundary,Maze_Wall_Crash,Cannot_Teleport_Immediately,Cannot_Teleport_To_The_Same_Square,Teleported,yes,no | ? | visited_squares,current_position,robot_y_position,robot_x_position | ? | MoveNorth,MoveEast,MoveSouth,MoveWest,Teleport,GetPosition,FoundExit,VisitedSquare,RobotsRoute | ? | seen(Machine(Maze)) | ? | Robot);
  List_Of_HiddenCst_Ids(Machine(Robot)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Robot)) == (?);
  List_Of_VisibleVar_Ids(Machine(Robot)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Robot)) == (?: ?);
  List_Of_Ids(Machine(Maze)) == (width,height,x_axis_range,y_axis_range,maze,internal_walls,entrance_square,exit_square | ? | ? | ? | ? | ? | ? | ? | Maze);
  List_Of_HiddenCst_Ids(Machine(Maze)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Maze)) == (width,height,x_axis_range,y_axis_range,maze,internal_walls,entrance_square,exit_square);
  List_Of_VisibleVar_Ids(Machine(Maze)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Maze)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Robot)) == (Type(OUTPUTS) == Cst(SetOf(etype(OUTPUTS,0,10))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Robot)) == (Type(Moved_North) == Cst(etype(OUTPUTS,0,10));Type(Moved_East) == Cst(etype(OUTPUTS,0,10));Type(Moved_South) == Cst(etype(OUTPUTS,0,10));Type(Moved_West) == Cst(etype(OUTPUTS,0,10));Type(Exceeding_Maze_Boundary) == Cst(etype(OUTPUTS,0,10));Type(Maze_Wall_Crash) == Cst(etype(OUTPUTS,0,10));Type(Cannot_Teleport_Immediately) == Cst(etype(OUTPUTS,0,10));Type(Cannot_Teleport_To_The_Same_Square) == Cst(etype(OUTPUTS,0,10));Type(Teleported) == Cst(etype(OUTPUTS,0,10));Type(yes) == Cst(etype(OUTPUTS,0,10));Type(no) == Cst(etype(OUTPUTS,0,10)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Robot)) == (Type(visited_squares) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(current_position) == Mvl(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(robot_y_position) == Mvl(btype(INTEGER,?,?));Type(robot_x_position) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Robot)) == (Type(RobotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(VisitedSquare) == Cst(etype(OUTPUTS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(FoundExit) == Cst(etype(OUTPUTS,?,?),No_type);Type(GetPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(Teleport) == Cst(etype(OUTPUTS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MoveWest) == Cst(etype(OUTPUTS,?,?),No_type);Type(MoveSouth) == Cst(etype(OUTPUTS,?,?),No_type);Type(MoveEast) == Cst(etype(OUTPUTS,?,?),No_type);Type(MoveNorth) == Cst(etype(OUTPUTS,?,?),No_type));
  Observers(Machine(Robot)) == (Type(RobotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(VisitedSquare) == Cst(etype(OUTPUTS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(FoundExit) == Cst(etype(OUTPUTS,?,?),No_type);Type(GetPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
