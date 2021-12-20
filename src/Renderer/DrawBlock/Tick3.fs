module Tick3
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open System.Text.RegularExpressions
// unique Id for Thing objects
type ThingId = string

// Thing represents a cicle or a rectangle on an SVG canvas
type Thing = {   
    Id: ThingId
    IsRectangle: bool
    Side: int // which side (of a rectangle) is currently being dragged 0: right, 1: bottom, 2: left, 3: top
    X: float // x coordinate of centre of Thing
    Y: float // y coordinate of centre of Thing
    X1: float // width of rectangle or diameter of circle
    X2: float // height of rectangle
}

type Model3 = {
    ClickRadius: float
    Things: Map<ThingId,Thing>
    MouseIsTick3: bool // true changes Issie functionality so all mouse operations are processed by Tick3 code
    Dragging: bool // is something being currently dragged to resize by the mouse
    DraggedThing: Thing // which Thing is being dragged
}

type RenderThingProps = Thing

let dummyThing = {
    Id = "abc"
    IsRectangle = false  // true if thing represents a rectangle
    Side = 0 // which side (of a rectangle) is currently being dragged 0: right, 1: bottom, 2: left, 3: top
    X = 0. // x coordinate of centre of Thing
    Y = 0. // y coordinate of centre of Thing
    X1 = 0. // width of rectangle or diameter of circle
    X2 = 0. // height of rectangle
}

/// initialise the Model
/// for testing add a rectangle and circle Thing (in different positions)
let tick3Init() : Model3 = 
    {
        Things = Map.empty // no things initially
        ClickRadius = 2. // how near do you have to click an object to initiate a drag
        Dragging = false
        DraggedThing = dummyThing // nothing is dragged initially
        MouseIsTick3 = true  // switches between normal Issie operation, and mouse messages processed by Tick3 code
    }

//------------------------------Code for Dragging The Thing--------------------------------------------------//

(*
The next section of code returns the required mouse position offset when dragging one side of a rectangle-type
Thing, coded as an intermediate C programmer might approach the problem when told to use lots of functions.
This solution, in F#, shows many of the coding style problems in the Wiki https://github.com/tomcl/issie/wiki
It is obvious, looking at this code not having written it, that it is bad. However when writing code it
is quite easy for novice F# programmers to write this.
*)

/// returns true if the coordinate (X or Y) in common between the two side endpoints is positive
/// relative to the rectangle position
let sideHasPositiveCommonCoordinateOffset side =
    side = 0 || side = 1

/// Return the two side endpoint sets of coordinates
/// for side s of rectangle center (c1,c2), width x1, height x2
/// The most positive end must be first
let getCoordinates s c1 c2 x1 x2 =
    match s with
    | 0 -> (c1 + x1/2.0, c2 + x2/2.0),(c1 + x1/2.0, c2 - x2/2.0)
    | 2 -> (-c1 - x1/2.0, c2 + x2/2.0),(-c1 - x1/2.0, c2 - x2/2.0)
    | 1 -> (c1 + x1/2.0, c2 + x2/2.0),(c1 - x1/2.0, c2 + x2/2.0)
    | 3 -> (c1 + x1/2.0, c2 - x2/2.0), (c1 - x1/2.0, c2 - x2/2.0)
    | _ -> (0. , 0.), (0. , 0.) // Return a default zero value for bad s to avoid exception
  
    

// get offset between side of rectangle and current mouse position
// direction = true => horizontal side
// (x1,y1): side end point (either will do)
// (x,y) current mouse pos
let subtractFromX1OrY1 direction x1 y1 x y =
    if direction then x1 - x else y1 - y

// return movement needed when dragging to change the size of a rectangle thing
// as change in its X1, X2 components
// one of these component changes will be 0
// output is tuple in form X1,X2
// side = side that is being dragged by mouse
// thing = rectangle
let doSubtraction (thing: Thing) side x y =
    let cc1,cc2 = getCoordinates side thing.X thing.Y thing.X1 thing.X2
    let d = subtractFromX1OrY1 (side % 2 = 0) (fst cc1) (snd cc1) x y
    match side % 2 with
    | 0 | 2 -> thing.X1 + d*2., thing.X2
    | 1 | 3 -> thing.X1, thing.X2 + d*2.

/// Alter size of currently dragged thing to make its edge (or its clicked side) follow pos
/// For circles the circle should go through pos
/// For rectangles pos shoudl be colinear with the dragged side (common coordinate the same)
let dragThing (pos: XYPos) (model: Model3) =
    failwithf "Not implemented"
    
//-----------------------Code to display all things in the view function--------------------------//
    

/// draw a thing   
/// circ: true if thing is circle
/// x,y,x1,x2 fields from thing of same name
let doDrawing r x y x1 x2 : ReactElement list=
    // see DrawHelpers for some examples of how to draw.
    failwithf "Not Implemented"

/// display as a single SVG element the Thing defined by ThingProps
let renderThing =        
        FunctionComponent.Of(
            (fun (thingProps : RenderThingProps) ->
                g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" thingProps.X thingProps.Y) ] ]) (doDrawing (not thingProps.IsRectangle) thingProps.X thingProps.Y thingProps.X1 thingProps.X2)),                
            "Thing",
            equalsButFunctions,
            withKey = fun props -> props.Id // speeds up react caching
            )

/// display a a single SVG element the Things defined in model
let renderTick3 (model: Model3) display = 
    model.Things
    |> Helpers.mapValues
    |> Seq.toList
    |> List.map renderThing
    |> ofList

//--------------------------------Code to determine what was clicked-------------------------//

/// is a rectangle side (determined by its two endpoints) clicked
let clickedSideOpt clickRadius (pos:XYPos) (i,((x1,y1),(x2,y2))) =
    if abs (x1 - x2) < abs (y1 - y2) then
        // it is a vertical side
        if abs (pos.Y - y1) < clickRadius && x1 > pos.X && pos.X > x2 then
            Some i
        else
            None
    else 
        if abs (pos.X - x1) < clickRadius && y1 > pos.Y && pos.Y > y2 then
            Some i
        else
            None
            



/// return None or the thing (and possibly side, for rectangles) clicked
let clickedThingOpt (clickRadius: float) (pos:XYPos) (thingId: ThingId) (thing: Thing):
        {|ThingId: ThingId; ItemSide:int|} option =
    if thing.IsRectangle then
        [0..3]
        |> List.map (fun side -> side, getCoordinates side thing.X thing.Y thing.X1 thing.X2)
        |> List.tryPick (clickedSideOpt clickRadius pos)
        |> Option.map (fun side -> {|ThingId = thingId; ItemSide = side|})
    elif abs (euclideanDistance pos {X=thing.X;Y=thing.Y} - thing.X1 / 2.0) < clickRadius then
        Some {|ThingId = thingId; ItemSide = 0|}
    else 
        None
    
/// return None or the thing (and possibly side, for rectangle things) clicked
let tryFindClickedThing (clickRadius: float) (pos: XYPos) (m:Model3) : {|ThingId: ThingId; ItemSide:int|} option =
    Map.tryPick (clickedThingOpt clickRadius pos) m.Things
  

//--------------------------------Update function for Tick3----------------------------------//

/// alter model to start a drag operation
let startDragging (draggable: {|ThingId: ThingId; ItemSide:int|}) (model: Model3) : Model3 =
    failwithf "not implemented"

/// alter model to stop a drag operation
let stopDragging (model: Model3) : Model3 =
    failwithf "Not Implemented"

/// Update the model after given Mouse event (see type MouseT).
/// Called with every mouse operation if model.MouseIsTick3 = true
/// Returns the desired new Tick3 part of model based on the mouse event
let updateTick3 (model: Model3) (mMsg: MouseT): Model3 =
    match mMsg.Op with
    | Down ->
        tryFindClickedThing model.ClickRadius mMsg.Pos model 
        |> Option.map (fun thingToDrag -> startDragging thingToDrag model)
        |> Option.defaultValue model
    | Up -> stopDragging model
    | Move -> model // do nothing
    | Drag -> dragThing mMsg.Pos model


