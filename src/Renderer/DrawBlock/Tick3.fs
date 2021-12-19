module Tick3
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
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

let dragThing (pos: XYPos) (model: Model3) =
    failwithf "Noyt implemented"
    
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

let insideBoundingBox (pos:XYPos) (thingKey: ThingId) (thing: Thing):
    {|ItemId: ThingId; ItemSide:int|} option =
    failwithf "Not implemented"

let findClickedItem (pos: XYPos) (m:Model3) : {|ItemId: string; ItemSide:int|} option =
    Map.tryPick (insideBoundingBox pos) m.Things
  
let startDragging (draggable: {|ItemId: string; ItemSide:int|}) (model: Model3) : Model3 =
    failwithf "not implemented"

let stopDragging (model: Model3) : Model3 =
    failwithf "Not Implemented"
//--------------------------------Update function for Tick3----------------------------------//



/// called with every mouse operation if model.MouseIsTick3 = true
/// returns the desired new Tick3 part of model based on the mouse event
let updateTick3 (model: Model3) (mMsg: MouseT): Model3 =
    match mMsg.Op with
    | Down ->
        findClickedItem mMsg.Pos model 
        |> Option.map (fun thingToDrag -> startDragging thingToDrag model)
        |> Option.defaultValue model
    | Up -> stopDragging model
    | Move -> model // do nothing
    | Drag -> dragThing mMsg.Pos model


