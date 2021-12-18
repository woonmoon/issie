module Tick3
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions
// unique Id for Thing objects
type ThingId = string

// represents a cicle or a rectangle
type Thing = {
    id: ThingId
    X: float // x coordinate of centre of Thing
    Y: float // y coordinate of centre of Thing
    X1: float // width of rectangle or radius of circle
    X2: float // height of circle
}

type Model3 = {
    Things: Map<ThingId,Thing>
    MouseIsTick3: bool
}

type RenderThingProps = Thing

let tick3Init() : Model3 = 
    {
        Things = Map.empty
        MouseIsTick3 = true  // switches between normal Issie operation, and mouse messages processed by Tick3 code
    }

     
let doDrawing x y x1 x2 : ReactElement list=
    // see DrawHelpers for some examples of how to draw.
    failwithf "Not Impelmented"


let renderThing  =        
        FunctionComponent.Of(
            fun (thingProps : RenderThingProps) ->
                g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" thingProps.X thingProps.Y) ] ]) (doDrawing thingProps.X thingProps.Y thingProps.X1 thingProps.X2)
                
            , "Thing"
            , equalsButFunctions
            )

let renderTick3 (model: Model3) display = 
    model.Things
    |> Helpers.mapValues
    |> Seq.toList
    |> List.map renderThing
    |> ofList

/// called with every mouse operation if model.MouseIsTick3 = true
/// returns the desired new Tick3 part of model based on teh mouse event
let updateTick3 (model: Model3) (mMsg: MouseT) =
    model

