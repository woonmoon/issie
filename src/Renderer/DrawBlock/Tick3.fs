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
}

type RenderThingProps = Thing

let tick3Init() : Model3 = 
    {
        Things = Map.empty
    }


let doDrawing x y x1 x2 =
    failwithf "Not Impelmented"


let renderThing  =        
        FunctionComponent.Of(
            fun (thingProps : RenderThingProps) ->
                g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" thingProps.X thingProps.Y) ] ]) (doDrawing thingProps.X thingProps.Y thingProps.X1 thingProps.X2)
                
            , "Symbol"
            , equalsButFunctions
            )
let renderTick3 (model: Model3) display = 
    model.Things
    |> Map.toList
    |> List.map (fun (_,thing) -> renderThing thing)
    |> ofList



