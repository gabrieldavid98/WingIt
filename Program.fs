open System

type Point = { Lat: float; Lng: float }

type Vehicle =
    | Porsche
    | Tayato
    | Sleta

type TransportType =
    | Walking of Point * Point
    | Driving of Point * Point * Vehicle

type Itinerary =
    { Distance: float
      Cost: float
      Duration: float }

module Vehicle =
    let cost vehicle x =
        match vehicle with
        | Porsche -> 0.12 * 1.5 * x
        | Tayato -> 0.07 * 1.3 * x
        | Sleta -> 0.2 * 0.1 * x

let walkingSpeed = 4.0
let drivingSpeed = 70.0

let euclidenanDistance from to' =
    let degLen = 110.25
    let x = (to'.Lat - from.Lat) ** 2
    let y = (Math.Cos to'.Lat * (to'.Lng - from.Lng)) ** 2

    degLen * Math.Sqrt(x + y)

let itinerary =
    function
    | Walking(from, to') ->
        let distance = euclidenanDistance from to'
        let duration = distance / walkingSpeed

        { Distance = distance
          Cost = 0
          Duration = duration }
    | Driving(from, to', vehicle) ->
        let distance = euclidenanDistance from to'
        let cost = distance |> Vehicle.cost vehicle
        let duration = distance / drivingSpeed

        { Distance = distance
          Cost = cost
          Duration = duration }
