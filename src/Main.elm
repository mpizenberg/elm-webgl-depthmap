module Main exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Camera3d exposing (Camera3d)
import Direction3d
import File exposing (File)
import File.Select
import Frame3d
import Html exposing (Html)
import Html.Attributes as HA exposing (height, style, width)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode exposing (Decoder, Value)
import Length exposing (Length, Meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Point3d exposing (Point3d)
import Quantity
import Task exposing (Task)
import Vector3d
import Viewpoint3d exposing (Viewpoint3d)
import WebGL exposing (Mesh, Shader)
import WebGL.Matrices
import WebGL.Texture as Texture exposing (Texture)


main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> ( Landing, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type Model
    = Landing
    | LoadingTexture
    | ErrorLoadingTexture Texture.Error
    | Rendering RenderingModel


type alias RenderingModel =
    { depthMap : Texture
    , size : ( Int, Int )
    , mesh : Mesh Vertex
    , currentTime : Float
    , camera : Camera
    , lighting : Lighting
    }


type alias Lighting =
    { azimuth : Angle
    , elevation : Angle
    }


initialLighting : Lighting
initialLighting =
    { azimuth = Quantity.zero
    , elevation = Angle.degrees 90
    }


type alias Camera =
    { controlling : Controlling
    , focalPoint : Point3d Meters ()
    , azimuth : Angle
    , elevation : Angle
    , orbitDistance : Length
    }


type Controlling
    = NoControl
    | Orbiting
    | Panning


initialCamera : ( Float, Float ) -> Camera
initialCamera ( targetX, targetY ) =
    { controlling = NoControl
    , focalPoint = Point3d.xyz (Length.meters targetX) (Length.meters targetY) Quantity.zero
    , azimuth = Angle.degrees -90
    , elevation = Viewpoint3d.isometricElevation
    , orbitDistance = Length.meters 3
    }



-- Update


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Rendering r ->
            case r.camera.controlling of
                NoControl ->
                    onAnimationFrameDelta AnimationFrame

                _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (Decode.map MouseMove movementDecoder)
                        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                        ]

        _ ->
            Sub.none


movementDecoder : Decoder ( Float, Float )
movementDecoder =
    Decode.map2 (\a b -> ( a, b ))
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


type Msg
    = AnimationFrame Float
    | ImageLoaded File
    | UrlGenerated String
    | TextureLoaded (Result Texture.Error Texture)
    | ClickedSelectImageButton
      -- Camera
    | ZoomIn
    | ZoomOut
    | MouseDown Mouse.Event
    | MouseMove ( Float, Float )
    | MouseUp
      -- Lighting
    | ChangeLightAzimuth Float
    | ChangeLightElevation Float


loadTexture : String -> Task Texture.Error Texture
loadTexture =
    Texture.loadWith
        { magnify = Texture.linear
        , minify = Texture.nearest
        , horizontalWrap = Texture.clampToEdge
        , verticalWrap = Texture.clampToEdge
        , flipY = True
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedSelectImageButton, Landing ) ->
            ( Landing, File.Select.file [ "image/png" ] ImageLoaded )

        ( ImageLoaded file, Landing ) ->
            ( LoadingTexture, Task.perform UrlGenerated (File.toUrl file) )

        ( UrlGenerated url, LoadingTexture ) ->
            ( LoadingTexture, Task.attempt TextureLoaded (loadTexture url) )

        ( TextureLoaded (Err err), _ ) ->
            ( ErrorLoadingTexture err, Cmd.none )

        ( TextureLoaded (Ok texture), _ ) ->
            let
                ( w, h ) =
                    Texture.size texture
            in
            ( Rendering
                { depthMap = texture
                , mesh = gridMesh w h
                , size = ( w, h )
                , currentTime = 0
                , camera = initialCamera (centerTarget ( w, h ))
                , lighting = initialLighting
                }
            , Cmd.none
            )

        ( AnimationFrame elapsed, Rendering r ) ->
            ( Rendering { r | currentTime = r.currentTime + elapsed }
            , Cmd.none
            )

        -- Camera
        ( ZoomIn, Rendering r ) ->
            ( Rendering { r | camera = controlZoomIn r.camera }, Cmd.none )

        ( ZoomOut, Rendering r ) ->
            ( Rendering { r | camera = controlZoomOut r.camera }, Cmd.none )

        ( MouseDown event, Rendering r ) ->
            ( Rendering { r | camera = controlMouseDown event r.camera }, Cmd.none )

        ( MouseUp, Rendering r ) ->
            ( Rendering { r | camera = controlMouseUp r.camera }, Cmd.none )

        ( MouseMove movement, Rendering r ) ->
            ( Rendering { r | camera = controlMouseMove movement r.camera }, Cmd.none )

        -- Lighting
        ( ChangeLightAzimuth az, Rendering r ) ->
            ( Rendering { r | lighting = changeLightAzimuth az r.lighting }, Cmd.none )

        ( ChangeLightElevation el, Rendering r ) ->
            ( Rendering { r | lighting = changeLightElevation el r.lighting }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Camera


controlZoomIn : Camera -> Camera
controlZoomIn camera =
    { camera | orbitDistance = Quantity.multiplyBy (21 / 29.7) camera.orbitDistance }


controlZoomOut : Camera -> Camera
controlZoomOut camera =
    { camera | orbitDistance = Quantity.multiplyBy (29.7 / 21) camera.orbitDistance }


controlMouseDown : Mouse.Event -> Camera -> Camera
controlMouseDown event camera =
    let
        controlling =
            if event.keys.ctrl then
                Panning

            else
                Orbiting
    in
    { camera | controlling = controlling }


controlMouseUp : Camera -> Camera
controlMouseUp camera =
    { camera | controlling = NoControl }


controlMouseMove : ( Float, Float ) -> Camera -> Camera
controlMouseMove ( dx, dy ) camera =
    case camera.controlling of
        NoControl ->
            camera

        Orbiting ->
            orbit dx dy camera

        Panning ->
            pan dx dy camera


orbit : Float -> Float -> Camera -> Camera
orbit dx dy camera =
    let
        minElevation =
            Angle.degrees 0

        maxElevation =
            Angle.degrees 90
    in
    { controlling = camera.controlling
    , focalPoint = camera.focalPoint
    , azimuth = Quantity.plus (Angle.degrees -dx) camera.azimuth
    , elevation = Quantity.clamp minElevation maxElevation (Quantity.plus (Angle.degrees dy) camera.elevation)
    , orbitDistance = camera.orbitDistance
    }


pan : Float -> Float -> Camera -> Camera
pan dx dy camera =
    let
        viewPoint =
            Viewpoint3d.orbitZ
                { focalPoint = camera.focalPoint
                , azimuth = camera.azimuth
                , elevation = camera.elevation
                , distance = camera.orbitDistance
                }

        displacement =
            Vector3d.xyOn (Viewpoint3d.viewPlane viewPoint)
                (Quantity.multiplyBy (-0.001 * dx) camera.orbitDistance)
                (Quantity.multiplyBy (0.001 * dy) camera.orbitDistance)
    in
    { controlling = camera.controlling
    , focalPoint = Point3d.translateBy displacement camera.focalPoint
    , azimuth = camera.azimuth
    , elevation = camera.elevation
    , orbitDistance = camera.orbitDistance
    }



-- Lighting


changeLightAzimuth : Float -> Lighting -> Lighting
changeLightAzimuth value lighting =
    { lighting | azimuth = Angle.degrees value }


changeLightElevation : Float -> Lighting -> Lighting
changeLightElevation value lighting =
    { lighting | elevation = Angle.degrees value }



-- View


view : Model -> Html Msg
view model =
    case model of
        Landing ->
            Html.div []
                [ Html.button
                    [ HE.onClick ClickedSelectImageButton ]
                    [ Html.text "Select a PNG image containing normals and depth" ]
                ]

        LoadingTexture ->
            Html.text "Loading texture ..."

        ErrorLoadingTexture _ ->
            Html.text "X: An error occurred when loading texture"

        Rendering { depthMap, mesh, camera, lighting } ->
            Html.div []
                [ lightControls lighting
                , WebGL.toHtml
                    [ width 800
                    , height 800
                    , style "display" "block"
                    , Wheel.onWheel chooseZoom
                    , Mouse.onDown MouseDown
                    ]
                    [ WebGL.entity
                        vertexShader
                        fragmentShader
                        mesh
                        { modelViewProjection = modelViewProjection camera
                        , directionalLight = directionalLight lighting
                        , texture = depthMap
                        }
                    ]
                ]


directionalLight : Lighting -> Vec3
directionalLight { azimuth, elevation } =
    Direction3d.xyZ azimuth elevation
        |> Direction3d.components
        |> (\( x, y, z ) -> vec3 x y z)


lightControls : Lighting -> Html Msg
lightControls lighting =
    Html.div []
        [ Html.p [] [ Html.text "Light direction" ]
        , Html.div []
            [ Html.text "Azimuth: 0"
            , Html.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "360"
                , HA.step "1"
                , HA.value (String.fromInt <| round <| Angle.inDegrees lighting.azimuth)
                , HE.stopPropagationOn "input" (valueDecoder ChangeLightAzimuth)
                ]
                []
            , Html.text "360 degrees"
            ]
        , Html.div []
            [ Html.text "Elevation: 0"
            , Html.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "90"
                , HA.step "1"
                , HA.value (String.fromInt <| round <| Angle.inDegrees lighting.elevation)
                , HE.stopPropagationOn "input" (valueDecoder ChangeLightElevation)
                ]
                []
            , Html.text "90 degrees"
            ]
        ]


valueDecoder : (Float -> Msg) -> Decoder ( Msg, Bool )
valueDecoder toMsg =
    Decode.at [ "target", "value" ] Decode.string
        |> Decode.map (String.toFloat >> Maybe.withDefault 0)
        |> Decode.map toMsg
        |> Decode.map (\x -> ( x, True ))


chooseZoom : Wheel.Event -> Msg
chooseZoom wheelEvent =
    if wheelEvent.deltaY > 0 then
        ZoomOut

    else
        ZoomIn



-- Camera


centerTarget : ( Int, Int ) -> ( Float, Float )
centerTarget ( w, h ) =
    let
        maxSize =
            toFloat (max w h)
    in
    ( 0.5 * toFloat w / maxSize, 0.5 * toFloat h / maxSize )


modelViewProjection : Camera -> Mat4
modelViewProjection camera =
    WebGL.Matrices.modelViewProjectionMatrix
        Frame3d.atOrigin
        (persectiveCamera camera)
        { nearClipDepth = Length.meters 0.01
        , farClipDepth = Length.meters 100
        , aspectRatio = 1
        }


persectiveCamera : Camera -> Camera3d Meters ()
persectiveCamera camera =
    Camera3d.perspective
        { viewpoint = viewpoint camera
        , verticalFieldOfView = Angle.degrees 30
        }


viewpoint : Camera -> Viewpoint3d Meters ()
viewpoint camera =
    Viewpoint3d.orbitZ
        { focalPoint = camera.focalPoint
        , azimuth = camera.azimuth
        , elevation = camera.elevation
        , distance = camera.orbitDistance
        }



-- Matrix


type alias Matrix a =
    List (List a)


toTriangleStrip : Matrix a -> List a
toTriangleStrip =
    stripHelper []


stripHelper : List a -> Matrix a -> List a
stripHelper accum matrix =
    case matrix of
        currentLine :: nextLine :: followingLines ->
            stripHelper (joinTerminatedStrip currentLine nextLine accum) (nextLine :: followingLines)

        _ ->
            accum


joinTerminatedStrip : List a -> List a -> List a -> List a
joinTerminatedStrip line1 line2 accum =
    case ( line1, line2 ) of
        -- Beginning of the strip, we double the first vertex
        -- to cut from the previous strip with an empty triangle (not drawn)
        ( x1 :: x1s, x2 :: x2s ) ->
            terminatedStrip x1s x2s (x2 :: x1 :: x1 :: accum)

        _ ->
            accum


terminatedStrip : List a -> List a -> List a -> List a
terminatedStrip line1 line2 accum =
    case ( line1, line2, accum ) of
        -- Middle of the strip, just continue progressing.
        ( x1 :: ((_ :: _) as x1s), x2 :: ((_ :: _) as x2s), _ ) ->
            terminatedStrip x1s x2s (x2 :: x1 :: accum)

        -- End of the strip, we double the last vertex
        -- to cut from the next strip with an empty triangle (not drawn)
        ( x1 :: _, x2 :: _, _ ) ->
            x2 :: x2 :: x1 :: accum

        _ ->
            accum



-- Mesh


type alias Vertex =
    { mapCoordinates : Vec2
    , position : Vec2
    }


gridMesh : Int -> Int -> Mesh Vertex
gridMesh w h =
    grid w h
        |> List.map (List.map (toVertex w h))
        |> toTriangleStrip
        |> WebGL.triangleStrip


grid : Int -> Int -> Matrix ( Int, Int )
grid w h =
    let
        xs =
            List.range 0 (w - 1)

        ys =
            List.range 0 (h - 1)
    in
    List.map (\x -> List.map (\y -> ( x, y )) ys) xs


toVertex : Int -> Int -> ( Int, Int ) -> Vertex
toVertex w h ( u, v ) =
    let
        x =
            toFloat u

        y =
            toFloat v

        widthScale =
            1.0 / toFloat w

        heightScale =
            1.0 / toFloat h

        scaleCoef =
            min widthScale heightScale
    in
    { mapCoordinates = vec2 (widthScale * x) (heightScale * y)
    , position = vec2 (scaleCoef * x) (scaleCoef * y)
    }



-- Shaders


type alias Uniforms =
    { modelViewProjection : Mat4
    , directionalLight : Vec3
    , texture : Texture
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3 }
vertexShader =
    [glsl|

        attribute vec2 mapCoordinates;
        attribute vec2 position;
        uniform mat4 modelViewProjection;
        uniform sampler2D texture;
        varying vec3 vcolor;
        varying vec3 vnormal;

        void main () {
            vec4 tex = texture2D(texture, mapCoordinates);
            float nx = 2.0 * tex.x - 1.0;
            float ny = 2.0 * tex.y - 1.0;
            float nz = 2.0 * tex.z - 1.0;
            vnormal = vec3(nx, ny, nz);
            vcolor = vec3(position, 0);
            gl_Position = modelViewProjection * vec4(position, tex.w / -10.0, 1.0);
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform vec3 directionalLight;
        varying vec3 vcolor;
        varying vec3 vnormal;

        void main () {
            // normalizing the normal varying
            vec3 normal = normalize(vnormal);

            // computing directional lighting
            float intensity = dot(normal, directionalLight);

            // gl_FragColor = vec4(vcolor, 1.0);
            // gl_FragColor = vec4(intensity, intensity, intensity, 1.0);
            gl_FragColor = vec4(intensity * vcolor, 1.0);
        }

    |]
