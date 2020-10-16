module Main exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Camera3d exposing (Camera3d)
import File exposing (File)
import File.Select
import Frame3d
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
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
    , controls : Controls
    }


type alias Controls =
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


initialControls : ( Float, Float ) -> Controls
initialControls ( targetX, targetY ) =
    { controlling = NoControl
    , focalPoint = Point3d.xyz (Length.meters targetX) (Length.meters targetY) Quantity.zero
    , azimuth = Angle.degrees 90
    , elevation = Viewpoint3d.isometricElevation
    , orbitDistance = Length.meters 3
    }



-- Update


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Rendering r ->
            case r.controls.controlling of
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
      -- Controls
    | ZoomIn
    | ZoomOut
    | MouseDown Mouse.Event
    | MouseMove ( Float, Float )
    | MouseUp


loadTexture : String -> Task Texture.Error Texture
loadTexture =
    Texture.loadWith
        { magnify = Texture.linear
        , minify = Texture.nearest
        , horizontalWrap = Texture.clampToEdge
        , verticalWrap = Texture.clampToEdge
        , flipY = False
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
                , controls = initialControls (centerTarget ( w, h ))
                }
            , Cmd.none
            )

        ( AnimationFrame elapsed, Rendering r ) ->
            ( Rendering { r | currentTime = r.currentTime + elapsed }
            , Cmd.none
            )

        -- Controls
        ( ZoomIn, Rendering r ) ->
            ( Rendering { r | controls = controlZoomIn r.controls }, Cmd.none )

        ( ZoomOut, Rendering r ) ->
            ( Rendering { r | controls = controlZoomOut r.controls }, Cmd.none )

        ( MouseDown event, Rendering r ) ->
            ( Rendering { r | controls = controlMouseDown event r.controls }, Cmd.none )

        ( MouseUp, Rendering r ) ->
            ( Rendering { r | controls = controlMouseUp r.controls }, Cmd.none )

        ( MouseMove movement, Rendering r ) ->
            ( Rendering { r | controls = controlMouseMove movement r.controls }, Cmd.none )

        _ ->
            ( model, Cmd.none )


controlZoomIn : Controls -> Controls
controlZoomIn controls =
    { controls | orbitDistance = Quantity.multiplyBy (21 / 29.7) controls.orbitDistance }


controlZoomOut : Controls -> Controls
controlZoomOut controls =
    { controls | orbitDistance = Quantity.multiplyBy (29.7 / 21) controls.orbitDistance }


controlMouseDown : Mouse.Event -> Controls -> Controls
controlMouseDown event controls =
    let
        controlling =
            if event.keys.ctrl then
                Orbiting

            else
                Panning
    in
    { controls | controlling = controlling }


controlMouseUp : Controls -> Controls
controlMouseUp controls =
    { controls | controlling = NoControl }


controlMouseMove : ( Float, Float ) -> Controls -> Controls
controlMouseMove ( dx, dy ) controls =
    case controls.controlling of
        NoControl ->
            controls

        Orbiting ->
            orbit dx dy controls

        Panning ->
            pan dx dy controls


orbit : Float -> Float -> Controls -> Controls
orbit dx dy controls =
    let
        minElevation =
            Angle.degrees -90

        maxElevation =
            Angle.degrees 90
    in
    { controlling = controls.controlling
    , focalPoint = controls.focalPoint
    , azimuth = Quantity.plus (Angle.degrees -dx) controls.azimuth
    , elevation = Quantity.clamp minElevation maxElevation (Quantity.plus (Angle.degrees dy) controls.elevation)
    , orbitDistance = controls.orbitDistance
    }


pan : Float -> Float -> Controls -> Controls
pan dx dy controls =
    let
        viewPoint =
            Viewpoint3d.orbitZ
                { focalPoint = controls.focalPoint
                , azimuth = controls.azimuth
                , elevation = controls.elevation
                , distance = controls.orbitDistance
                }

        displacement =
            Vector3d.xyOn (Viewpoint3d.viewPlane viewPoint)
                (Quantity.multiplyBy (-0.001 * dx) controls.orbitDistance)
                (Quantity.multiplyBy (0.001 * dy) controls.orbitDistance)
    in
    { controlling = controls.controlling
    , focalPoint = Point3d.translateBy displacement controls.focalPoint
    , azimuth = controls.azimuth
    , elevation = controls.elevation
    , orbitDistance = controls.orbitDistance
    }



-- View


view : Model -> Html Msg
view model =
    case model of
        Landing ->
            Html.button
                [ HE.onClick ClickedSelectImageButton ]
                [ Html.text "Select a PNG image containing normals and depth" ]

        LoadingTexture ->
            Html.text "Loading texture ..."

        ErrorLoadingTexture _ ->
            Html.text "X: An error occurred when loading texture"

        Rendering { depthMap, mesh, controls } ->
            WebGL.toHtml
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
                    { modelViewProjection = modelViewProjection controls
                    , directionalLight = vec3 0 0 -1
                    , texture = depthMap
                    }
                ]


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


modelViewProjection : Controls -> Mat4
modelViewProjection controls =
    WebGL.Matrices.modelViewProjectionMatrix
        Frame3d.atOrigin
        (camera controls)
        { nearClipDepth = Length.meters 0.01
        , farClipDepth = Length.meters 100
        , aspectRatio = 1
        }


camera : Controls -> Camera3d Meters ()
camera controls =
    Camera3d.perspective
        { viewpoint = viewpoint controls
        , verticalFieldOfView = Angle.degrees 30
        }


viewpoint : Controls -> Viewpoint3d Meters ()
viewpoint controls =
    Viewpoint3d.orbitZ
        { focalPoint = controls.focalPoint
        , azimuth = controls.azimuth
        , elevation = controls.elevation
        , distance = controls.orbitDistance
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
            vnormal = tex.xyz;
            vcolor = vec3(tex.w);
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
            float intensity = - dot(normal, directionalLight);

            // gl_FragColor = vec4(vcolor, 1.0);
            gl_FragColor = vec4(intensity, intensity, intensity, 1.0);
        }

    |]
