module Main exposing (main)

import Angle
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Camera3d exposing (Camera3d)
import File exposing (File)
import File.Select
import Frame3d
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events as HE
import Json.Decode exposing (Value)
import Length exposing (Meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Point3d
import Quantity
import Task exposing (Task)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL exposing (Mesh, Shader)
import WebGL.Matrices
import WebGL.Texture as Texture exposing (Texture)


main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> ( Landing, Cmd.none )
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta AnimationFrame
        , update = update
        }


type Model
    = Landing
    | LoadingTexture
    | ErrorLoadingTexture Texture.Error
    | Rendering Texture (Mesh Vertex) ( Int, Int ) Float


type Msg
    = AnimationFrame Float
    | ImageLoaded File
    | UrlGenerated String
    | TextureLoaded (Result Texture.Error Texture)
    | ClickedSelectImageButton


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
            ( Rendering texture (gridMesh w h) ( w, h ) 0, Cmd.none )

        ( AnimationFrame elapsed, Rendering texture mesh size currentTime ) ->
            ( Rendering texture mesh size (currentTime + elapsed), Cmd.none )

        _ ->
            ( model, Cmd.none )


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

        Rendering texture mesh ( w, h ) currentTime ->
            WebGL.toHtml
                [ width 800
                , height 800
                , style "display" "block"
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    mesh
                    { modelViewProjection = modelViewProjection (centerTarget w h) (currentTime / 100)
                    , directionalLight = vec3 0 0 -1
                    , texture = texture
                    }
                ]



-- Camera


centerTarget : Int -> Int -> ( Float, Float )
centerTarget w h =
    let
        maxSize =
            toFloat (max w h)
    in
    ( 0.5 * toFloat w / maxSize, 0.5 * toFloat h / maxSize )


modelViewProjection : ( Float, Float ) -> Float -> Mat4
modelViewProjection target t =
    WebGL.Matrices.modelViewProjectionMatrix
        Frame3d.atOrigin
        (camera target t)
        { nearClipDepth = Length.meters 0.01
        , farClipDepth = Length.meters 100
        , aspectRatio = 1
        }


camera : ( Float, Float ) -> Float -> Camera3d Meters coordinates
camera target t =
    Camera3d.perspective
        { viewpoint = viewpoint target t
        , verticalFieldOfView = Angle.degrees 30
        }


viewpoint : ( Float, Float ) -> Float -> Viewpoint3d Meters coordinates
viewpoint ( targetX, targetY ) t =
    Viewpoint3d.orbitZ
        { focalPoint = Point3d.xyz (Length.meters targetX) (Length.meters targetY) Quantity.zero
        , azimuth = Angle.degrees t
        , elevation = Viewpoint3d.isometricElevation
        , distance = Length.meters 3
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
