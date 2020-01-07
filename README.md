# Sphere tracing signed distance functions

[![Build Status](https://travis-ci.org/sungiant/sdf.png?branch=master)][travis]
[![License](https://img.shields.io/badge/license-MIT-lightgrey.svg)][license]

This stand-alone repository illustrates the basics of both the sphere tracing method of ray marching and constructive solid geometry modelling with signed distance fields.

The renderer is written specifically to run exclusively on the CPU instead of the GPU so as to best illustrate the entire rendering pipeline as clearly and concisely as possible.

When run, the program produces this image (writing it to disk in PNG format):

![composite](/renders/render-00-composite.png?raw=true)

To quickly run this program, install `sbt` (the interactive Scala build tool), clone this repository and then from the root of the repository run:

```
sbt run
```

The project is built from scratch without any external dependencies; the program itself is less than six hundred lines of functional Scala in its entirety.

```
$ cloc --include-lang=Scala .
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Scala                            1             96             97            565
-------------------------------------------------------------------------------
```

## Concepts

The process of generating the image above is based on the combination of the following concepts:

### Constructive Solid Geometry

Constructive solid geometry is the technique of using boolean operators to combine geometrical objects.  A practical way to represent CSG objects is with a binary tree - with leaves representing primitives and nodes representing operations.

<img src="/docs/csg.png" style="float: right;" />

The following code snippet shows how CSG trees are implemented within this demo:

```scala
object CSG {
  trait Op
  object Op {
    object Union extends Op
    object Intersection extends Op
    object Difference extends Op
  }

  type Tree = Either[Tree.Node, Tree.Leaf]
  object Tree {
    case class Node (operation: Op, left: Tree, right: Tree)
    case class Leaf (sdf: SDF, material: Material.ID)
    def apply (operation: Op, left: Tree, right: Tree): Tree = Left (Node (operation, left, right))
    def apply (sdf: SDF): Tree = Right (Leaf (sdf, 0))
    def apply (sdf: SDF, material: Material.ID): Tree = Right (Leaf (sdf, material))
  }
}
```

Constructive solid geometry is a powerful abstraction that provides a language with which complex geometry can be defined using relatively simple building blocks.


### Signed Distance Fields

Given a position in 3D space (`p`), a signed distance field, as a construct, can be used to query both the distance between `p` and the nearest surface and whether `p` is inside or outside of the surface; the resultant value of a signed distance field query is a signed real number, the magnitude of which indicates the distance between `p` and the surface, the sign indicates whether `p` lies inside (negative) or outside (positive) the surface.

Signed distance fields are often used in modern game engines where a discrete sampling of points in 3D space (which may or may not be updated at runtime) is packed into a 3D texture and then used as a quantised signed distance field lookup to facilitate the implementation of fast realtime shadows.

Another mechanism for constructing a queryable signed distance field is with pure algebra.  Take for example the surface of a [unit sphere](https://en.wikipedia.org/wiki/Unit_sphere) and consider the results that a signed distance field query should yield for the following input points:

* `p` = `(0.0, 0.75, 0.0)` => inside the unit sphere, value: `-0.25`
* `p` = `(0.0, 1.25, 0.0)` => outside the unit sphere, value: `0.25`
* `p` = `(1.0, 1.0, 1.0)` => on the surface of the unit sphere, value: `0.0`
* `p` = `(0.4, 0.0, 0.3)` => inside the unit sphere, value: `-0.5`

In this case it is clear that the results are directly related to the magnitude of `p`.

This can be written algebraically in the form: `f (p): SQRT (p.x*p.x + p.y*p.y + p.z*p.z) - 1.0`.

More complex and flexible shapes can be defined with more complex equations.

For example, a very simple extension to the above would be to generalise the equation to work with spheres, again centered at the origin, but additionally of any radius: `f (p): SQRT (p.x*p.x + p.y*p.y + p.z*p.z) - RADIUS`.

The following code snippet shows this demo's algebraic implementations of various signed distance fields:

```scala
// Signed distance function for a unit sphere (radius = 1).
def sphere (offset: Vector, radius: Double)(position: Vector): Double = (position - offset).length - radius

// Signed distance function for a unit cube (h, w, d = 1).
def cube (offset: Vector, size: Double)(position: Vector): Double = {
  val d: Vector = (position - offset).abs - (Vector (size, size, size) / 2.0)
  val insideDistance: Double = Math.min (Math.max (d.x, Math.max (d.y, d.z)), 0.0)
  val outsideDistance: Double = d.max (0.0).length
  insideDistance + outsideDistance
}

def cuboid (offset: Vector, size: Vector)(position: Vector): Double = {
  val q: Vector = (position - offset).abs - (size / 2.0)
  q.max (0.0).length + Math.min (Math.max (q.x, Math.max (q.y, q.z)), 0.0)
}
```


### Combining SDFs using CSG

Signed distance functions are particularly suited to being combined using the principles of constructive solid geometry, this is because the boolean operators involved can be mapped directly to mathematical operators:

```scala
type SDF = Vector => Double

def evaluate (scene: CSG.Tree): SDF = {
  def r (t: CSG.Tree): SDF = t match {
    case Right (leaf) => leaf.value
    case Left (node) => (pos: Vector) => {
      val a = pos |> r (node.left)
      val b = pos |> r (node.right)
      node.value match {
        case Op.Intersection => Math.max (a,  b)
        case Op.Union        => Math.min (a,  b)
        case Op.Difference   => Math.max (a, -b)
      }
    }
  }
  r (scene)
}
```


### Sphere Tracing SDFs

Given a point `p` we can use an SDF to quickly determine the distance between that point and the nearest surface defined by that SDF.

| Standard SDF query |
|:---:|
|<img src="/docs/sdf_query.png" width="320" height="180" />|

Ideally however it would be very useful if we could find the distance from our point `p` to the nearest surface constrained along a specific direction.

| Constrained SDF query |
|:---:|
|<img src="/docs/constrained_sdf_query.png" width="320" height="180" />|

Ray constrained SDF queries are an essential tool for working with SDFs and be achieved through a technique known as ray-marching.  In this demo a particular optimised specialisation known as sphere tracing is used.  Here's how it works:

* Given a starting point `p0` and a direction (i.e. a ray) begin by querying the SDF as usual to produce a depth result `r0`.
* Next step along the ray from `p0` by a distance of `r0` and query again.
* Continue this process until the result returned is approximately zero.
* Finally sum all results to produce the depth value required.
* Additionally, if the number of steps taken exceeds an abitary predefined threshold, stop the process and assume that the ray does not hit a suface.

<img src="/docs/sphere_tracing.png" style="float: right;" />

The following code sample shows the part of this demo that implements the sphere tracing algorithm:

```scala
object Algorithm {

  // distance: distance to surface intersection (if intersection was found) from start of ray.
  case class March (distance: Option[(Double, Material.ID)], settings: March.Settings, stats: March.Stats)

  object March {
    case class Settings (iterationLimit: Int, minimumStep: Double, tolerance: Double)
    object Settings { lazy val default = Settings (256, 0.001, 0.0001) }

    // minimumConeRatio: the minimum result of the ratio of all individual sdf results along the march over the
    //                   total distance covered at that iteration.
    // iterations:       number of iterations performed
    case class Stats (minimumConeRatio: Double, iterations: Int)
  }

  // Given a ray and an SDF recursively evaluates the SDF until an intersection is either found or the limit of
  // iterations is reached. Returns the distance along the ray to the first intersection.
  def march (settings: March.Settings)(start: Double, end: Double, sdf: MaterialSDF, ray: Ray): March = {
    @scala.annotation.tailrec
    def step (distanceCovered: Double, minConeRatio: Double, stepCount: Int, lastMaterial: Material.ID): March = {
      stepCount match {
        case currentStep if currentStep == settings.iterationLimit =>
          // We've run out of marching steps and not found a sausage.  Perhaps we should assume we have hit
          // something though, as normally when we run out of iteration steps we are close to something.
          March (Some (distanceCovered, lastMaterial), settings, March.Stats (minConeRatio, settings.iterationLimit))
        case _ =>
          ((ray.position + ray.direction * distanceCovered) |> sdf) match {
            case (nextStepSize, m) if nextStepSize < settings.tolerance =>
              // Hit! `p` is within `settings.tolerance` being considered on the surface.
              March (Some ((distanceCovered, m)), settings, March.Stats (minConeRatio, stepCount))
            case (nextStepSize, m) =>
              distanceCovered + nextStepSize match { // new distance along the ray
                case newDistanceCovered if newDistanceCovered >= end =>
                  // We've marched out of the camera's view frustum.
                  March (None, settings, March.Stats (minConeRatio, stepCount))
                case newDistanceCovered =>
                  val nextDistanceCovered = Math.max (settings.minimumStep, newDistanceCovered)
                  val newMinConeRatio = if (distanceCovered == 0.0) Double.MaxValue
                                        else Math.min (minConeRatio, nextStepSize / distanceCovered)
                  step (nextDistanceCovered, newMinConeRatio, stepCount + 1, m)
              }
          }
      }
    }
    step (start, minConeRatio = Double.MaxValue, stepCount = 0, 0)
  }
}
```

### Rasterization

Given a scene defined using SDFs and CSG the process of producing the image above is done by rasterizing the information available into the pixels  using the sphere tracing method described above once for each pixel of the final image.

<img src="/docs/raster.png" width="600" style="float: right;" />

To produce a rasterization of an SDF:

* select a position for the camera.
* draw a grid in front of the camera at an appropriate position to capture the required field of view.
* select an appropriate grid size, each grid square corresponds to a pixel of the output image.
* send a ray (using sphere tracing) from the camera through the center of each grid square (storing the results).

## Rendering Techniques

The final render in this demo is a standard composition of multiple common datasets (just like the z-buffer and g-buffers in a [deferred renderer](https://en.wikipedia.org/wiki/Deferred_shading)).  The datasets themselves are nothing new or special - the interesting part here is (given our unconventional SDF scene definition) how we go about producing the datasets.

### Depth

The most logical dataset to produce from a scene SDF is the depth buffer as each SDF query yields a depth value.  Given a particular camera position and setup the rasterization of a scene SDF is naturally the depth buffer.


| Depth buffer | Processing cost |
|:---:|:---:|
|<img src="/renders/render-02-depth.png" width="320" height="180" />|<img src="/renders/render-04-depth-steps.png" width="320" height="180" />|

This dataset can be easily produced by sphere tracing the scene SDF with a ray corresponding to each camera pixel.  The image to the right gives an indication of the cost of calculating the depth value for each pixel.

### Normals

Signed distance functions do not directly provide a way to access surface normals, however, surface normals can be easily estimated by making additional queries of the scene SDF on each axes around given point on a surface.

```scala
type SDF = Vector => Double
val NORM_SAMPLE = 0.001
def estimateNormal (pos: Vector, sdf: SDF) = Vector (
  sdf (Vector (pos.x + NORM_SAMPLE, pos.y, pos.z)) - sdf (Vector (pos.x - NORM_SAMPLE, pos.y, pos.z)),
  sdf (Vector (pos.x, pos.y + NORM_SAMPLE, pos.z)) - sdf (Vector (pos.x, pos.y - NORM_SAMPLE, pos.z)),
  sdf (Vector (pos.x, pos.y, pos.z + NORM_SAMPLE)) - sdf (Vector (pos.x, pos.y, pos.z - NORM_SAMPLE))).normalise
```

| Normals buffer |
|:---:|
|<img src="/renders/render-05-normals.png" width="320" height="180" />|

This technique introduces, for each pixel representing a surface, a performance hit of six additional queries of the Scene SDF.


### Albedo

In this demo the signed distance function used to represent the scene is composed of multiple signed distance functions using constructive solid geometry.  By augmenting the signature of a standard signed distance function it is possible to introduce per object material identification data, which can then be used to produce buffers like this Albedo buffer:

| Albedo buffer |
|:---:|
|<img src="/renders/render-01-albedo.png" width="320" height="180" />|

Augmenting the signature of the SDF such that, in addition to returning a depth value, a material identifier associated with the object at the given location is also returned can be used in conjunction with an augmented version of the function above to support CSG composition of objects with material associations.  The following snippet shows how the CSG evaluation function can be adjusted:

```scala
type MaterialSDF = Vector => (Double, Material.ID)

def evaluate (scene: CSG.Tree): MaterialSDF = {
  def r (t: CSG.Tree): MaterialSDF = t match {
    case Right (leaf) => (v: Vector) => (v |> leaf.sdf, leaf.material)
    case Left (node) => (pos: Vector) => {
      val a = pos |> r (node.left)
      val b = pos |> r (node.right)
      node.operation match {
        case Op.Union => (a,  b) match {
          case ((a, am), (b, _)) if a <= b => (a, am)
          case (_, (b, bm)) => (b, bm)
        }
        case Op.Intersection => (a,  b) match {
          case ((a, am), (b, _)) if a >= b => (a, am)
          case ((_, am), (b, _)) => (b, am)
        }
        case Op.Difference => (a,  b) match {
          case ((a, am), (b, _)) if a >= -b => (a, am)
          case ((_, am), (b, _)) => (-b, am)
        }
      }
    }
  }
  r (scene)
}
 ```

This technique makes it easy to assign material identifiers to simple SDF shapes and then compose them in the conventional manner using CSG.

### Shadows

Calculating shadows in this context is surprisingly straightforward.  For each pixel the depth buffer can be used to calculate the point `p` in the scene corresponding to the position on the surface for that pixel (if there is one), then calculate a ray from `p` towards a light source at position `l`.  Next sphere trace along that ray - if the resultant depth is less than the distance between `p` and `l` we can determine that the surface at `p` is occluded by something and as such the pixel should be considered to be in shadow.

| Hard Shadows | Processing cost | 
|:---:|:---:|
|<img src="/renders/render-06-shadow-hard.png" width="320" height="180" />|<img src="/renders/render-08-shadow-steps.png" width="320" height="180" />|

Soft shadows are also straightforward, albeit they require keeping track of additional data whilst sphere tracing the rays towards the light sources.  The particular data needed is the minimum cone ratio observed whilst sphere tracing.  The cone ratio can be calculated each iteration as the ratio of the step size over the total distance covered.  The minimum observed result is then directly proportional to the soft shadow penumbra.  Ignation Quilezles does a great job of explaining this in detail [here](https://www.iquilezles.org/www/articles/rmshadows/rmshadows.htm).

| Soft Shadows |
|:---:|
|<img src="/renders/render-07-shadow-soft.png" width="320" height="180" />|

Multiple light sources can be easily supported by casting more rays and then combining the shadow results as desired.  To calculate shadows each pixel requires one additional sphere tracing operation for each shadow casting light source.

### Ambient Occlusion

This demo implements a simple approximation for ambient occlusion.  For each pixel corresponding to a point `p` on a surface the algorithm asks the question "how far is it to the nearest neighboring surface".  The particulars of the algorithm used in the demo are described [here](http://bentonian.com/Lectures/FGraphics1819/7.%20Global%20Illumination.pdf) on (page 13).

| Ambient Occlusion |
|:---:|
|<img src="/renders/render-09-ambient-occlusion.png" width="320" height="180" />|

Essentially by ray marching (i.e. fixed size steps) a small distance from `p` along the surface normal at `p` and keeping track of the cone ratio we can produce a nice approximation of ambient occlusion:

```scala
(0 until input.depthPass.input.h * input.depthPass.input.w).map { i =>
  (input.depthPass.surfacePosition (i), input.normalsPass.output.surfaceNormals (i)) match {
    case (Some (p), Some (n)) =>
      val step = 0.01
      val lim = 0.1
      @scala.annotation.tailrec
      def calcAOR (a: Double, t: Double): Double = {
        if (t > lim) a
        else {
          val samplePosition = p + (n * t)
          val d = Math.abs (input.depthPass.input.sdf (samplePosition)._1)
          val a2 = Math.min (a, d / t)
          val t2 = t + Math.max (d, step)
          calcAOR (a2, t2)
        }
      }
      calcAOR (1.0, step)
    case _ => 1.0
  }
}
```

### Lighting

The lighting technique used in the demo is simply an application of the Phong lighting model.  The input data required to apply the Phong lighting model is limited to surface positions, surface normals, material properties and camera location; all required input data is easily derived from calculations used and described already, as such, when it comes to this particular lighting technique, there is no relevance or change in approach needed due to the demo scene being defined as a SDF.

| Ambient component | Diffuse component | Specular component |
|:---:|:---:|:---:|
|<img src="/renders/render-10-phong-ambient.png" width="256" height="144" />|<img src="/renders/render-11-phong-diffuse.png" width="256" height="144" />|<img src="/renders/render-12-phong-specular.png" width="256" height="144" />|


### Compositing

The final image is a composition, using varied blending techniques, of the data buffers produced during the execution of the demo and illustrated above.

| Aggregate composition |
|:---:|
|<img src="/renders/render-00-composite.png" width="320" height="180" />|


## References

* [Ignation Quilezles' Homepage](https://www.iquilezles.org/www/index.htm)
* [Jamie Wong, Ray marching signed distance functions](http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/)
* [Alex Benton, Ray marching and signed distance fields](http://bentonian.com/Lectures/FGraphics1819/1.%20Ray%20Marching%20and%20Signed%20Distance%20Fields.pdf)
* [9-bit Science, Raymarching distance fields](http://9bitscience.blogspot.com/2013/07/raymarching-distance-fields_14.html)
* [Wikipedia, Signed Distance Function](https://en.wikipedia.org/wiki/Signed_distance_function)


## License

This software is licensed under the **[MIT License][license]**; you may not use this software except in compliance with the License.

```
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

[travis]: https://travis-ci.org/sungiant/sdf
[license]: https://raw.githubusercontent.com/sungiant/sdf/master/LICENSE

