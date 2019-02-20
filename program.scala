object Program {

  // Extensions
  //------------------------------------------------------------------------------------------------------------------//
  implicit class I[A](val self: A) { def |>[B](f: A => B): B = f (self) }

  // Basic Type Definitions
  //------------------------------------------------------------------------------------------------------------------//
  // Defines the signature of a signed distance function.  The absolute value of the result indicates the distance to
  // the surface, the sign of the result indicates whether the position is inside or outside of the surface, negative
  // indicating inside.
  type SDF = Vector => Double
  case class Ray   (position: Vector, direction: Vector)
  case class Light (position: Vector, colour: Colour, castsShadows: Boolean)

  // Configuration
  //------------------------------------------------------------------------------------------------------------------//
  val EPSILON = 0.0001

  // Utility functions
  //------------------------------------------------------------------------------------------------------------------//
  def clamp01 (x: Double) = if (x < 0.0) 0.0 else if (x > 1.0) 1.0 else x
  def time[R](id: String)(block: => R): R = { // Not FP, urgh, do this properly later.
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println (s"[$id] Elapsed time: " + ((t1 - t0) / 1000000) + "ms")
    result
  }

  // Application
  //------------------------------------------------------------------------------------------------------------------//
  def main (args: Array[String]): Unit = demo (640, 360).foreach { case (n, i) => i |> Image.writePNG (s"renders/$n.png") _ }

  def demo (w: Int, h: Int): List[(String, Image)] = {
    val camera   : Camera          = Scene.camera (y = 4, zx = 6, fov = 57.5, aspect = w.toDouble / h.toDouble, near = 0.1, far = 100.0) 
    val lighting : List[Light]     = Scene.lighting
    val sdf      : SDF             = Scene.edits |> CSG.evaluate
    // Render pipeline
    val depthPass         = Renderer.DepthPass (w, h, sdf, camera)
    val normalsPass       = Renderer.NormalsPass (depthPass)
    val shadowPass        = Renderer.ShadowPass (depthPass, lighting)
    val aoPass            = Renderer.AmbientOcclusionPass (depthPass, normalsPass)
    val lightingPass      = Renderer.LightingPass (depthPass, normalsPass, lighting)
    val bundledPasses     = (depthPass, shadowPass, aoPass, lightingPass)

    // Visualise work done
    ("render-00-composite",              bundledPasses |> Compositor.aggregate) ::
    ("render-01-albedo",                 depthPass     |> Compositor.albedo) ::
    ("render-02-depth",                  depthPass     |> Compositor.depth) ::
    ("render-03-depth-cone",             depthPass     |> Compositor.depthCone) ::
    ("render-03-depth-steps",            depthPass     |> Compositor.depthStepsPretty) ::
    ("render-05-normals",                normalsPass   |> Compositor.normals) ::
    ("render-06-shadow",                 shadowPass    |> Compositor.shadow) ::
    ("render-07-shadow-cone",            shadowPass    |> Compositor.shadowCone) ::
    ("render-08-shadow-steps",           shadowPass    |> Compositor.shadowStepsPretty) ::
    ("render-09-ambient-occlusion",      aoPass        |> Compositor.ao) ::
    ("render-10-phong-ambient",          lightingPass  |> Compositor.phongAmbient) ::
    ("render-11-phong-diffuse",          lightingPass  |> Compositor.phongDiffuse) ::
    ("render-12-phong-specular",         lightingPass  |> Compositor.phongSpecular) :: Nil
  }

  // Scene
  //------------------------------------------------------------------------------------------------------------------//
  object Scene {

    def camera (y: Double, zx: Double, fov: Double, aspect: Double, near: Double, far: Double): Camera = Camera (
        Vector (zx, y, zx),
        Quaternion.fromYawPitchRoll (5.0 * math.Pi / 4.0, (math.Pi / 2.0) - ((Math.sqrt (zx * zx * 2.0) / y) |> Math.atan), 0.0),
        fov, aspect, near, far)

    def lighting: List[Light] =
      Light (Vector (-1, 1.5, 5), Colour (112, 0, 112), true) ::
      Light (Vector (6, 8, 2), Colour (160, 0, 0), true) :: Nil

    def edits: CSG.Tree = 
      CSG.Tree (CSG.Op.Union,
        CSG.Tree (CSG.Op.Difference,
          CSG.Tree (CSG.Op.Intersection,
            CSG.Tree (SDF.cube (Vector.zero, 2.0) _),
            CSG.Tree (SDF.sphere (Vector.zero, 2.4) _)),
          CSG.Tree (CSG.Op.Union,
            CSG.Tree (SDF.sphere (Vector.unitX, 0.8) _),
            CSG.Tree (CSG.Op.Union,
              CSG.Tree (SDF.sphere (Vector.unitY, 0.8) _),
              CSG.Tree (SDF.sphere (Vector.unitZ, 0.8) _)))),
        CSG.Tree (CSG.Op.Union,
          CSG.Tree (CSG.Op.Union,
            CSG.Tree (CSG.Op.Union,
              CSG.Tree (CSG.Op.Union,
                CSG.Tree (CSG.Op.Union,
                  CSG.Tree (CSG.Op.Union,
                    CSG.Tree (CSG.Op.Union,
                      CSG.Tree (CSG.Op.Union,
                        CSG.Tree (SDF.cube (Vector (0.0, -5.8, 0.0), 10.0) _),
                        CSG.Tree (SDF.cube (Vector (-3.5, 0.7, -3.5), 3.0) _)),
                      CSG.Tree (SDF.cube (Vector (2.0, -0.3, -4.5 ), 1.0) _)),
                    CSG.Tree (SDF.cube (Vector (-4.0, 0.2, 0.0 ), 2.0) _)),
                  CSG.Tree (SDF.cube (Vector (-4.5, -0.3, 2.0 ), 1.0) _)),
                CSG.Tree (SDF.sphere (Vector (0.65, 0.8, -0.65), 0.4) _)),
              CSG.Tree (SDF.sphere (Vector (-0.65, 0.8, -0.65), 0.4) _)),
            CSG.Tree (SDF.sphere (Vector (0.65, 0.8, 0.65), 0.4) _)),
          CSG.Tree (SDF.sphere (Vector (-0.65, 0.8, 0.65), 0.4) _)))
  }

  object CSG {
    // Enumeration of constructive solid geometry ops
    trait Op; object Op {
      object Union extends Op          // Merger of two objects (commutative)
      object Intersection extends Op   // Portion common to both objects (commutative)
      object Difference extends Op     // Subtraction of one object from another (not commutative)
    }

    type Tree = Either[Tree.Node, Tree.Leaf]
    object Tree {
      case class Node (value: Op, left: Tree, right: Tree)
      case class Leaf (value: SDF)
      def apply (value: Op, left: Tree, right: Tree): Tree = Left (Node (value, left, right))
      def apply (value: SDF): Tree = Right (Leaf (value))
    }

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
  }

  // Signed Distance Fields
  //------------------------------------------------------------------------------------------------------------------//
  object SDF {
    // Signed distance function for a unit sphere (diameter = 1).
    def sphere (offset: Vector, diameter: Double)(position: Vector): Double = (position - offset).length - (diameter / 2.0)

    // Signed distance function for a unit cube (h,w,d = 1).
    def cube (offset: Vector, size: Double)(position: Vector): Double = {
      val d: Vector = (position - offset).abs - (Vector (size, size, size) / 2.0)
      val insideDistance: Double = Math.min (Math.max (d.x, Math.max (d.y, d.z)), 0.0)
      val outsideDistance: Double = d.max (0.0).length
      insideDistance + outsideDistance
    }
  }

  // Algorithms
  //------------------------------------------------------------------------------------------------------------------//
  object Algorithm {

    // d: distance to surface intersection (if intersection was found) from star of ray.
    case class March (distance: Option[Double], settings: March.Settings, stats: March.Stats)

    object March {
      case class Settings (iterationLimit: Int, minimumStep: Double, tolerance: Double)
      object Settings { lazy val default = Settings (256, 0.001, 0.0001) }

      // m: the minimum result of the ratio of all individual sdf results along the march over the distance covered at that point.
      // i: number of iterations performed
      case class Stats (minimumConeRatio: Double, iterations: Int)
    }

    // Given a ray and an SDF recursively evaluates the SDF until an interestion is either found or the limit of iterations is reached.
    // Returns the distance along the ray to the first interesection.
    def march (settings: March.Settings)(start: Double, end: Double, sdf: (Vector) => Double, ray: Ray): March = {
      @scala.annotation.tailrec def step (distanceCovered: Double, minConeRatio: Double, stepCount: Int): March = stepCount match {
        case currentStep if currentStep == settings.iterationLimit => March (Some (distanceCovered), settings, March.Stats (minConeRatio, settings.iterationLimit)) // We've run out of marching steps and not found a sausage.  Perhaps we should assume we have hit something though, as normally when we run out of iteration steps we are close to something.
        case _ => ((ray.position + ray.direction * distanceCovered) |> sdf) match {
          case nextStepSize if nextStepSize < settings.tolerance => March (Some (distanceCovered), settings, March.Stats (minConeRatio, stepCount)) // Hit! `p` is within `settings.tolerance` being considered on the surface.
          case nextStepSize => distanceCovered + nextStepSize match { // new distance along the ray
            case newDistanceCovered if newDistanceCovered >= end => March (None, settings, March.Stats (minConeRatio, stepCount)) // We've marched out of the camera's view frustum.
            case newDistanceCovered =>
              val nextDistanceCovered = Math.max (settings.minimumStep, newDistanceCovered)
              val newMinConeRatio = if (distanceCovered == 0.0) Double.MaxValue else Math.min (minConeRatio, nextStepSize / distanceCovered)
              step (nextDistanceCovered, newMinConeRatio, stepCount + 1) }}
      }
      step (start, minConeRatio = Double.MaxValue, stepCount = 0)
    }

    def estimateNormal (pos: Vector, sdf: SDF) = { val NORM_SAMPLE = 0.001; Vector (
      sdf (Vector (pos.x + NORM_SAMPLE, pos.y, pos.z)) - sdf (Vector (pos.x - NORM_SAMPLE, pos.y, pos.z)),
      sdf (Vector (pos.x, pos.y + NORM_SAMPLE, pos.z)) - sdf (Vector (pos.x, pos.y - NORM_SAMPLE, pos.z)),
      sdf (Vector (pos.x, pos.y, pos.z + NORM_SAMPLE)) - sdf (Vector (pos.x, pos.y, pos.z - NORM_SAMPLE))).normalise }

    case class PhongReflection (ambient: Vector, diffuse: Vector, specular: Vector)
    def phong (surfacePosition: Vector, surfaceNormal: Vector, eyePosition: Vector, lights: List[Light]): PhongReflection = {
      val ambientColour = Vector (0.1, 0.1, 0.1)  // HARDCODED
      val specularColour = Vector.one             // HARDCODED
      val shininessCoefficient = 8.0              // HARDCODED
      val result = lights.foldLeft ((Vector.zero, Vector.zero)) { (a, i) =>
        val lightPosition = i.position
        val lightIntensity = Colour.toVector01 (i.colour)
        val N = surfaceNormal
        val L = (lightPosition - surfacePosition).normalise
        val V = (eyePosition - surfacePosition).normalise
        val R = Vector.reflect (-L, N).normalise
        val dotLN = Vector.dot (L, N)
        val dotRV = Vector.dot (R, V)
        val contrib =
          if (dotLN < 0.0) (Vector.zero, Vector.zero)
          else if (dotRV < 0.0) (lightIntensity * dotLN, Vector.zero)
          else (lightIntensity * dotLN, lightIntensity * specularColour * Math.pow (dotRV, shininessCoefficient))
        (a._1 + contrib._1, a._2 + contrib._2)
      }
      PhongReflection (ambientColour, result._1, result._2)
    }
  }

  // Renderer
  //------------------------------------------------------------------------------------------------------------------//
  object Renderer {

    trait Pass {
      def input: Pass.Input; def output: Pass.Output; def stats: Pass.Stats
      def width: Int;def height: Int
    }
    object Pass { trait Input; trait Output; trait Stats }

    //----------------------------------------------------------------------------------------------------------------//

    case class DepthPass (input: DepthPass.Input, output: DepthPass.Output, stats: DepthPass.Stats) extends Pass {
      lazy val (width, height) = (input.w, input.h)
      def surfacePosition (i: Int): Option[Vector] = output.depths (i).map (input.rays (i).position + input.rays (i).direction * _)

      // Right now just hard code all objects to WHITE
      def albedo (i: Int): Vector = output.depths (i) match { case None => Vector.zero; case Some (_) => Vector.one }
      def albedo (x: Int, y: Int): Vector = albedo (x + y * width)
    }
    object DepthPass {
      case class Input (w: Int, h: Int, sdf: SDF, camera: Camera) extends Pass.Input { lazy val rays = camera.pixelRays (w, h) }
      case class Output (depths: IndexedSeq[Option[Double]]) extends Pass.Output {
        lazy val depthsLowerBound = depths.collect { case Some (d) => d }.min
        lazy val depthsUpperBound = depths.collect { case Some (d) => d }.max
      }
      case class Stats (minimumConeRatio: IndexedSeq[Double], iterations: IndexedSeq[Int], iterationLimit: Int) extends Pass.Stats {
        lazy val minimumConeRatioLowerBound = minimumConeRatio.min
        lazy val minimumConeRatioUpperBound = minimumConeRatio.max
        lazy val averageIterations = iterations.sum.toDouble / iterations.size.toDouble
      }

      // To run a depth pass you need the scene definition given by an SDF, you need a camera to define what is being looked at and you need a pixel size for the rastered output.
      def apply (w: Int, h: Int, sdf: SDF, camera: Camera): DepthPass = time ("depth-pass") { DepthPass.Input (w, h, sdf, camera) |> process }

      def process (input: Input): DepthPass = {
        val settings = Algorithm.March.Settings.default
        val results = input.rays.map (Algorithm.march (settings)(0.0, input.camera.frustumDepth, input.sdf, _)) // from near plane
        val minimumConeRatio = results.map { case Algorithm.March (_, _, Algorithm.March.Stats (m, _)) => m }
        val iterations = results.map { case Algorithm.March (_, _, Algorithm.March.Stats (_, i)) => i }
        DepthPass (input, Output (results.map (_.distance)), Stats (minimumConeRatio, iterations, settings.iterationLimit))
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class NormalsPass (input: NormalsPass.Input, output: NormalsPass.Output, stats: NormalsPass.Stats) extends Pass {
      lazy val (width, height) = (input.depthPass.input.w, input.depthPass.input.h)
      def surfaceNormal (i: Int): Option[Vector] = output.surfaceNormals (i)
    }
    object NormalsPass {

      case class Input (depthPass: DepthPass) extends Pass.Input 
      case class Output (surfaceNormals: IndexedSeq[Option[Vector]]) extends Pass.Output 
      case class Stats () extends Pass.Stats 

      // To run a normals pass a depth pass is a prerequisite, all required input information can be gathered from the inputs and outputs of the depth pass. 
      def apply (depthPass: DepthPass): NormalsPass = time ("normals-pass") { Input (depthPass) |> process }

      def process (input: Input) = {
        val results: IndexedSeq[Option[Vector]] = (0 until input.depthPass.height * input.depthPass.width).map { i =>
          input.depthPass.surfacePosition (i).map (Algorithm.estimateNormal (_, input.depthPass.input.sdf))
        }
        NormalsPass (input, Output (results), Stats ())
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class ShadowPass (input: ShadowPass.Input, output: ShadowPass.Output, stats: ShadowPass.Stats) extends Pass {
      lazy val (width, height) = (input.depthPass.input.w, input.depthPass.input.h)
      def shadowValue (i: Int): Double = output.shadowmap (i)
    }
    object ShadowPass {
      case class Input (depthPass: DepthPass, lighting: List[Light]) extends Pass.Input 
      case class Output (shadowmap: IndexedSeq[Double]) extends Pass.Output 
      case class Stats (minimumConeRatio: IndexedSeq[Double], iterations: IndexedSeq[Int], iterationLimit: Int) extends Pass.Stats  {
        lazy val minimumConeRatioLowerBound = minimumConeRatio.min
        lazy val minimumConeRatioUpperBound = minimumConeRatio.max
        lazy val averageIterations = iterations.sum.toDouble / iterations.size.toDouble
      }

      def apply (depthPass: DepthPass, lighting: List[Light]): ShadowPass = time ("shadow-pass") { Input (depthPass, lighting) |> process }

      def process (input: Input) = {
        val settings = Algorithm.March.Settings.default
        case class Result (shadow: Double, minimumConeRatio: Double, iterations: Int)
        val results: IndexedSeq[Result] = (0 until input.depthPass.height * input.depthPass.width).map { i =>
          val shadowCastingLights = input.lighting.collect { case l if l.castsShadows => l }
          input.depthPass.surfacePosition (i) match {
            case None => Result (1.0, 1.0, 0)
            case Some (p) => shadowCastingLights.foldLeft (Result (1.0, 1.0, 0)) { (a, l) =>
              val surfaceToLightRay = Ray (p,  (l.position - p).normalise)
              val start = 10.0 * settings.tolerance
              val distanceToEnd = (l.position - p).length - start
              Algorithm.march (settings)(start, distanceToEnd, input.depthPass.input.sdf, surfaceToLightRay) match {
                  case Algorithm.March (Some (d), _, stats) =>
                    Result (0.0, 0.0, a.iterations + stats.iterations)
                  case Algorithm.March (None, _, stats) =>
                    val k = 2.0; val z = stats.minimumConeRatio * k // https://iquilezles.org/www/articles/rmshadows/rmshadows.htm
                    Result (a.shadow, Math.min (a.minimumConeRatio, clamp01 (z)), a.iterations + stats.iterations)
                  case _ => a }}}
        }
        ShadowPass (input, Output (results.map (_.shadow)), Stats (results.map (_.minimumConeRatio), results.map (_.iterations), settings.iterationLimit))
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class AmbientOcclusionPass (input: AmbientOcclusionPass.Input, output: AmbientOcclusionPass.Output, stats: AmbientOcclusionPass.Stats) extends Pass {
      lazy val (width, height) = (input.depthPass.input.w, input.depthPass.input.h)
      def occlusionValue (i: Int): Double = output.aomap (i)
    }
    object AmbientOcclusionPass {
      case class Input (depthPass: DepthPass, normalsPass: NormalsPass) extends Pass.Input 
      case class Output (aomap: IndexedSeq[Double]) extends Pass.Output 
      case class Stats () extends Pass.Stats 

      def apply (depthPass: DepthPass, normalsPass: NormalsPass): AmbientOcclusionPass = time ("ao-pass") { Input (depthPass, normalsPass) |> process }

      def process (input: Input) = {
        val results: IndexedSeq[Double] = (0 until input.depthPass.height * input.depthPass.width).map { i =>
          (input.depthPass.surfacePosition (i), input.normalsPass.surfaceNormal (i)) match {
            case (Some (p), Some (n)) =>
              val step = 0.01
              val lim = 0.1
              @scala.annotation.tailrec
              def calcAOR (a: Double, t: Double): Double = {
                if (t > lim) a
                else {
                  val samplePosition = p + (n * t)
                  val d = Math.abs (input.depthPass.input.sdf (samplePosition))
                  val a2 = Math.min (a, d / t)
                  val t2 = t + Math.max (d, step)
                  calcAOR (a2, t2)
                }
              }
              calcAOR (1.0, step)
            case _ => 1.0
          }
        }
        AmbientOcclusionPass (input, Output (results), Stats ())
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class LightingPass (input: LightingPass.Input, output: LightingPass.Output, stats: LightingPass.Stats) extends Pass {
      lazy val (width, height) = (input.depthPass.input.w, input.depthPass.input.h)
      def ambientValue (i: Int): Vector = output.ambient (i)
      def diffuseValue (i: Int): Vector = output.diffuse (i)
      def specularValue (i: Int): Vector = output.specular (i)
    }
    object LightingPass {
      case class Input (depthPass: DepthPass, normalsPass: NormalsPass, lighting: List[Light]) extends Pass.Input 
      case class Output (ambient: IndexedSeq[Vector], diffuse: IndexedSeq[Vector], specular: IndexedSeq[Vector]) extends Pass.Output 
      case class Stats () extends Pass.Stats 

      def apply (depthPass: DepthPass, normalsPass: NormalsPass, lighting: List[Light]): LightingPass = time ("lighting-pass") { Input (depthPass, normalsPass, lighting) |> process }

      def process (input: Input) = {
        val results: IndexedSeq[Algorithm.PhongReflection] = (0 until input.depthPass.height * input.depthPass.width).map { i =>
          (input.depthPass.surfacePosition (i), input.normalsPass.surfaceNormal (i)) match {
            case (Some (p), Some (n)) => Algorithm.phong (p, n, input.depthPass.input.camera.position, input.lighting)
            case _ => Algorithm.PhongReflection (Vector.zero, Vector.zero, Vector.zero)
          }
        }
        LightingPass (input, Output (results.map (_.ambient), results.map (_.diffuse), results.map (_.specular)), Stats ())
      }
    }
  }

  // Compositor ~ This just converts data from render pass buffers into human viewable images.
  //------------------------------------------------------------------------------------------------------------------//
  object Compositor { import Renderer._

    private[this] def process[T <: Pass] (p: T)(f: (Int, Int) => Colour): Image = {
      val d = (0 until p.height).flatMap { j => (0 until p.width).map { i => f (i, j) } }
      Image (p.width, p.height, d)
    }

    def depth (p: DepthPass) = process (p) { (x: Int, y: Int) => p.output.depths (x + y * p.width) match {
      case None => Colour.white
      case Some (d) => Colour.fromRange (d, p.output.depthsLowerBound, p.output.depthsUpperBound) }
    }

    def depthSteps (p: DepthPass) = process (p) { (x: Int, y: Int) =>
      Colour (0, 0, (p.stats.iterations (x + y * p.width).toDouble / p.stats.iterationLimit * 255.0).toChar)
    }

    def depthStepsPretty (p: DepthPass) = process (p) { (x: Int, y: Int) => p.stats.iterations (x + y * p.width) match {
      case x if x == p.stats.iterationLimit => Colour.white
      case x => Colour (0, 0, (clamp01 (x.toDouble / p.stats.averageIterations / 2.0) * 255.0).toChar) }
    }

    def depthCone (p: DepthPass) = process (p) { (x: Int, y: Int) =>
      Colour.fromRange (p.stats.minimumConeRatio (x + y * p.width), p.stats.minimumConeRatioLowerBound, p.stats.minimumConeRatioUpperBound)
    }

    def albedo (p: DepthPass) = process (p) { (x: Int, y: Int) => 
      p.albedo (x, y) |> Colour.fromVector01
    }

    def normals (p: NormalsPass) = process (p) { (x: Int, y: Int) => p.surfaceNormal (x + y * p.width) match {
      case None => Colour.black
      case Some (n) => n |> Colour.fromNormal }
    }

    def shadow (p: ShadowPass) = process (p) { (x: Int, y: Int) =>
      val v = p.shadowValue (x + y * p.width)
      val c = (clamp01(v) * 255.0).toChar
      Colour (c, c, c)
    }

    def shadowSteps (p: ShadowPass) = process (p) { (x: Int, y: Int) =>
      Colour (0, 0, (p.stats.iterations (x + y * p.width).toDouble / p.stats.iterationLimit * 255.0).toChar)
    }

    def shadowStepsPretty (p: ShadowPass) = process (p) { (x: Int, y: Int) => p.stats.iterations (x + y * p.width) match {
      case x if x == p.stats.iterationLimit => Colour.white
      case x => Colour (0, 0, (clamp01 (x.toDouble / p.stats.averageIterations / 2.0) * 255.0).toChar) }
    }

    def shadowCone (p: ShadowPass) = process (p) { (x: Int, y: Int) =>
      Colour.fromRange (p.stats.minimumConeRatio (x + y * p.width), p.stats.minimumConeRatioLowerBound, p.stats.minimumConeRatioUpperBound)
    }

    def ao (p: AmbientOcclusionPass) = process (p) { (x: Int, y: Int) =>
      val v = p.occlusionValue (x + y * p.width)
      val c = (clamp01(v) * 255.0).toChar
      Colour (c, c, c)
    }

    def phongAmbient (p: LightingPass) = process (p) { (x: Int, y: Int) =>
      val res = p.ambientValue (x + y * p.width)
      Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)) |> Colour.fromVector01
    }

    def phongDiffuse (p: LightingPass) = process (p) { (x: Int, y: Int) =>
      val res = p.diffuseValue (x + y * p.width)
      Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)) |> Colour.fromVector01
    }

    def phongSpecular (p: LightingPass) = process (p) { (x: Int, y: Int) =>
      val res = p.specularValue (x + y * p.width)
      Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)) |> Colour.fromVector01
    }

    def aggregate (i: (DepthPass, ShadowPass, AmbientOcclusionPass, LightingPass)): Image = aggregate (i._1, i._2, i._3, i._4)
    def aggregate (depthPass: DepthPass, shadowPass: ShadowPass, aoPass: AmbientOcclusionPass, lightingPass: LightingPass) =
      Image (lightingPass.width, lightingPass.height, (0 until lightingPass.height * lightingPass.width).map { i =>
      val x =
        (lightingPass.ambientValue (i) /* * depthPass.albedo (i) */) +
        (lightingPass.diffuseValue (i) * depthPass.albedo (i)) +
        lightingPass.specularValue (i)
      val shadowFactor = 0.6
      val aoFactor = 0.8
      val shadowMultiplier = (1.0 - (1.0 - shadowPass.shadowValue (i)) * shadowFactor)
      val shadowPenmbraMultiplier = (1.0 - (1.0 - shadowPass.stats.minimumConeRatio (i)) * shadowFactor)
      val aoMultiplier = (1.0 - (1.0 - aoPass.occlusionValue (i)) * aoFactor)
      val res = x * /*shadowMultiplier * */ aoMultiplier * shadowPenmbraMultiplier
      Colour.fromVector01 (Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)))
    })
  }


  // Data
  //------------------------------------------------------------------------------------------------------------------//
  case class Colour (r: Char, g: Char, b: Char)
  object Colour {
    lazy val black = Colour (0.toChar, 0.toChar, 0.toChar)
    lazy val white = Colour (255.toChar, 255.toChar, 255.toChar)

    def fromNormal    (v: Vector) = Colour (((v.x + 1.0) / 2.0 * 255.0).toChar, ((v.y + 1.0) / 2.0 * 255.0).toChar, ((v.z + 1.0) / 2.0 * 255.0).toChar)
    def fromRange     (f: Double, min: Double, max: Double) = { val c = ((f - min) / (max - min) * 255.0).toChar; Colour (c, c, c) }
    def fromRangeInv  (f: Double, min: Double, max: Double) = { val c = (255.0 - ((f - min) / (max - min) * 255.0)).toChar; Colour (c, c, c) }
    def toVector01    (c: Colour) = Vector (c.r.toDouble / 255.0, c.g.toDouble / 255.0, c.b.toDouble / 255.0)
    def fromVector01  (v: Vector) = Colour ((v.x * 255.0).toChar, (v.y * 255.0).toChar, (v.z * 255.0).toChar)
  }

  //------------------------------------------------------------------------------------------------------------------//
  // Camera `fov` is the vertical field of view; horizontal FOV varies depending on the viewport's aspect ratio
  case class Camera (position: Vector, orientation: Quaternion, fov: Double, aspect: Double, near: Double, far: Double) {

    lazy val frustumDepth = far - near

    // Given a location anywhere on the near bounding surface of the camera's view frustum, returns the associated ray.
    def ray (x: Double, y: Double): Ray = { // co-ordinates range between -1.0 and +1.0 for each axis.
      val xx = (x + 1.0) / 2.0
      val yy = (y + 1.0) / 2.0
      val sh = Math.sin (fov / 2.0)
      val nnw = Vector ( near * sh * aspect,  near * sh, near)
      val nse = Vector (-near * sh * aspect, -near * sh, near)
      val fnw = Vector (  far * sh * aspect,   far * sh, far)
      val fsw = Vector (- far * sh * aspect, - far * sh, far)
      val f = Vector (xx, yy, 0.0)
      val p = position + Vector.lerp (nnw, nse, f).transform (orientation)
      val d = Vector.lerp (fnw - nnw, fsw - nse, f).transform (orientation).normalise
      Ray (p, d)
    }

    // Create a ray for each pixel in screen space starting on the camera's near bounding surface and directed at
    // corresponding part of the camera's far bounding surface.
    // TODO: Currently the near and far bounding surfaces are curved, i.e. points on the surface are a always the
    // same distance to the camera position.  This may need to be changed as it seems to be causing a fish eye effect.
    def pixelRays (w: Int, h: Int): IndexedSeq[Ray] = (0 until h).flatMap { j => (0 until w).map { i =>
      val fx = 2.0 - (2.0 * (i.toDouble + 0.5) / w.toDouble) - 1.0 // x axis: -1.0 to 1.0
      val fy = 2.0 - (2.0 * (j.toDouble + 0.5) / h.toDouble) - 1.0 // y axis: -1.0 to 1.0
      ray (fx, fy)
    }}
  }

  //------------------------------------------------------------------------------------------------------------------//
  case class Image (width: Int, height: Int, pixels: IndexedSeq[Colour]) {
    def get (x: Int, y: Int): Colour = pixels (x + y * width)
  }
  object Image {
    def create (w: Int, h: Int)(r: IndexedSeq[Colour]): Image = Image (w, h, r)
    def writePNG (path: String)(image: Image): Unit = {
      import javax.imageio.ImageIO, java.awt.image.BufferedImage, java.io.File
      val buffer = new BufferedImage (image.width, image.height, BufferedImage.TYPE_INT_RGB)
       (0 until image.height)
        .flatMap { y => (0 until image.width).map { x => (x, y) } }
        .foreach { case (x, y) =>
          val c = image.get (x, y)
          val value = c.r.toInt << 16 | c.g.toInt << 8 | c.b.toInt
          buffer.setRGB (x, y, value)
        }
      val file = new File (path)
      file.getParentFile().mkdirs();
      ImageIO.write (buffer, "png", file)
    }
  }

  // Maths
  //------------------------------------------------------------------------------------------------------------------//
  case class Quaternion (i: Double, j: Double, k: Double, u: Double)
  object Quaternion {
    lazy val identity = Quaternion (0, 0, 0, 1)

    def fromYawPitchRoll (yaw: Double, pitch: Double, roll: Double): Quaternion = { // Right handed co-ordinate system.
      val (y, p, r) = (yaw * 0.5, pitch * 0.5, roll * 0.5)
      val (sy, cy) = (y |> Math.sin, y |> Math.cos)
      val (sp, cp) = (p |> Math.sin, p |> Math.cos)
      val (sr, cr) = (r |> Math.sin, r |> Math.cos)
      Quaternion (
        (cy * sp * cr) + (sy * cp * sr), (sy * cp * cr) - (cy * sp * sr),
        (cy * cp * sr) - (sy * sp * cr), (cy * cp * cr) + (sy * sp * sr))
    }
  }

  //------------------------------------------------------------------------------------------------------------------//
  case class Vector (x: Double, y: Double, z: Double) {
    lazy val isZero     : Boolean = Math.abs (x) < EPSILON && Math.abs (y) < EPSILON && Math.abs (z) < EPSILON
    lazy val length     : Double  = isZero match { case false => (x * x + y * y + z * z) |> Math.sqrt; case true => 0.0 }
    lazy val normalise  : Vector  = this.length match { case 0.0 => Vector.zero; case l => Vector (x / l, y / l, z / l) }
    lazy val abs        : Vector  = Vector (Math.abs (x), Math.abs (y), Math.abs (z))

    def max (v: Double) : Vector  = Vector (Math.max (this.x, v), Math.max (this.y, v), Math.max (this.z, v))
    def min (v: Double) : Vector  = Vector (Math.min (this.x, v), Math.min (this.y, v), Math.min (this.z, v))
    def unary_+ ()      : Vector  = this
    def unary_- ()      : Vector  = Vector (-this.x, -this.y, -this.z)
    def + (x: Vector)   : Vector  = Vector (this.x + x.x, this.y + x.y, this.z + x.z)
    def - (x: Vector)   : Vector  = Vector (this.x - x.x, this.y - x.y, this.z - x.z)
    def * (x: Vector)   : Vector  = Vector (this.x * x.x, this.y * x.y, this.z * x.z)
    def * (m: Double)   : Vector  = Vector (this.x * m, this.y * m, this.z * m)
    def / (x: Vector)   : Vector  = Vector (this.x / x.x, this.y / x.y, this.z / x.z)
    def / (m: Double)   : Vector  = Vector (this.x / m, this.y / m, this.z / m)

    def transform (q: Quaternion): Vector = {
      val (ii, jj, kk) = (q.i * q.i, q.j * q.j, q.k * q.k)
      val (ui, uj, uk) = (q.u * q.i, q.u * q.j, q.u * q.k)
      val (ij, ik, jk) = (q.i * q.j, q.i * q.k, q.j * q.k)
      Vector (
        + this.x - (2.0 * this.x * (jj + kk)) + (2.0 * this.y * (ij - uk)) + (2.0 * this.z * (ik + uj)),
        + this.y + (2.0 * this.x * (ij + uk)) - (2.0 * this.y * (ii + kk)) + (2.0 * this.z * (jk - ui)),
        + this.z + (2.0 * this.x * (ik - uj)) + (2.0 * this.y * (jk + ui)) - (2.0 * this.z * (ii + jj)))
    }
  }
  object Vector {
    lazy val (zero, one)            = (Vector (0, 0, 0), Vector (1, 1, 1))
    lazy val (unitX, unitY, unitZ)  = (Vector (1, 0, 0), Vector (0, 1, 0), Vector (0, 0, 1))
    
    def reflect (v: Vector, n: Vector)            : Vector = { val f = 2.0 * (v.x * n.x + v.y * n.y + v.z * n.z); Vector (v.x - f * n.x, v.y - f * n.y, v.z - f * n.z) }
    def dot     (a: Vector, b: Vector)            : Double = (a.x * b.x) + (a.y * b.y) + (a.z * b.z)
    def cross   (a: Vector, b: Vector)            : Vector = Vector ((a.y * b.z) - (a.z * b.y), (a.z * b.x) - (a.x * b.z), (a.x * b.y) - (a.y * b.x))
    def lerp    (a: Vector, b: Vector, f: Vector) : Vector = Vector (a.x + (b.x - a.x) * f.x, a.y + (b.y - a.y) * f.y, a.z + (b.z - a.z) * f.z)
  }
}

