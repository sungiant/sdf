object Program {

  // Extensions
  //------------------------------------------------------------------------------------------------------------------//
  implicit class I[A](val self: A) { def |>[B](f: A => B): B = f (self) }

  // Basic Type Definitions
  //------------------------------------------------------------------------------------------------------------------//
  case class Ray   (position: Vector, direction: Vector)
  case class DirectionalLight (position: Vector, colour: Colour, castsShadows: Boolean)

  case class Material (albedo: Colour, shininess: Double)
  object Material {
    type ID = Int
    type Lookup = (Material.ID) => Material
    lazy val default = Material (Colour.magenta, 0)
  }

  // Defines the signature of a signed distance function.  The absolute value of the result indicates the distance to
  // the surface, the sign of the result indicates whether the position is inside or outside of the surface, negative
  // indicating inside.
  type SDF = Vector => Double
  type MaterialSDF = Vector => (Double, Material.ID) // how far to the nearest object, and what material is it?

  // Configuration 
  //------------------------------------------------------------------------------------------------------------------//
  val EPSILON = 0.0001

  // Utility functions
  //------------------------------------------------------------------------------------------------------------------//
  def clamp01 (x: Double) = clamp (0, 1)(x)
  def clamp (low: Double, high: Double)(x: Double) = if (x < low) low else if (x > high) high else x
  def clampInt (low: Int, high: Int)(x: Int) = if (x < low) low else if (x > high) high else x
  def sign (x: Double) = if (x > 0.0) 1.0 else { if (x < 0.0) -1.0 else 0.0 }
  def time[R](id: String)(block: => R): R = { // TODO: Not FP, urgh, do this instrumentation properly later.
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
    val camera            = Scene.createCamera (y = 4, zx = 6, fov = 57.5, aspect = w.toDouble / h.toDouble, near = 0.1, far = 100.0) 
    val lighting          = Scene.createLighting ()
    val jobs              = (Scene.createEdits () :: Nil/*Scene.createDocsEditsList*/).map (_ |> CSG.evaluate)
    val materials         = Scene.createMaterialLibrary ()
    val materialLookup    = (id: Material.ID) => materials.getOrElse (id, Material.default)

    jobs.zipWithIndex.flatMap { case (sdf, i) =>
      // Render pipeline
      val depthPass         = Renderer.DepthPass (w, h, sdf, camera, materialLookup)
      val normalsPass       = Renderer.NormalsPass (depthPass)
      val shadowPass        = Renderer.ShadowPass (depthPass, lighting)
      val aoPass            = Renderer.AmbientOcclusionPass (depthPass, normalsPass)
      val lightingPass      = Renderer.LightingPass (depthPass, normalsPass, lighting)
      val bundledPasses     = (depthPass.view, shadowPass.view, aoPass.view, lightingPass.view)

      val jobID = if (i == 0) "" else s"docs/j${i}-"

      // Visualise work done
      (s"${jobID}render-00-composite",              bundledPasses      |> Compositor.aggregate) ::
      (s"${jobID}render-01-albedo",                 depthPass.view     |> Compositor.albedo) ::
      (s"${jobID}render-02-depth",                  depthPass.view     |> Compositor.depth) ::
      (s"${jobID}render-03-depth-cone",             depthPass.view     |> Compositor.depthCone) ::
      (s"${jobID}render-04-depth-steps",            depthPass.view     |> Compositor.depthStepsPretty) ::
      (s"${jobID}render-05-normals",                normalsPass.view   |> Compositor.normals) ::
      (s"${jobID}render-06-shadow-hard",            shadowPass.view    |> Compositor.hardShadows) ::
      (s"${jobID}render-07-shadow-soft",            shadowPass.view    |> Compositor.softShadows) ::
      (s"${jobID}render-08-shadow-steps",           shadowPass.view    |> Compositor.shadowStepsPretty) ::
      (s"${jobID}render-09-ambient-occlusion",      aoPass.view        |> Compositor.ao) ::
      (s"${jobID}render-10-phong-ambient",          lightingPass.view  |> Compositor.phongAmbient) ::
      (s"${jobID}render-11-phong-diffuse",          lightingPass.view  |> Compositor.phongDiffuse) ::
      (s"${jobID}render-12-phong-specular",         lightingPass.view  |> Compositor.phongSpecular) :: Nil

    }
  }

  // Scene ~ All of the data defining the scene.
  //------------------------------------------------------------------------------------------------------------------//
  object Scene {

    def createCamera (y: Double, zx: Double, fov: Double, aspect: Double, near: Double, far: Double): Camera = Camera (
      Vector (zx, y, zx),
      Quaternion.fromYawPitchRoll (5.0 * math.Pi / 4.0, (math.Pi / 2.0) - ((Math.sqrt (zx * zx * 2.0) / y) |> Math.atan), 0.0),
      fov, aspect, near, far)

    def createLighting (): List[DirectionalLight] = // Todo: Add support for point and spot lighting.
      DirectionalLight (Vector (-1, 1.5, 5), Colour (128, 192, 192), true) ::
      DirectionalLight (Vector (6, 8, 2), Colour (192, 128, 128), true) ::
      Nil

    def createEdits (): CSG.Tree = CSG.Tree (ground, boxes, shape, debris)
    
    def createMaterialLibrary () = Map [Material.ID, Material](
      0 -> Material.default,
      1 -> Material (Colour.warmBlack, 1),
      2 -> Material (Colour.coolBlack, 10.0),
      3 -> Material (Colour.jade, 2.0),
      4 -> Material (Colour.persimmon, 30.0),
      5 -> Material (Colour.persimmon, 10.0),
      6 -> Material (Colour.vermillion, 20.0))

    def createDocsEditsList (): List[CSG.Tree] = shape_a :: shape_b :: shape_c :: shape_d :: shape_e :: shape_ab :: shape_de :: shape_cde :: shape :: Nil

    // Clearly seperated for docs CSG example
    private lazy val shape_a = CSG.Tree (SDF.cube (Vector.zero, 1.9) _, 3)
    private lazy val shape_b = CSG.Tree (SDF.sphere (Vector.zero, 1.2) _, 3)
    private lazy val shape_c0 = CSG.Tree (SDF.sphere (Vector.unitX, 0.5) _, 3)
    private lazy val shape_d0 = CSG.Tree (SDF.sphere (Vector.unitY, 0.5) _, 3)
    private lazy val shape_e0 = CSG.Tree (SDF.sphere (Vector.unitZ, 0.5) _, 3)

    private lazy val shape_c = CSG.Tree (SDF.cuboid (Vector.zero, Vector (2.2, 0.7, 0.7)) _, 3)
    private lazy val shape_d = CSG.Tree (SDF.cuboid (Vector.zero, Vector (0.7, 2.2, 0.7)) _, 3)
    private lazy val shape_e = CSG.Tree (SDF.cuboid (Vector.zero, Vector (0.7, 0.7, 2.2)) _, 3)

    //private lazy val shape_c = CSG.Tree (SDF.cylinder (Vector.zero, 0.5, Vector (-2.0, 0.0, 0.0), Vector (2.0, 0.0, 0.0)) _, 3)
    //private lazy val shape_d = CSG.Tree (SDF.cylinder (Vector.zero, 0.5, Vector (0.0, -2.0, 0.0), Vector (0.0, 2.0, 0.0)) _, 3)
    //private lazy val shape_e = CSG.Tree (SDF.cylinder (Vector.zero, 0.5, Vector (0.0, 0.0, -2.0), Vector (0.0, 0.0, 2.0)) _, 3)

    private lazy val shape_ab = CSG.Tree (CSG.Op.Intersection, shape_a, shape_b)
    private lazy val shape_de = CSG.Tree (CSG.Op.Union, shape_d, shape_e)
    private lazy val shape_cde = CSG.Tree (CSG.Op.Union, shape_c, shape_de)
    private lazy val shape: CSG.Tree = CSG.Tree (CSG.Op.Difference, shape_ab, shape_cde)

    private lazy val debris: CSG.Tree =
      CSG.Tree (
        CSG.Tree (SDF.sphere (Vector (-1.35, -0.6, 1.65), 0.2) _, 4),
        CSG.Tree (SDF.sphere (Vector (-1.45, -0.6, 2.35), 0.2) _, 4),
        CSG.Tree (SDF.sphere (Vector (-1.75, -0.6, 1.09), 0.2) _, 4),
        CSG.Tree (SDF.sphere (Vector ( -2.70, -0.6, 2.05), 0.2) _, 5),
        CSG.Tree (SDF.sphere (Vector ( -2.00, -0.6, 3.25), 0.2) _, 5),
        CSG.Tree (SDF.sphere (Vector ( -3.20, -0.6, 3.60), 0.2) _, 5),
        CSG.Tree (SDF.sphere (Vector ( 2.50, -0.6, 0.40), 0.2) _, 6),
        CSG.Tree (SDF.sphere (Vector ( 2.50, -0.6, 1.40), 0.2) _, 6),
        CSG.Tree (SDF.sphere (Vector ( 1.50, -0.6, 1.40), 0.2) _, 6),
        CSG.Tree (SDF.sphere (Vector ( 2.50, -0.6, -2.85), 0.2) _, 2),
        CSG.Tree (SDF.sphere (Vector ( 1.70, -0.6, -2.15), 0.2) _, 2),
        CSG.Tree (SDF.sphere (Vector ( 2.30, -0.6, -3.55), 0.2) _, 2))

    private lazy val ground: CSG.Tree = CSG.Tree (SDF.cube (Vector (0.0, -5.8, 0.0), 10.0) _, 1)

    private lazy val boxes: CSG.Tree =
      CSG.Tree (
        CSG.Tree (SDF.cube (Vector (-3.5, 0.7, -3.5), 3.0) _, 1),
        CSG.Tree (SDF.cuboid (Vector (2.0, 0.2, -4.5), Vector (1.0, 2.4, 1.0)) _, 1),
        CSG.Tree (SDF.cube (Vector (-4.0, 0.2, 0.0), 2.0) _, 1),
        CSG.Tree (SDF.cuboid (Vector (-4.5, 0.2, 2.0), Vector (1.0, 2.8, 1.0)) _, 1))
  }

  // Constructive solid geometry logic.
  //------------------------------------------------------------------------------------------------------------------//
  object CSG {
    // Enumeration of constructive solid geometry ops
    trait Op; object Op { object Union extends Op; object Intersection extends Op; object Difference extends Op }

    type Tree = Either[Tree.Node, Tree.Leaf]
    object Tree {
      case class Node (operation: Op, left: Tree, right: Tree)
      case class Leaf (sdf: SDF, material: Material.ID)
      def apply (operation: Op, left: Tree, right: Tree): Tree = Left (Node (operation, left, right))
      def apply (sdf: SDF): Tree = Right (Leaf (sdf, 0))
      def apply (sdf: SDF, material: Material.ID): Tree = Right (Leaf (sdf, material))
      def apply (trees: Tree*): Tree = trees.reduceLeft ((x, y) => CSG.Tree (CSG.Op.Union, x, y))
    }

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
  }

  // Signed distance fields.
  //------------------------------------------------------------------------------------------------------------------//
  object SDF {
    // Signed distance function for a unit sphere (radius = 1). https://en.wikipedia.org/wiki/Unit_sphere
    def sphere (offset: Vector, radius: Double)(position: Vector): Double = (position - offset).length - radius

    // Signed distance function for a unit cube (h, w, d = 1). https://en.wikipedia.org/wiki/Unit_cube
    def cube (offset: Vector, size: Double)(position: Vector): Double = {
      val d: Vector = (position - offset).abs - (Vector (size, size, size) / 2.0)
      d.max (0.0).length + Math.min (Math.max (d.x, Math.max (d.y, d.z)), 0.0)
    }

    def cuboid (offset: Vector, size: Vector)(position: Vector): Double = {
      val q: Vector = (position - offset).abs - (size / 2.0)
      q.max (0.0).length + Math.min (Math.max (q.x, Math.max (q.y, q.z)), 0.0)
    }
  }

  // Algorithms.
  //------------------------------------------------------------------------------------------------------------------//
  object Algorithm {

    // distance: distance to surface intersection (if intersection was found) from start of ray.
    case class March (distance: Option[(Double, Material.ID)], settings: March.Settings, stats: March.Stats)

    object March {
      case class Settings (iterationLimit: Int, minimumStep: Double, tolerance: Double)
      object Settings { lazy val default = Settings (256, 0.001, 0.0001) }

      // minimumConeRatio: the minimum result of the ratio of all individual sdf results along the march over the distance covered at that point.
      // iterations: number of iterations performed
      case class Stats (minimumConeRatio: Double, iterations: Int)
    }

    // Given a ray and an SDF recursively evaluates the SDF until an intersection is either found or the limit of iterations is reached.
    // Returns the distance along the ray to the first intersection.
    def march (settings: March.Settings)(start: Double, end: Double, sdf: MaterialSDF, ray: Ray): March = {
      @scala.annotation.tailrec def step (distanceCovered: Double, minConeRatio: Double, stepCount: Int, lastMaterial: Material.ID): March = stepCount match {
        case currentStep if currentStep == settings.iterationLimit =>
          // We've run out of marching steps and not found a sausage.  Perhaps we should assume we have hit something though,
          // as normally when we run out of iteration steps we are close to something.
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
                val newMinConeRatio = if (distanceCovered == 0.0) Double.MaxValue else Math.min (minConeRatio, nextStepSize / distanceCovered)
                step (nextDistanceCovered, newMinConeRatio, stepCount + 1, m)
            }
          }
      }
      step (start, minConeRatio = Double.MaxValue, stepCount = 0, 0)
    }

    def estimateNormal (pos: Vector, sdf: SDF) = { val NORM_SAMPLE = 0.001; Vector (
      sdf (Vector (pos.x + NORM_SAMPLE, pos.y, pos.z)) - sdf (Vector (pos.x - NORM_SAMPLE, pos.y, pos.z)),
      sdf (Vector (pos.x, pos.y + NORM_SAMPLE, pos.z)) - sdf (Vector (pos.x, pos.y - NORM_SAMPLE, pos.z)),
      sdf (Vector (pos.x, pos.y, pos.z + NORM_SAMPLE)) - sdf (Vector (pos.x, pos.y, pos.z - NORM_SAMPLE))).normalise }

    case class PhongReflection (ambient: Vector, diffuse: Vector, specular: Vector)
    def phong (surfacePosition: Vector, surfaceNormal: Vector, eyePosition: Vector, lights: List[DirectionalLight],
      material: Material): PhongReflection = {
      val ambientColour = Vector (0.2, 0.2, 0.2)  // HARDCODED
      val specularColour = Vector.one             // HARDCODED
      val result = lights.foldLeft ((Vector.zero, Vector.zero)) { (a, i) =>
        val lightColour = i.colour.toVector01
        val N = surfaceNormal
        val L = (i.position - surfacePosition).normalise
        val V = (eyePosition - surfacePosition).normalise
        val R = Vector.reflect (-L, N).normalise
        val dotLN = Vector.dot (L, N)
        val dotRV = Vector.dot (R, V)
        val contribution =
          if (dotLN < 0.0) (Vector.zero, Vector.zero)
          else if (dotRV < 0.0 || material.shininess <= 0.0) (lightColour * dotLN, Vector.zero)
          else (lightColour * dotLN, lightColour * specularColour * Math.pow (dotRV, material.shininess))
        // Currently the contribution each light source is simply additive to the result.
        (a._1 + contribution._1, a._2 + contribution._2)
      }
      PhongReflection (ambientColour, result._1, result._2)
    }
  }

  // Renderer
  //------------------------------------------------------------------------------------------------------------------//
  object Renderer {

    // Define commonality in approach for render passes.
    trait Pass { def input: Pass.Input; def output: Pass.Output; def stats: Pass.Stats; def view: Pass.View }
    object Pass {
      trait Input                                    // Raw input data for a render pass.
      trait Output                                   // Raw output data for a render pass.
      trait Stats                                    // Raw processing stats for a render pass.
      trait View { def width: Int; def height: Int } // Practical abstaction over raw render pass data.
    }


    //----------------------------------------------------------------------------------------------------------------//
    // This pass evaluates the our scene SDF by sphere tracing a ray from the camera through each pixel of the view.
    // Given that our SDF functions have been modified to also return material ids for our CSG the output of this
    // pass gives us a lot of data to work with:
    // * the depth at a given pixel
    // * the world space position of a given pixel (which is possible to work out from the depth as the ray details are know)
    // * the material associated with given pixel
    //----------------------------------------------------------------------------------------------------------------//

    case class DepthPass (input: DepthPass.Input, output: DepthPass.Output, stats: DepthPass.Stats) extends Pass {
      lazy val view = DepthPass.View (this)
      def surfacePosition (i: Int) = output.depths (i).map (input.rays (i).position + input.rays (i).direction * _)
      def material        (i: Int): Material = output.materials (i) match { case None => Material.default; case Some (m) => input.materialLookup (m) }
    }
    object DepthPass {
      case class Input  (w: Int, h: Int, sdf: MaterialSDF, camera: Camera, materialLookup: Material.Lookup) extends Pass.Input { lazy val rays = camera.pixelRays (w, h) }
      case class Output (depths: IndexedSeq[Option[Double]], materials: IndexedSeq[Option[Material.ID]]) extends Pass.Output
      case class Stats  (minimumConeRatio: IndexedSeq[Double], iterations: IndexedSeq[Int], iterationLimit: Int) extends Pass.Stats
      case class View   (private val raw: DepthPass) extends Pass.View {
        lazy val (width, height)                = (raw.input.w, raw.input.h)
        lazy val depthsLowerBound               = raw.output.depths.collect { case Some (d) => d }.min
        lazy val depthsUpperBound               = raw.output.depths.collect { case Some (d) => d }.max
        lazy val minimumConeRatioLowerBound     = raw.stats.minimumConeRatio.min
        lazy val minimumConeRatioUpperBound     = raw.stats.minimumConeRatio.max
        lazy val averageIterations              = raw.stats.iterations.sum.toDouble / raw.stats.iterations.size.toDouble
        lazy val iterationLimit                 = raw.stats.iterationLimit

        def depth             (x: Int, y: Int)  = raw.output.depths (x + y * width)
        def surfacePosition   (x: Int, y: Int)  = { val i = x + y * width; raw.output.depths (i).map (raw.input.rays (i).position + raw.input.rays (i).direction * _) }
        def iterations        (x: Int, y: Int)  = raw.stats.iterations (x + y * width)
        def minimumConeRatio  (x: Int, y: Int)  = raw.stats.minimumConeRatio (x + y * width)
        def material          (x: Int, y: Int)  = raw.output.materials (x + y * width) match {
          case None => Material.default
          case Some (m) => raw.input.materialLookup (m)
        }
      }
      // To run a depth pass you need the scene definition given by an SDF, you need a camera to define what is being looked at and you need a pixel size for the rastered output.
      def apply (w: Int, h: Int, sdf: MaterialSDF, camera: Camera, materialLookup: Material.Lookup): DepthPass = time ("depth-pass") { DepthPass.Input (w, h, sdf, camera, materialLookup) |> process }

      // The depth pass processing is straightforward, it essentially just uses our sphere tracing algorithm to produce its outputs.
      def process (input: Input): DepthPass = {
        val settings = Algorithm.March.Settings.default
        val results = input.rays.map (Algorithm.march (settings)(0.0, input.camera.frustumDepth, input.sdf, _)) // from near plane
        val minimumConeRatio = results.map { case Algorithm.March (_, _, Algorithm.March.Stats (m, _)) => m }
        val iterations = results.map { case Algorithm.March (_, _, Algorithm.March.Stats (_, i)) => i }
        DepthPass (input, Output (results.map (_.distance.map (_._1)), results.map (_.distance.map (_._2))), Stats (minimumConeRatio, iterations, settings.iterationLimit))
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class NormalsPass (input: NormalsPass.Input, output: NormalsPass.Output, stats: NormalsPass.Stats) extends Pass { lazy val view = NormalsPass.View (this) }
    object NormalsPass {

      case class Input  (depthPass: DepthPass) extends Pass.Input 
      case class Output (surfaceNormals: IndexedSeq[Option[Vector]]) extends Pass.Output 
      case class Stats  () extends Pass.Stats
      case class View   (private val raw: NormalsPass) extends Pass.View {
        lazy val (width, height)                = (raw.input.depthPass.input.w, raw.input.depthPass.input.h)
        def surfaceNormal (x: Int, y: Int)      = raw.output.surfaceNormals (x + y * width)
      }

      // To run a normals pass a depth pass is a prerequisite, all required input information can be gathered from the inputs and outputs of the depth pass. 
      def apply (depthPass: DepthPass): NormalsPass = time ("normals-pass") { Input (depthPass) |> process }

      def process (input: Input) = {
        val results = (0 until input.depthPass.input.w * input.depthPass.input.h).map { i =>
          input.depthPass
            .surfacePosition (i)
            .map (Algorithm.estimateNormal (_, (v: Vector) => input.depthPass.input.sdf (v)._1))
        }
        NormalsPass (input, Output (results), Stats ())
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class ShadowPass (input: ShadowPass.Input, output: ShadowPass.Output, stats: ShadowPass.Stats) extends Pass { lazy val view = ShadowPass.View (this) }
    object ShadowPass {
      case class Input  (depthPass: DepthPass, lighting: List[DirectionalLight]) extends Pass.Input 
      case class Output (hardShadows: IndexedSeq[Double], softShadows: IndexedSeq[Double]) extends Pass.Output 
      case class Stats  (minimumConeRatio: IndexedSeq[Double], iterations: IndexedSeq[Int], iterationLimit: Int) extends Pass.Stats
      case class View   (private val raw: ShadowPass) extends Pass.View {
        lazy val (width, height)                = (raw.input.depthPass.input.w, raw.input.depthPass.input.h)
        lazy val minimumConeRatioLowerBound     = raw.stats.minimumConeRatio.min
        lazy val minimumConeRatioUpperBound     = raw.stats.minimumConeRatio.max
        lazy val averageIterations              = raw.stats.iterations.sum.toDouble / raw.stats.iterations.size.toDouble
        lazy val iterationLimit                 = raw.stats.iterationLimit
        def softShadowValue   (x: Int, y: Int)  = raw.output.softShadows (x + y * width)
        def hardShadowValue   (x: Int, y: Int)  = raw.output.hardShadows (x + y * width)
        def iterations        (x: Int, y: Int)  = raw.stats.iterations (x + y * width)
        def minimumConeRatio  (x: Int, y: Int)  = raw.stats.minimumConeRatio (x + y * width)
      }

      def apply (depthPass: DepthPass, lighting: List[DirectionalLight]): ShadowPass = time ("shadow-pass") { Input (depthPass, lighting) |> process }

      def process (input: Input) = {
        // In this context 0.0 means no shadows and 1.0 means shadows.
        val settings = Algorithm.March.Settings.default
        val shadowCastingLights = input.lighting.collect { case l if l.castsShadows => l }
        lazy val insensityOfAllShadowCastingLights = shadowCastingLights.map { l => l.colour.toVector01.length }.sum
        case class R (hardShadow: Boolean, softShadow: Double, iterations: Int)
        val intermediateResults: List[(Double, IndexedSeq[R])] = shadowCastingLights.map { light =>
          val lightIntensity = light.colour.toVector01.length
          val results = (0 until input.depthPass.input.h * input.depthPass.input.w).map { i =>
            input.depthPass.surfacePosition (i) match {
              case None => R (false, 0.0, 0)
              case Some (p) => {
                val surfaceToLightRay = Ray (p,  (light.position - p).normalise)
                val start = 10.0 * settings.tolerance
                val distanceToEnd = (light.position - p).length - start
                Algorithm.march (settings)(start, distanceToEnd, input.depthPass.input.sdf, surfaceToLightRay) match {
                  case Algorithm.March (Some (_), _, stats) =>
                    R (true, 1.0, stats.iterations)
                  case Algorithm.March (None, _, stats) =>
                    val k = 2.0; val z = stats.minimumConeRatio * k // https://iquilezles.org/www/articles/rmshadows/rmshadows.htm
                    val softShadow = 1.0 - clamp01 (z)
                    R ( false, softShadow, stats.iterations)
                  case _ =>
                    R (false, 0.0, 0)
                }
              }
            }
          }
          (lightIntensity, results)
        }

        case class R2 (hardShadow: Double, softShadow: Double, iterations: Int)
        val start: IndexedSeq[R2] = List.fill (intermediateResults (0)._2.size)(R2 (0.0, 0.0, 0)).toIndexedSeq
        val results = intermediateResults.foldLeft (start) { case (a, (li, ri)) =>
          ri.zipWithIndex.map { case (r, idx) => R2 (
            a (idx).hardShadow + ((if (r.hardShadow) 1.0 else 0.0) * li / insensityOfAllShadowCastingLights),
            a (idx).softShadow + (r.softShadow * li / insensityOfAllShadowCastingLights),
            a (idx).iterations + r.iterations)
          }
        }
        ShadowPass (input, Output (
          results.map (x => x.hardShadow),
          results.map (_.softShadow)),
        Stats (results.map (_ => 0.0), results.map (_.iterations), settings.iterationLimit))
      }
    }

    //----------------------------------------------------------------------------------------------------------------//
    case class AmbientOcclusionPass (input: AmbientOcclusionPass.Input, output: AmbientOcclusionPass.Output, stats: AmbientOcclusionPass.Stats) extends Pass { lazy val view = AmbientOcclusionPass.View (this) }
    object AmbientOcclusionPass {
      case class Input  (depthPass: DepthPass, normalsPass: NormalsPass) extends Pass.Input 
      case class Output (aomap: IndexedSeq[Double]) extends Pass.Output 
      case class Stats  () extends Pass.Stats
      case class View   (private val raw: AmbientOcclusionPass) extends Pass.View {
        lazy val (width, height)                = (raw.input.depthPass.input.w, raw.input.depthPass.input.h)
        def occlusionValue (x: Int, y: Int)     = raw.output.aomap (x + y * width)
      }

      def apply (depthPass: DepthPass, normalsPass: NormalsPass): AmbientOcclusionPass = time ("ao-pass") { Input (depthPass, normalsPass) |> process }

      def process (input: Input) = { // todo: fire multiple rays instead of just one along the normal
        val results: IndexedSeq[Double] = (0 until input.depthPass.input.h * input.depthPass.input.w).map { i =>
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
        AmbientOcclusionPass (input, Output (results), Stats ())
      }
    }

    // Just basic Phong lighting with directional light sources for the time being.
    //----------------------------------------------------------------------------------------------------------------//
    case class LightingPass (input: LightingPass.Input, output: LightingPass.Output, stats: LightingPass.Stats) extends Pass { lazy val view = LightingPass.View (this) }
    object LightingPass {
      case class Input  (depthPass: DepthPass, normalsPass: NormalsPass, lighting: List[DirectionalLight]) extends Pass.Input 
      // Lighting data in this output structure is not clamped between 0.0 and 1.0, as each light additively contributes to the results,
      // all that is know is that components will be >= 0.
      case class Output (ambient: IndexedSeq[Vector], diffuse: IndexedSeq[Vector], specular: IndexedSeq[Vector]) extends Pass.Output 
      case class Stats  () extends Pass.Stats
      case class View   (private val raw: LightingPass) extends Pass.View {
        lazy val (width, height)                = (raw.input.depthPass.input.w, raw.input.depthPass.input.h)
        def ambientValue  (x: Int, y: Int)      = raw.output.ambient  (x + y * width)
        def diffuseValue  (x: Int, y: Int)      = raw.output.diffuse  (x + y * width)
        def specularValue (x: Int, y: Int)      = raw.output.specular (x + y * width)
      }

      def apply (depthPass: DepthPass, normalsPass: NormalsPass, lighting: List[DirectionalLight]): LightingPass = time ("lighting-pass") { Input (depthPass, normalsPass, lighting) |> process }

      def process (input: Input) = {
        val results = (0 until input.depthPass.input.h * input.depthPass.input.w).map { i =>
          val surfacePostion = input.depthPass.surfacePosition (i)
          val surfaceNormal  = input.normalsPass.output.surfaceNormals (i)
          surfacePostion.flatMap (p => surfaceNormal.map (n => (p, n))) match {
            case Some ((p, n)) => Algorithm.phong (p, n, input.depthPass.input.camera.position, input.lighting, input.depthPass.material (i))
            case _             => Algorithm.PhongReflection (Vector.zero, Vector.zero, Vector.zero)
          }
        }
        LightingPass (input, Output (results.map (_.ambient), results.map (_.diffuse), results.map (_.specular)), Stats ())
      }
    }
  }

  // Compositor ~ This just converts data from render pass buffers into human viewable images.
  //------------------------------------------------------------------------------------------------------------------//
  object Compositor { import Renderer._

    private[this] def process[T <: Pass.View] (p: T)(f: (Int, Int) => Colour): Image = {
      val d = (0 until p.height).flatMap { j => (0 until p.width).map { i => f (i, j) } }
      Image (p.width, p.height, d)
    }

    // SDF (depth) pass visualisations
    // -------------------------------
    def albedo (p: DepthPass.View) = { (x: Int, y: Int) => p.material (x, y).albedo } |> process (p)

    def depth (p: DepthPass.View) = { (x: Int, y: Int) =>
      p.depth (x, y) match {
        case None => Colour.white
        case Some (d) => Colour.fromRange (d, p.depthsLowerBound, p.depthsUpperBound)
      }
    } |> process (p)

    def depthSteps (p: DepthPass.View) = { (x: Int, y: Int) =>
      Colour (0, 0, (p.iterations (x, y).toDouble / p.iterationLimit * 255.0).toChar)
    } |> process (p)

    def depthStepsPretty (p: DepthPass.View) = { (x: Int, y: Int) =>
      p.iterations (x, y) match {
        case x if x == p.iterationLimit => Colour.white
        case x => Colour (0, 0, (clamp01 (x.toDouble / p.averageIterations / 2.0) * 255.0).toChar)
      }
    } |> process (p) 

    def depthCone (p: DepthPass.View) = { (x: Int, y: Int) =>
      Colour.fromRange (p.minimumConeRatio (x, y), p.minimumConeRatioLowerBound, p.minimumConeRatioUpperBound)
    } |> process (p) 

    // Geometric compositions
    // ----------------------
    def normals (p: NormalsPass.View) = { (x: Int, y: Int) =>
      p.surfaceNormal (x, y) match {
        case None => Colour.black
        case Some (n) => ((n + Vector.one) / 2.0) |> Colour.fromVector01
      }
    } |> process (p)

    // Shadow pass compositions
    // ------------------------
    def hardShadows (p: ShadowPass.View) = { (x: Int, y: Int) => Colour.from01 (1.0 - p.hardShadowValue (x, y)) } |> process (p)

    def softShadows (p: ShadowPass.View) = { (x: Int, y: Int) => Colour.from01 (1.0 - p.softShadowValue (x, y)) } |> process (p)

    def shadowSteps (p: ShadowPass.View) = { (x: Int, y: Int) =>
      Colour (0, 0, (p.iterations (x, y).toDouble / p.iterationLimit * 255.0).toChar)
    } |> process (p)

    def shadowStepsPretty (p: ShadowPass.View) = { (x: Int, y: Int) =>
      p.iterations (x, y) match {
        case x if x == p.iterationLimit => Colour.white
        case x => Colour (0, 0, (clamp01 (x.toDouble / p.averageIterations / 2.0) * 255.0).toChar)
      }
    } |> process (p)

    // Ambient Occlusion (SDF based)
    // -----------------------------
    def ao (p: AmbientOcclusionPass.View) = { (x: Int, y: Int) => Colour.from01 (p.occlusionValue (x, y)) } |> process (p)

    // Shadow pass compositions
    // ------------------------
    def phongAmbient (p: LightingPass.View) = { (x: Int, y: Int) =>
      val res = p.ambientValue (x, y)
      Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)) |> Colour.fromVector01
    } |> process (p)

    def phongDiffuse (p: LightingPass.View) = { (x: Int, y: Int) =>
      val res = p.diffuseValue (x, y)
      Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)) |> Colour.fromVector01
    } |> process (p)

    def phongSpecular (p: LightingPass.View) = { (x: Int, y: Int) =>
      val res = p.specularValue (x, y)
      Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)) |> Colour.fromVector01
    } |> process (p)


    // Combined final composition
    // --------------------------
    def aggregate (i: (DepthPass.View, ShadowPass.View, AmbientOcclusionPass.View, LightingPass.View)): Image = aggregate (i._1, i._2, i._3, i._4)
    def aggregate (depthPass: DepthPass.View, shadowPass: ShadowPass.View, aoPass: AmbientOcclusionPass.View, lightingPass: LightingPass.View) =
      Image (lightingPass.width, lightingPass.height,
        (0 until lightingPass.height).flatMap { y =>
          (0 until lightingPass.width).map { x =>

            if (depthPass.material (x, y).albedo == Colour.magenta)
              Colour.magenta
            else {

            val lighting =
              (lightingPass.ambientValue (x, y) * depthPass.material (x, y).albedo.toVector01) +
              (lightingPass.diffuseValue (x, y) * depthPass.material (x, y).albedo.toVector01) +
              lightingPass.specularValue (x, y)
            val shadowFactor = 1.0
            val aoFactor = 0.8
            val softShadowMultiplier = (1.0 - shadowPass.softShadowValue (x, y)) * shadowFactor
            val aoMultiplier = (1.0 - (1.0 - aoPass.occlusionValue (x, y)) * aoFactor)
            //val hardShadowMultiplier = (1.0 - shadowPass.hardShadowValue (x, y)) * shadowFactor
            //val res = lighting * aoMultiplier * hardShadowMultiplier
            val res = lighting * aoMultiplier * softShadowMultiplier
            Colour.fromVector01 (Vector (clamp01 (res.x), clamp01 (res.y), clamp01 (res.z)))

            }
          }
        })
  }


  // RGB Colour structure.
  // Ideally Scala would have proper unsigned types, in this case Char can double up as an 8-bit unsigned integer.
  //------------------------------------------------------------------------------------------------------------------//
  case class Colour (r: Char, g: Char, b: Char) {
    // Convert a colour to a Vector with components ranging between 0.0 and 1.0
    def toVector01: Vector = Vector (r.toDouble / 255.0, g.toDouble / 255.0, b.toDouble / 255.0)
  }
  object Colour {
    lazy val white        = Colour (255, 255, 255)
    lazy val black        = Colour (  0,   0,   0)
    lazy val magenta      = Colour (255,   0, 255) // programmer-art fan favourite
    lazy val coolBlack    = Colour (  0,  46,  99)
    lazy val warmBlack    = Colour (  0,  66,  66)
    lazy val persimmon    = Colour (236,  88,   0)
    lazy val vermillion   = Colour ( 89,  26,  20)
    lazy val jade         = Colour (  0, 168, 107)

    def from01 (v: Double): Colour = { val c = (clamp01 (v) * 255.0).toChar; Colour (c, c, c) }
    // Creates a Colour from 32-bit signed-integers.
    def apply (r: Int, g: Int, b: Int): Colour = Colour ((r |> clampInt (0, 255) _).toChar, (g |> clampInt (0, 255) _).toChar, (b |> clampInt (0, 255) _).toChar)
    // Interpolates `f` along a grayscale colour scale, `min` corresponding black and `max` corresponding to white.
    def fromRange     (f: Double, min: Double, max: Double) = { val c = ((f - min) / (max - min) * 255.0).toChar; Colour (c, c, c) }
    // Creates a colour from a vector with components ranging between 0.0 and 1.0
    def fromVector01  (v: Vector) = Colour ((v.x * 255.0).toChar, (v.y * 255.0).toChar, (v.z * 255.0).toChar)
  }

  // Basic camera with ability to generate rays corresponding to pixels at a specific resolution.
  // n.b. currently the near and far bounding planes are actually spherical surfaces defined by the spheres centered at the
  // camera position with radius of `near` and `far` respectively.
  // Camera `fov` is the vertical field of view; horizontal FOV varies depending on the viewport's aspect ratio
  //------------------------------------------------------------------------------------------------------------------//
  case class Camera (position: Vector, orientation: Quaternion, fov: Double, aspect: Double, near: Double, far: Double) {

    lazy val frustumDepth = far - near

    // Given a location anywhere on the near bounding surface of the camera's view frustum, returns the associated ray.
    private [this] def ray (x: Double, y: Double): Ray = { // co-ordinates range between -1.0 and +1.0 for each axis.
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

  // Simple bitmap stucture with the ability to write to PNG.
  //------------------------------------------------------------------------------------------------------------------//
  case class Image (width: Int, height: Int, pixels: IndexedSeq[Colour]) {
    def get (x: Int, y: Int): Colour = pixels (x + y * width)
  }
  object Image {
    def create (w: Int, h: Int)(r: IndexedSeq[Colour]): Image = Image (w, h, r)
    def writePNG (path: String)(image: Image): Unit = { // TODO: Not FP, urgh, do this IO properly later.
      import javax.imageio.ImageIO, java.awt.image.BufferedImage, java.io.File
      val buffer = new BufferedImage (image.width, image.height, BufferedImage.TYPE_INT_ARGB)
       (0 until image.height)
        .flatMap { y => (0 until image.width).map { x => (x, y) } }
        .foreach { case (x, y) =>
          val c = image.get (x, y)

          val a = if (c == Colour.magenta) 0 else 255
          val value = a << 24 | c.r.toInt << 16 | c.g.toInt << 8 | c.b.toInt
          buffer.setRGB (x, y, value)
        }
      val file = new File (path)
      file.getParentFile().mkdirs();
      ImageIO.write (buffer, "png", file)
    }
  }

  // Very basic 3D quaternion class, by no means complete, just the functions needed for this program.
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

  // Very basic 3D vector class, by no means complete, just the functions needed for this program.
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

