package sdf

object Extensions { implicit class I[A](val self: A) { def |>[B](f: A => B): B = f (self) } }; import Extensions._
object Configuration { lazy val tolerance = 0.0000001 }; import Configuration._

//--------------------------------------------------------------------------------------------------------------------//

case class Quaternion (i: Double, j: Double, k: Double, u: Double)
object Quaternion {
  lazy val identity = Quaternion (0, 0, 0, 1)

  // Right handed co-ordinate system.
  def fromYawPitchRoll (yaw: Double, pitch: Double, roll: Double): Quaternion = {
    val (y, p, r) = (yaw * 0.5, pitch * 0.5, roll * 0.5)
    val (sy, cy) = (y |> Math.sin, y |> Math.cos)
    val (sp, cp) = (p |> Math.sin, p |> Math.cos)
    val (sr, cr) = (r |> Math.sin, r |> Math.cos)
    Quaternion (
      (cy * sp * cr) + (sy * cp * sr),
      (sy * cp * cr) - (cy * sp * sr),
      (cy * cp * sr) - (sy * sp * cr),
      (cy * cp * cr) + (sy * sp * sr))
  }
}

case class Ray (position: Vector, direction: Vector)

case class Vector (x: Double, y: Double, z: Double) {
  lazy val isZero: Boolean = Math.abs (x) < tolerance && Math.abs (y) < tolerance && Math.abs (z) < tolerance
  lazy val length: Double = isZero match { case false => (x * x + y * y + z * z) |> Math.sqrt; case true => 0.0 }
  lazy val normalise: Vector = this.length match { case 0.0 => Vector.zero; case l => Vector (x / l, y / l, z / l) }
  lazy val abs: Vector = Vector (Math.abs (x), Math.abs (y), Math.abs (z))

  def max (v: Double): Vector = Vector.max (this, v)
  def min (v: Double): Vector = Vector.min (this, v)
  def unary_+ (): Vector = this
  def unary_- (): Vector = Vector.negate (this)
  def + (x: Vector): Vector = Vector.add (this, x)
  def - (x: Vector): Vector = Vector.subtract (this, x)
  def * (x: Vector): Vector = Vector.multiply (this, x)
  def * (m: Double): Vector = Vector.multiply (this, m)
  def / (x: Vector): Vector = Vector.divide (this, x)
  def / (m: Double): Vector = Vector.divide (this, m)
  def transform (q: Quaternion): Vector = Vector.transform (this, q)
}
object Vector {
  lazy val (zero, one) = (Vector (0, 0, 0), Vector (1, 1, 1))
  lazy val (unitX, unitY, unitZ) = (Vector (1, 0, 0), Vector (0, 1, 0), Vector (0, 0, 1))

  def max (v: Vector, x: Double): Vector = Vector (Math.max (v.x, x), Math.max (v.y, x), Math.max (v.z, x))
  def min (v: Vector, x: Double): Vector = Vector (Math.min (v.x, x), Math.min (v.y, x), Math.min (v.z, x))
  def negate (v: Vector): Vector = Vector (-v.x, -v.y, -v.z)
  def add (a: Vector, b: Vector): Vector = Vector (a.x + b.x, a.y + b.y, a.z + b.z)
  def subtract (a: Vector, b: Vector): Vector = Vector (a.x - b.x, a.y - b.y, a.z - b.z)
  def multiply (a: Vector, b: Vector): Vector = Vector (a.x * b.x, a.y * b.y, a.z * b.z)
  def multiply (v: Vector, m: Double): Vector = Vector (v.x * m, v.y * m, v.z * m)
  def divide (a: Vector, b: Vector): Vector = Vector (a.x / b.x, a.y / b.y, a.z / b.z)
  def divide (v: Vector, m: Double): Vector = Vector (v.x / m, v.y / m, v.z / m)

  def transform (v: Vector, q: Quaternion): Vector = {
    val (ii, jj, kk) = (q.i * q.i, q.j * q.j, q.k * q.k)
    val (ui, uj, uk) = (q.u * q.i, q.u * q.j, q.u * q.k)
    val (ij, ik, jk) = (q.i * q.j, q.i * q.k, q.j * q.k)
    Vector (
      + v.x - (2.0 * v.x * (jj + kk)) + (2.0 * v.y * (ij - uk)) + (2.0 * v.z * (ik + uj)),
      + v.y + (2.0 * v.x * (ij + uk)) - (2.0 * v.y * (ii + kk)) + (2.0 * v.z * (jk - ui)),
      + v.z + (2.0 * v.x * (ik - uj)) + (2.0 * v.y * (jk + ui)) - (2.0 * v.z * (ii + jj)))
  }

  def lerp (a: Vector, b: Vector, f: Vector): Vector = Vector (
    a.x + (b.x - a.x) * f.x,
    a.y + (b.y - a.y) * f.y,
    a.z + (b.z - a.z) * f.z)
}

case class Colour (r: Byte, g: Byte, b: Byte)
object Colour {
  lazy val black = Colour (0.toByte, 0.toByte, 0.toByte)
  lazy val white = Colour (255.toByte, 255.toByte, 255.toByte)

  def fromNormal (v: Vector): Colour = Colour (
    ((v.x + 1.0) / 2.0 * 255.0).toByte,
    ((v.y + 1.0) / 2.0 * 255.0).toByte,
    ((v.z + 1.0) / 2.0 * 255.0).toByte)
}

// Camera `fov` is the vertical field of view; horizontal FOV varies depending on the viewport's aspect ratio
case class Camera (position: Vector, orientation: Quaternion, fov: Double, aspect: Double, near: Double, far: Double) {
  // Given a location anywhere on the near bounding surface of the camera's view frustum, returns the associated ray.
  def ray (x: Double, y: Double): Ray = {
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
}

case class Image (width: Int, height: Int, pixels: IndexedSeq[Colour]) {
  def get (x: Int, y: Int): Colour = pixels (x + y * width)
}
object Image {
  def create (w: Int, h: Int)(r: IndexedSeq[Colour]): Image = Image (w, h, r)
  def toNetPBM (path: String)(image: Image): Unit = {
    val out = new java.io.DataOutputStream (new java.io.BufferedOutputStream (
      java.nio.file.Files.newOutputStream (java.nio.file.Paths.get (path), java.nio.file.StandardOpenOption.CREATE)))
    out.writeBytes ("P6\u000a%d %d\u000a%d\u000a".format (image.width, image.height, 255))
    (0 until image.height)
      .flatMap { y => (0 until image.width).map { x => (x, y) } }
      .map { case (x, y) => image.get (x, y) }
      .foreach { c => out.writeByte (c.r); out.writeByte (c.g); out.writeByte (c.b) }
      out.flush ()
  }
}

//--------------------------------------------------------------------------------------------------------------------//

object Program {

  // Defines the signature of a signed distance function.  The absolute value of the result indicates the distance to
  // the surface, the sign of the result indicates whether the position is inside or outside of the surface, negative indicating inside.
  type SDF = Vector => Double

  // Enumeration of constructive solid geometry operations
  trait CSG; object CSG { object Intersect extends CSG; object Union extends CSG; object Difference extends CSG }

  // A scene is defined as a list of edits.
  case class Edit (sdf: SDF, op: CSG)

  // Folds a list of edits into an SDF.
  def evaluate (edits: List[Edit]): Option[SDF] = edits.foldLeft (None: Option[SDF]){ (acc: Option[SDF], edit: Edit) =>
    Some { (pos: Vector) => 
      val b = edit.sdf (pos)
      acc match {
        case None => b // Ignore the first edit's op.
        case Some (asdf) =>
          val a = asdf (pos)
          edit.op match {
            case CSG.Intersect   => Math.max ( a,  b)
            case CSG.Union       => Math.min ( a,  b)
            case CSG.Difference  => Math.max ( a, -b)
          }
      }
    }
  }

  // Signed distance function for a unit sphere (diameter = 1).
  def sphere (offset: Vector, diameter: Double)(position: Vector): Double = (position - offset).length - (diameter / 2.0)

  // Signed distance function for a unit cube (h,w,d = 1).
  def cube (offset: Vector, size: Double)(position: Vector): Double = {
    val d: Vector = (position - offset).abs - (Vector (size, size, size) / 2.0)
    val insideDistance: Double = Math.min (Math.max (d.x, Math.max (d.y, d.z)), 0.0)
    val outsideDistance: Double = d.max (0.0).length
    insideDistance + outsideDistance
  }
  
  // Given a ray and an SDF recursively evaluates the SDF until an interestion is either found or the limit of iterations is reached.
  def distanceAlongRayWhenItHitsASurface (ray: Ray, sdf: (Vector) => Double, start: Double, end: Double): Option[Double] = {
    val limit = 100
    @scala.annotation.tailrec
    def r (depth: Double, step: Int): Option[Double] = step match {
      case 0.0 => None
      case _ =>
        val p = ray.position + ray.direction * depth
        (p |> sdf) match {
          case dist if dist < tolerance => Some (depth) // Hit! `p` is inside the surface.
          case dist =>
            val newDepth = depth + dist
            if (newDepth >= end) None
            else r (newDepth, step - 1)
        }
    }
    r (start, step = limit)
  }

  def estimateNormal (pos: Vector, sdf: (Vector) => Double): Vector = {
    val sd = tolerance * 100.0 // sample distance
    Vector (
      sdf (Vector (pos.x + sd, pos.y, pos.z)) - sdf (Vector (pos.x - sd, pos.y, pos.z)),
      sdf (Vector (pos.x, pos.y + sd, pos.z)) - sdf (Vector (pos.x, pos.y - sd, pos.z)),
      sdf (Vector (pos.x, pos.y, pos.z + sd)) - sdf (Vector (pos.x, pos.y, pos.z - sd)),
    ).normalise
  }

  val scene: List[Edit] =
    Edit (  cube (Vector.zero,  2.0) _, CSG.Intersect ) ::
    Edit (sphere (Vector.zero,  2.4) _, CSG.Intersect ) ::
    Edit (sphere (Vector.unitX, 0.8) _, CSG.Difference) ::
    Edit (sphere (Vector.unitY, 0.8) _, CSG.Difference) ::
    Edit (sphere (Vector.unitZ, 0.8) _, CSG.Difference) :: Nil

  def demo (w: Int, h: Int): Image = { // w & h in units of pixels
    val (y, zx) = (4.0, 6.0)
    val theta = (Math.sqrt (zx * zx * 2.0) / y) |> Math.atan
    val camera = Camera (
      position = Vector (zx, y, zx),
      orientation = Quaternion.fromYawPitchRoll (5.0 * math.Pi / 4.0, (math.Pi / 2.0) - theta, 0.0),
      fov = 57.0, aspect = w.toDouble / h.toDouble, near = 0.1, far = 100.0)
    (0 until h).flatMap { j =>
      (0 until w).map { i =>
        val fx = 2.0 - (2.0 * (i.toDouble + 0.5) / w.toDouble) - 1.0 // x axis: -1.0 to 1.0
        val fy = 2.0 - (2.0 * (j.toDouble + 0.5) / h.toDouble) - 1.0 // y axis: -1.0 to 1.0
        val pixelRay = camera.ray (fx, fy)
        val sdf = evaluate (scene).getOrElse ((_: Vector) => 0.0)
        distanceAlongRayWhenItHitsASurface (pixelRay, sdf, camera.near, camera.far) match {
          case None => Colour.black
          case Some (d) =>
            val pointOfIntersection = pixelRay.position + pixelRay.direction * d
            val normal = estimateNormal (pointOfIntersection, sdf)
            Colour.fromNormal (normal)
        }
      }
    } |> Image.create (w, h) _
  }

  def main (args: Array[String]): Unit = demo (640, 360) |> Image.toNetPBM ("sdf.ppm") _
}