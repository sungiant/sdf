package sdf

object Extensions { implicit class I[A](val self: A) { def |>[B](f: A => B): B = f (self) } }; import Extensions._
object Configuration { lazy val tolerance = 0.0000001 }; import Configuration._

//--------------------------------------------------------------------------------------------------------------------//
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

//--------------------------------------------------------------------------------------------------------------------//
case class Quaternion (i: Double, j: Double, k: Double, u: Double)
object Quaternion {
  lazy val identity = Quaternion (0, 0, 0, 1)

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

//--------------------------------------------------------------------------------------------------------------------//
case class Colour (r: Char, g: Char, b: Char)
object Colour {
  lazy val black = Colour (0.toChar, 0.toChar, 0.toChar)
  lazy val white = Colour (255.toChar, 255.toChar, 255.toChar)

  def fromNormal (v: Vector): Colour = Colour (
    ((v.x + 1.0) / 2.0 * 255.0).toChar,
    ((v.y + 1.0) / 2.0 * 255.0).toChar,
    ((v.z + 1.0) / 2.0 * 255.0).toChar)
}

//--------------------------------------------------------------------------------------------------------------------//
case class Ray (position: Vector, direction: Vector)

//--------------------------------------------------------------------------------------------------------------------//
// `fov` is the vertical field of view; horizontal FOV varies depending on the viewport's aspect ratio
case class Camera (position: Vector, orientation: Quaternion, fov: Double, aspectRation: Double, near: Double, far: Double) {
  def getRay (x: Double, y: Double): Ray = {
    val xx = (x + 1.0) / 2.0
    val yy = (y + 1.0) / 2.0
    val sh = Math.sin (fov / 2.0)
    val nearNW: Vector = Vector ( near * sh * aspectRation,  near * sh, near)
    val nearSE: Vector = Vector (-near * sh * aspectRation, -near * sh, near)
    val farNW:  Vector = Vector (  far * sh * aspectRation,   far * sh, far)
    val farSE:  Vector = Vector (- far * sh * aspectRation, - far * sh, far)
    val f = Vector (xx, yy, 0.0)
    val p = position + Vector.lerp (nearNW, nearSE, f).transform (orientation)
    val d = Vector.lerp (farNW - nearNW, farSE - nearSE, f).transform (orientation).normalise
    Ray (p, d)
  }
}

//--------------------------------------------------------------------------------------------------------------------//
// Simple class for writting PPM files to disk.
case class Image (width: Int, height: Int, pixels: List[Colour]) {
  def getPixel (x: Int, y: Int): Colour = pixels (x + y * width)
  def pixelTable: List[(Int, Int)] = (0 until height).toList.flatMap { y => (0 until width).toList.map { x => (x, y) } }
}
object Image {
  def create (w: Int, h: Int)(r: List[Colour]): Image = Image (w, h, r)
  def toNetPBM (path: String)(image: Image): Unit = {
    val out = new java.io.DataOutputStream (new java.io.FileOutputStream (path))
    out.writeBytes ("P6\u000a%d %d\u000a%d\u000a".format (image.width, image.height, 255))
    image.pixelTable
      .map { case (x, y) => image.getPixel (x, y) }
      .foreach { c =>
        out.writeByte (c.r)
        out.writeByte (c.g)
        out.writeByte (c.b)
      }
  }
}

//--------------------------------------------------------------------------------------------------------------------//
// The absolute value of the return value indicates the distance to the surface.
// The sign indicates whether the position is inside or outside the surface, negative indicating inside.
object SDF {
  def intersect (a: Double, b: Double): Double = Math.max (a, b)
  def union (a: Double, b: Double): Double = Math.min (a, b)
  def difference (a: Double, b: Double): Double = Math.max (a, -b)

  // unit sphere (diameter = 1)
  def sphere (position: Vector, offset: Vector, diameter: Double): Double = 
    (position - offset).length - (diameter / 2.0)

  // unit cube with (h,w,d = 1)
  def cube (position: Vector, offset: Vector, size: Double): Double = {
    val d: Vector = (position - offset).abs - (Vector (size, size, size) / 2.0)
    val insideDistance: Double = Math.min (Math.max (d.x, Math.max (d.y, d.z)), 0.0)
    val outsideDistance: Double = d.max (0.0).length
    insideDistance + outsideDistance
  }
}

//--------------------------------------------------------------------------------------------------------------------//
// Where it all kicks off.  Right handed co-ordinate system.
object Program {
  
  def distanceAlongRayWhenItIntersectsASurface (ray: Ray, sdf: (Vector) => Double, start: Double, end: Double): Option[Double] = {
    @scala.annotation.tailrec
    def r (depth: Double, step: Int): Option[Double] = step match {
      case 0 => None
      case _ =>
        val pos = ray.position + ray.direction * depth
        (pos |> sdf) match {
          case dist if dist < tolerance => Some (depth) // hit, p is inside the surface
          case dist =>
            val newDepth = depth + dist
            if (newDepth >= end) None
            else r (newDepth, step - 1)
        }
    }
    val maxSteps = 100
    r (start, maxSteps)
  }

  def estimateNormal (pos: Vector, sdf: (Vector) => Double): Vector = {
    val sd = tolerance * 100.0 // sample distance
    Vector (
      sdf (Vector (pos.x + sd, pos.y, pos.z)) - sdf (Vector (pos.x - sd, pos.y, pos.z)),
      sdf (Vector (pos.x, pos.y + sd, pos.z)) - sdf (Vector (pos.x, pos.y - sd, pos.z)),
      sdf (Vector (pos.x, pos.y, pos.z + sd)) - sdf (Vector (pos.x, pos.y, pos.z - sd)),
    ).normalise
  }

  def scene (pos: Vector): Double = {
    SDF.difference (
      SDF.difference (
        SDF.difference (
          SDF.intersect (
            SDF.cube (pos, Vector.zero, 2.0),
            SDF.sphere (pos, Vector.zero, 2.4)),
          SDF.sphere (pos, Vector.unitX, 0.8)),
        SDF.sphere (pos, Vector.unitY, 0.8)),
      SDF.sphere (pos, Vector.unitZ, 0.8))
  }

  def demo (w: Int, h: Int): Image = { // w & h in units of pixels
    val (y, zx) = (4.0, 6.0)
    val theta = (Math.sqrt (zx * zx * 2.0) / y) |> Math.atan
    val camera = Camera (
      Vector (zx, y, zx),
      Quaternion.fromYawPitchRoll (5.0 * math.Pi / 4.0, (math.Pi / 2.0) - theta, 0.0),
      57.0, w.toDouble / h.toDouble, 0.1, 100.0)
    (0 until h).toList.flatMap { j =>
      (0 until w).toList.map { i =>
        val fx = 2.0 - (2.0 * (i.toDouble + 0.5) / w.toDouble) - 1.0 // x axis: -1.0 to 1.0
        val fy = 2.0 - (2.0 * (j.toDouble + 0.5) / h.toDouble) - 1.0 // y axis: -1.0 to 1.0
        val pixelRay = camera.getRay (fx, fy)
        distanceAlongRayWhenItIntersectsASurface (pixelRay, scene, camera.near, camera.far) match {
          case None => Colour.black
          case Some (d) =>
            val pointOfIntersection = pixelRay.position + pixelRay.direction * d
            val normal = estimateNormal (pointOfIntersection, scene)
            Colour.fromNormal (normal)
        }
      }
    } |> Image.create (w, h) _
  }

  def main (args: Array[String]): Unit = {
    val path = "sdf.ppm"
    val file = new java.io.File (path)
    file.exists match { case true => file.delete (); case false => () }
    demo (640, 360) |> Image.toNetPBM (path) _
  }
}