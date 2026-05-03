/**
 * REPS - Functional Programming Extensions (Functors)
 *
 * Contributors: boyangao, bofengli, yuxuanhe
 */

// A generic Trait defining the Functor mathematical structure
trait Functor[F[_]] {
  /**
   * Applies a transformation function 'f' to a value inside a container 'F'.
   * It "lifts" a function A => B into a function F[A] => F[B].
   */
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// A simple wrapper class to encapsulate energy-related values
case class EnergyWrapper[A](value: A)

// Implementation of the Functor trait specifically for EnergyWrapper
object EnergyFunctor extends Functor[EnergyWrapper] {
  /**
   * Takes a value inside EnergyWrapper and applies the function 'f' to it,
   * returning a new EnergyWrapper containing the result.
   */
  override def map[A, B](fa: EnergyWrapper[A])(f: A => B): EnergyWrapper[B] = {
    EnergyWrapper(f(fa.value))
  }
}

object FunctorApp extends App {

  // Create an initial reading wrapped in our container
  val mwReading = EnergyWrapper(120.50)
  println(s"Initial Reading: ${mwReading.value} MW")

  // Use the Functor to transform Megawatts (MW) to Kilowatts (kW)
  val kwReading = EnergyFunctor.map(mwReading)(v => v * 1000)
  println(s"Converted Reading: ${kwReading.value} kW")

  // Use the Functor to transform a numeric value into a formatted String report
  val report = EnergyFunctor.map(kwReading)(v => s"System Output: $v kW [STATUS: STABLE]")
  println(report.value)

  // Verification of Functor Laws

  // Law 1: Identity
  // Mapping the identity function (x => x) should result in the original container
  val law1 = EnergyFunctor.map(mwReading)(x => x) == mwReading
  println(s"Functor Law 1 (Identity) satisfied: $law1")

  // Law 2: Composition
  // Mapping f then g should be the same as mapping the composition of f and g (g(f(x)))
  val f: Double => Double = _ * 2
  val g: Double => String = v => s"$v units"

  val left = EnergyFunctor.map(EnergyFunctor.map(mwReading)(f))(g)
  val right = EnergyFunctor.map(mwReading)(x => g(f(x)))

  println(s"Functor Law 2 (Composition) satisfied: ${left == right}")
}