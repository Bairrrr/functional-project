sealed trait EnergySource
case object Solar extends EnergySource
case object Wind extends EnergySource
case object Hydro extends EnergySource

case class EnergyReading(
                          date: String,
                          hour: Int,
                          source: EnergySource,
                          value: Double,
                          status: String
                        )