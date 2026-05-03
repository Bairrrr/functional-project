import scala.io.Source
import java.io.File
import scala.annotation.tailrec

/**
 * REPS - Data Access and Processing Modules
 *
 * Contributors: boyangao, bofengli, yuxuanhe
 */

object FileIO {
  /**
   * Loads energy production data from a specified CSV file.
   * Parses the file, cleans the data, and maps it to a list of EnergyReading objects.
   *
   * @param filePath Path to the CSV source file
   * @param sourceType The type of energy (Solar, Wind, or Hydro)
   * @return A list of parsed EnergyReading records
   */
  def loadData(filePath: String, sourceType: EnergySource): List[EnergyReading] = {
    val file = new File(filePath)
    // Check if the file exists to avoid runtime errors
    if (!file.exists()) return Nil

    val source = Source.fromFile(file)
    // Skip the CSV header line and convert the remaining lines to a list
    val lines = source.getLines().drop(1).toList

    val readings = lines.flatMap { line =>
      // Split by semicolon and remove quotes or extra whitespace
      val columns = line.split(";").map(_.trim.filterNot(_ == '"'))
      try {
        // Parse Date: Extract YYYY-MM-DD from the timestamp and convert to DD/MM/YYYY
        val rawTime = columns(0)
        val dateParts = rawTime.substring(0, 10).split("-")
        val formattedDate = s"${dateParts(2)}/${dateParts(1)}/${dateParts(0)}"

        // Parse Hour: Extract the hour component from the timestamp string
        val hourValue = rawTime.substring(11, 13).toInt

        // Parse Production: Handle comma-based decimals (e.g., "1,23" to "1.23")
        val productionValue = columns(2).replace(",", ".").toDouble

        // Logic to detect equipment issues:
        // If it's daytime (8 AM to 5 PM) but solar production is 0 or less, mark as Malfunction.
        val operationalStatus = if (sourceType == Solar && productionValue <= 0 && hourValue > 8 && hourValue < 17)
          "Malfunction"
        else "Normal"

        // Return a successful reading wrapped in Some
        Some(EnergyReading(formattedDate, hourValue, sourceType, productionValue, operationalStatus))
      } catch {
        // Ignore malformed lines or parsing errors
        case _: Exception => None
      }
    }
    source.close() // Ensure the file resource is released
    readings
  }
}

object Analysis {
  /**
   * Calculates the arithmetic mean (average) of the data.
   */
  def calculateMean(data: List[Double]): Double = {
    // Tail-recursive helper function to calculate the sum efficiently
    @tailrec
    def sumAcc(xs: List[Double], acc: Double): Double = xs match {
      case Nil => acc
      case head :: tail => sumAcc(tail, acc + head)
    }
    if (data.isEmpty) 0.0 else sumAcc(data, 0.0) / data.length
  }

  /**
   * Calculates the median (middle value) of the dataset.
   */
  def calculateMedian(data: List[Double]): Double = {
    if (data.isEmpty) return 0.0
    val sorted = data.sorted
    val size = sorted.length
    // If odd, take the middle; if even, average the two middle elements
    if (size % 2 == 1) sorted(size / 2)
    else (sorted(size / 2 - 1) + sorted(size / 2)) / 2.0
  }

  /**
   * Calculates the mode (most frequent value) in the dataset.
   */
  def calculateMode(data: List[Double]): Double = {
    if (data.isEmpty) return 0.0
    // Group values by identity, count occurrences, and pick the one with the max count
    data.groupBy(identity).view.mapValues(_.size).toMap.maxBy(_._2)._1
  }

  /**
   * Calculates the range (difference between maximum and minimum).
   */
  def calculateRange(data: List[Double]): Double = {
    if (data.isEmpty) 0.0 else data.max - data.min
  }

  /**
   * Calculates the midrange (average of the maximum and minimum).
   */
  def calculateMidrange(data: List[Double]): Double = {
    if (data.isEmpty) 0.0 else (data.max + data.min) / 2.0
  }

  /**
   * Filters the master list for a specific date (DD/MM/YYYY).
   */
  def filterByDate(data: List[EnergyReading], targetDate: String): List[EnergyReading] = {
    data.filter(_.date == targetDate)
  }

  /**
   * Filters the master list for a specific month based on the date format.
   */
  def filterByMonth(data: List[EnergyReading], month: String): List[EnergyReading] = {
    data.filter(_.date.split("/")(1) == month)
  }
}