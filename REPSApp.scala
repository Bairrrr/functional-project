import scala.io.StdIn
import scala.annotation.tailrec

/**
 * REPS - Renewable Energy Plant System
 *
 * Contributors: boyangao, bofengli, yuxuanhe
 */
object REPSApp extends App {
  // File paths for the different energy source datasets
  val solarFile = "src/main/scala/project/solar panels.csv"
  val windFile = "src/main/scala/project//wind turbines.csv"
  val hydroFile = "src/main/scala/project/hydro power.csv"

  // Load data from CSV files and map them to their respective EnergySource types
  val solarData = FileIO.loadData(solarFile, Solar)
  val windData  = FileIO.loadData(windFile, Wind)
  val hydroData = FileIO.loadData(hydroFile, Hydro)

  // Combine all source data into a single master list
  val allData = solarData ++ windData ++ hydroData

  /**
   * Calculates and displays statistical metrics for a given list of energy readings.
   * Includes Mean, Median, Mode, Range, and Midrange.
   */
  def displayStats(data: List[EnergyReading]): Unit = {
    if (data.isEmpty) {
      println("No data available for analysis.")
      return
    }
    val values = data.map(_.value)
    println(f"Mean:     ${Analysis.calculateMean(values)}%.2f MW")
    println(f"Median:   ${Analysis.calculateMedian(values)}%.2f MW")
    println(f"Mode:     ${Analysis.calculateMode(values)}%.2f MW")
    println(f"Range:    ${Analysis.calculateRange(values)}%.2f MW")
    println(f"Midrange: ${Analysis.calculateMidrange(values)}%.2f MW")
  }

  /**
   * Prompts user for a date, validates it, and filters data for that specific day.
   * Displays hourly breakdown per source and a daily statistical summary.
   */
  def searchByDate(): Unit = {
    print("Enter date (DD/MM/YYYY): ")
    val input = StdIn.readLine()

    // Use the Validator utility to ensure date format is correct
    Validator.validateDate(input) match {
      case Right(date) =>
        val results = Analysis.filterByDate(allData, date)
        if (results.isEmpty) {
          println("No available data for the selected date. Please choose another date.")
        } else {
          println(s"\nResults for $date (All Sources):")
          // Sort results by source name and then by hour for a clean display
          results.sortBy(r => (r.source.toString, r.hour)).foreach { r =>
            println(f"${r.source}%-7s | Hour ${r.hour}%02d:00 | ${r.value}%8.2f MW | [${r.status}]")
          }
          println("\nDaily Summary Statistics:")
          displayStats(results)
        }
      case Left(error) => println(error)
    }
  }

  /**
   * Scans the dataset for any readings where the status is not "Normal".
   * Alerts the user and lists the first 15 issues found.
   */
  def checkHealth(): Unit = {
    val issues = allData.filter(_.status != "Normal")
    if (issues.isEmpty) {
      println("All systems normal.")
    } else {
      println(s"ATTENTION: ${issues.size} abnormal readings detected.")
      // Show a sample of detected issues
      issues.take(15).foreach(i => println(s"[${i.source}] Date: ${i.date}, Hour: ${i.hour}:00, Status: ${i.status}"))
    }
  }

  /**
   * Main application menu loop.
   * Uses @tailrec to ensure the recursive call is optimized and doesn't cause stack overflow.
   */
  @tailrec
  def menu(): Unit = {
    println("\n========================================")
    println("   Renewable Energy Plant System (REPS) ")
    println("========================================")
    println("1. Total Plant Statistics (April 2026)")
    println("2. Search & Analyze by Specific Date")
    println("3. Equipment Health & Alert Monitor")
    println("4. Exit")
    print("Select Option: ")

    StdIn.readLine() match {
      case "1" =>
        println("\n--- Global Production Summary ---")
        displayStats(allData)
        menu()
      case "2" =>
        searchByDate()
        menu()
      case "3" =>
        checkHealth()
        menu()
      case "4" =>
        println("Shutting down REPS...")
      case _   =>
        println("Invalid input. Please try again.")
        menu()
    }
  }

  // Launch the application
  menu()
}