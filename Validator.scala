object Validator {
  def validateDate(input: String): Either[String, String] = {
    val dateRegex = """(\d{2})/(\d{2})/(\d{4})""".r
    input match {
      case dateRegex(d, m, y) => Right(input)
      case _ =>
        Left(s"Invalid date format. Please enter the date in the format 'DD/MM/YYYY'.\n" +
          s"For example, enter '12/04/2026' for April 12, 2026.")
    }
  }
}