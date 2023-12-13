import scala.io.Source

object playing_with_files_and_strings {
  def get_next_word(sorted_lines_lexicographically: List[(List[String], Int)]): (String, Int) = {
    val stop_words = Set("the", "and", "of", "to", "about")
    var next_word = (sorted_lines_lexicographically(0)._1(0), 0)
    for ((line, index) <- sorted_lines_lexicographically) {
      for (word <- line) {
        if (word < next_word._1 && !stop_words.contains(word.toLowerCase)) {
          next_word = (word, index)
        }
      }
    }
    return next_word
  }


  def main(args: Array[String]) {
    val filename = args(0)
    val lines = Source.fromFile(filename)
                      .getLines
                      .toList
                      .map(x => x.split(" ").toList)
    val sorted_lines_lexicographically = lines.map(x => x.sorted).zipWithIndex

    var next_word = get_next_word(sorted_lines_lexicographically)
    println(next_word)
    // boring
  }
}
