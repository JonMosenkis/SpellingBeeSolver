import trie.Trie

import scala.io.Source

object SpellingBee extends App {
  def getWordsFromFile: Seq[String] = {
    def isWord(string: String): Option[String] =
      if (string.matches("^[a-zA-Z]+$")) Some(string.strip())
      else None

    val bufferedSource = Source.fromFile("src/main/resources/words.txt")
    val buffered = for {
      line <- bufferedSource.getLines()
      word <- isWord(line)
    } yield word.toLowerCase()

    val res = buffered.toList
    bufferedSource.close()
    res
  }


    val words = getWordsFromFile
    val trie = Trie.load(words)
    val chars: Seq[Char] = scala.io.StdIn.readLine("Enter the characters. First character is the center char:\n").toList
    val firstChar = chars.head
    val matches = trie.getWordsFromCharacters(chars).filter(word => word.length >= 4 && word.contains(firstChar))
    matches.foreach(println)
}
