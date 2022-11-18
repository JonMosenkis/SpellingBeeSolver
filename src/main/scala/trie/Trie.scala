package trie


case class Trie(children: Map[Char, Trie], prefix: String = "", wordEnd: Boolean = false) {

  def hasLetter(letter: Char): Boolean = children.contains(letter)

  def containsWord(word: String): Boolean = containsWord(word.toList)

  def containsWord(word: Seq[Char]): Boolean = word match {
    case Nil => false
    case char :: Nil => children.get(char).exists(_.wordEnd)
    case char :: rest => children.get(char).exists(_.containsWord(rest))
  }

  def addWord(word: String): Trie = addWord(word.toList)

  def addWord(word: Seq[Char]): Trie =
    word match {
      case char :: Nil => {
        val nextNode = children.getOrElse(char, Trie.empty)
          .copy(prefix = this.prefix + char, wordEnd = true)

        this.copy(children = children.updated(char, nextNode))
      }
      case char :: rest => {
        val nextNode = children.getOrElse(char, Trie.empty)
          .copy(prefix = this.prefix + char)
          .addWord(rest)

        this.copy(children = children.updated(char, nextNode))
      }
      case Nil => this
    }

  def completesWord(char: Char): Option[String] =
    children.get(char).flatMap(
      child => {
        if (child.wordEnd) Some(prefix + char)
        else None
      }
    )

  def getWordsFromCharacters(characters: Seq[Char]): Seq[String] =
    Trie.getWordsFromCharacters(this, characters)
}

object Trie {

  def load(withWords: Seq[String]): Trie =
    withWords.foldLeft(Trie.empty)(_.addWord(_))

  def empty: Trie = Trie(Map.empty)

  def getWordsFromCharacters(trieNode: Trie, characters: Seq[Char]): Seq[String] =
    characters.flatMap(char => {
      val childrenResults = trieNode.children.get(char) match {
        case Some(node) => getWordsFromCharacters(node, characters)
        case None => Seq.empty
      }
      childrenResults ++ trieNode.completesWord(char).toList
    })
}

