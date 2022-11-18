import org.specs2.mutable.Specification
import trie.Trie

class TrieTest extends Specification {

  "A Trie should" >> {
    "detects single letter word" >> {
      val myWord = "a"
      val trie = Trie.load(Seq(myWord))
      trie.containsWord(myWord) must===(true)
    }
    "returns false if word is not included single letter" >> {
      Trie.empty.containsWord("z") must===(false)
    }
    "trie with multiple single letter words" >> {
      val myWords = Seq("a", "b")
      val trie = Trie.load(myWords)
      myWords.forall(word => trie.containsWord(word)) must===(true)
    }

    "differentiate between multi letter and single letter words" >> {
      val myWords = Seq("at")
      val trie = Trie.load(myWords)
      myWords.forall(word => trie.containsWord(word)) must===(true)
      trie.containsWord("a") must===(false)
    }

    "can load multiple multi letter words" >> {
      val myWords = Seq("hello", "attempt", "a", "ate", "help")
      val trie = Trie.load(myWords)
      myWords.forall(word => trie.containsWord(word)) must===(true)
      trie.containsWord("at") must===(false)
    }

    "get completed word" >> {
      val trie = Trie.load(Seq("at"))
      val childTrie = trie.children.get('a').flatMap(_.completesWord('t')) must beSome("at")
    }

    "find single letter words" >> {
      val trie = Trie.load(Seq("a", "b", "c", "d"))
      val chars = Seq('a', 'b', 'd')
      val results = trie.getWordsFromCharacters(chars)
      results must contain("a", "b", "d")
      results must haveSize(3)
    }

    "find all words from characters" >> {
      val trie = Trie.load(Seq("cat", "hat", "bat", "bold", "bob"))
      val chars = Seq('b', 'o', 't', 'a', 'c')
      val results = trie.getWordsFromCharacters(chars)
      results must contain("bob", "bat", "cat")
      results must haveSize(3)
    }
  }
}
