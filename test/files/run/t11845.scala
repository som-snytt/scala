object Test {
  trait Two[@specialized C, @specialized F] {
    def trouble(p: Int): C
  }

  class Instance(val p: Int) extends Two[Long, Int] {
    override def trouble(p: Int): Long = 0
  }

  // in this trait, and only here, "C" is actually a cyrillic "С"
  // for специализация
  trait Entry {
    def call[@specialized С, @specialized F](a: Two[С, F]): Unit
  }

  object SimpleEntry extends Entry {
    def call[@specialized C, @specialized F](a: Two[C, F]): Unit = a.trouble(0)
  }

  def main(args: Array[String]): Unit = {
    SimpleEntry.call(new Instance(0))

    val algorithm: Entry = SimpleEntry
    algorithm.call(new Instance(0))
  }
}
