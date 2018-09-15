package nl.wwbakker.catssandbox.chapter11

object A {
  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): GCounter =
      GCounter(counters.updated(machine, counters.getOrElse(machine, 0) + amount))


    def merge(that: GCounter): GCounter =
      GCounter(
        (counters.keySet ++ that.counters.keySet).map { machine =>
          machine -> Integer.max(counters(machine), that.counters(machine))
        }.toMap
      )

    def total: Int =
      counters.values.sum
  }
}