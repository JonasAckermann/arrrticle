package example

object Hello extends App {

  val counted = tokenWithPrevious(Text.string).foldLeft(Map.empty[String, Occurrence]){ (acc, i) =>
    acc.updated(i._1, acc.getOrElse(i._1, Occurrence.init()).update(i._2))
  }

  println(counted.getOrElse("JVM", Occurrence.init()).mostLikely(0.5))

  // TODO read in text
  // TODO analyze text
  // 1. get token with previous
  // 2. for each, get most likely previous
  // 3. if == previous or most likely "" keep as is, else prepend
  // 4. reassemble
  val mostLikelyPrevious = tokenWithPrevious(Text.bad).map{ c =>
    val mostLikelyFor = counted.getOrElse(c._1, Occurrence.init()).mostLikely(0.5)
    if((mostLikelyFor == c._2) | (mostLikelyFor == "")) c._1 else (mostLikelyFor ++ " " ++ c._1)
  }
  val reassembled: String = mostLikelyPrevious.foldLeft(""){(acc, i) => acc ++ i ++ " "}

  println(reassembled)

  def tokenWithPrevious(in: String): Vector[(String, String)] = {
    val tokens = in.split(" ").toVector
    Vector(tokens.head -> "") ++ tokens.tail.zip(tokens.reverse.tail.reverse)
  }

}

case class Occurrence(total: Int, withA: Int, withAn: Int, withThe: Int){
  def update(newPrev: String): Occurrence =  newPrev match {
    case "a" => this.copy(total = total + 1, withA = withA + 1)
    case "an" => this.copy(total = total + 1, withAn = withAn + 1)
    case "the" => this.copy(total = total + 1, withThe = withThe + 1)
    case _ => this.copy(total = total + 1)
  }
  def mostLikely(threshold: Double): String = {
    val likeliest = Map("a" -> withA, "an" -> withAn, "the" -> withThe).view.mapValues(v => if(total != 0)v.toDouble/total.toDouble else 0).maxBy(_._2)
    if(likeliest._2 > threshold) likeliest._1 else ""
  }
}
object Occurrence{
  def init(): Occurrence = Occurrence(0,0,0,0)
}
