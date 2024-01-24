package playground

object Playground extends App {

  val quarterScore = "[32 - 15]"
  val allScoresZipped: List[(String, Int)] = quarterScore.replaceAll("\\[|\\]", "").split("-").toList.zipWithIndex
  val homeScores = allScoresZipped.filter((first, second) => second % 2 == 0).map((first, second) => first.trim)
  val awayScores = allScoresZipped.filter((first, second) => second % 2 != 0).map((first, second) => first.trim)

}
