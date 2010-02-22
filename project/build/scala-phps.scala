import sbt._


class PHPSProject(info : ProjectInfo) extends DefaultProject(info) {

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath   = "lib"
  override def testScalaSourcePath = "tests"

  val scalatools = "Scala Tools Repository" at 
    "http://scala-tools.org/repo-releases/"

  val scalatest = "org.scalatest" % "scalatest" % "1.0" % "test"
  
}
