import sbt._


class PHPSProject(info : ProjectInfo) extends DefaultProject(info) {

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath   = "lib"
  override def testScalaSourcePath = "tests"
  
}
