import sbt._


class Mvn_Repo_Project(info: ProjectInfo) extends DefaultProject(info) {

  // NOTE: use asURL to convert win path sep to '/'
  val mvn_repo = "(Local) Maven Repo" at
                 Path.userHome.asURL + ".m2/repository"

  override
  def testJavaCompileOptions =
     super.testJavaCompileOptions ++ javaCompileOptions("-Xmx1280m")
}
