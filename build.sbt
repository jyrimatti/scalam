name := "Scalam"

version := "1.0"

organization := "-"

scalaVersion := "2.9.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies ++= Seq(
	"org.scalala"   % "scalala_2.9.1"   % "1.0.0.RC2",
	"org.scalatest" % "scalatest_2.9.1" % "1.6.1"      % "test",
	"junit"         % "junit"           % "4.10"       % "test"
)