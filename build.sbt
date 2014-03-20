name := "scalatron-jimbot"

artifactName:= { (_, _, _) => "ScalatronBot.jar" }

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-encoding", "UTF-8", "-target:jvm-1.7", "-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.10" % "test"
)