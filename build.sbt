name := "scalatron-jimbot"

artifactName:= { (_, _, _) => "ScalatronBot.jar" }

scalaVersion := "2.9.3"

scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.2"
)