name := "freec-sandbox"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions")
