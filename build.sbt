name := "tproger_bot"

version := "1.1"

lazy val `tproger_bot` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(jdbc, cache, ws, specs2 % Test)

libraryDependencies += "org.jsoup" % "jsoup" % "1.10.2"

//libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102.jdbc41"
//2.1.0

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "9.3-1100-jdbc4",
  "com.typesafe.slick" %% "slick" % "3.0.0"
 )

unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

herokuAppName in Compile := "tproger_bot"
