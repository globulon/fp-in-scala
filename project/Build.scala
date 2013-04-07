import sbt._
import Keys._

object Properties {
  lazy val appVer         = "0.1-SNAPSHOT"
  lazy val scalaVer       = "2.10.1"
  lazy val scalaTestVer   = "1.9.1"
  lazy val scalaCheckVer  = "1.1.1"
 }

object BuildSettings {
  import Properties._
  lazy val buildSettings = Defaults.defaultSettings ++ Seq (
    organization        := "com.promindis",
    version             := appVer,
    scalaVersion        := scalaVer,
    scalacOptions       := Seq("-unchecked", "-deprecation"),
    ivyValidate         := false

  )
}

object Resolvers {
  lazy val typesafeReleases = "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
  lazy val scalaToolsRepo = "sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/"
}

object ApplicationDependencies {
  import Properties._
}

object TestDependencies {
  import Properties._
  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVer % "test" withSources()
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.0" %  "test" withSources()
}

object ApplicationBuild extends Build {
  import Resolvers._
  import TestDependencies._
  import BuildSettings._

  lazy val fpInScala = Project(
    "fp-in-scala",
    file("."),
    settings = buildSettings ++ Seq(resolvers += typesafeReleases) ++ Seq(scalacOptions ++= Seq("-feature", "-target:jvm-1.7")) ++
              Seq (libraryDependencies ++= Seq(scalaTest, scalaCheck))
  )
}
