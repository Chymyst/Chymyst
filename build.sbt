val commonSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "io.chymyst",
  version := "0.0.1",
  scalaVersion in ThisBuild := "2.11.8",
  crossScalaVersions := Seq("2.11.5", "2.11.6", "2.11.7", "2.11.8", "2.11.9", "2.12.0", "2.12.1"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    "Typesafe releases" at "http://repo.typesafe.com/typesafe/releases"
  ),

  scalacOptions ++= Seq(// https://tpolecat.github.io/2014/04/11/scalac-flags.html
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-feature",
    //    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    // "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args", // Makes calling a() fail to substitute a Unit argument into a.apply(x: Unit)
    "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Ywarn-unused"
  ) ++ (//target:jvm-1.8 supported from 2.11.5, warn-unused-import deprecated in 2.12
    if (scalaVersion.value startsWith "2.11") {
      val revision = scalaVersion.value.split('.').last.toInt
      Seq("-Ywarn-unused-import") ++ (
        if (revision >= 5) {
          Seq("-target:jvm-1.8")
        }
        else {
          Nil
        })
    }
    else Nil
    )
    ++ (
    if (scalaVersion.value startsWith "2.12") Seq("-target:jvm-1.8", "-Ypartial-unification") // (SI-2712 pertains to partial-unification)
    else Nil
    )
)

tutSettings

lazy val errorsForWartRemover = Seq(Wart.EitherProjectionPartial, Wart.Enumeration, Wart.Equals, Wart.ExplicitImplicitTypes, Wart.FinalCaseClass, Wart.FinalVal, Wart.LeakingSealed, Wart.Return, Wart.StringPlusAny, Wart.TraversableOps, Wart.TryPartial)

lazy val warningsForWartRemover = Seq() //Seq(Wart.Any, Wart.AsInstanceOf, Wart.ImplicitConversion, Wart.IsInstanceOf, Wart.JavaConversions, Wart.Option2Iterable, Wart.OptionPartial, Wart.NoNeedForMonad, Wart.Nothing, Wart.Product, Wart.Serializable, Wart.ToString, Wart.While)

val rootProject = Some(chymyst)

lazy val chymyst = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "chymyst",
    libraryDependencies ++= Seq(
      "io.chymyst" %% "core" % "latest.integration",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    )
  )
