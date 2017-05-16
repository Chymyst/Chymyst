val commonSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "io.chymyst",
  version := "0.0.1",
  scalaVersion in ThisBuild := "2.12.2",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),
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

lazy val disableWarningsForTut = Set("-Ywarn-unused", "-Xlint")

enablePlugins(TutPlugin)

lazy val errorsForWartRemover = Seq(Wart.EitherProjectionPartial, Wart.Enumeration, Wart.Equals, Wart.ExplicitImplicitTypes, Wart.FinalCaseClass, Wart.FinalVal, Wart.LeakingSealed, Wart.Return, Wart.StringPlusAny, Wart.TraversableOps, Wart.TryPartial)

lazy val warningsForWartRemover = Seq(Wart.JavaConversions, Wart.IsInstanceOf, Wart.OptionPartial) //Seq(Wart.Any, Wart.AsInstanceOf, Wart.ImplicitConversion, Wart.Option2Iterable, Wart.NoNeedForMonad, Wart.Nothing, Wart.Product, Wart.Serializable, Wart.ToString, Wart.While)
val rootProject = Some(chymyst)

lazy val chymyst = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "chymyst-lab",
    wartremoverWarnings in(Compile, compile) ++= warningsForWartRemover,
    wartremoverErrors in(Compile, compile) ++= errorsForWartRemover,
    tutSourceDirectory := (sourceDirectory in Compile).value / "tut",
    tutTargetDirectory := baseDirectory.value / "docs",
    scalacOptions in Tut := scalacOptions.value.filterNot(disableWarningsForTut.contains),
    libraryDependencies ++= Seq(
      "io.chymyst" %% "core" % "latest.integration",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )
