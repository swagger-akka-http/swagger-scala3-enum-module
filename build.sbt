// Settings file for all the modules.
import xml.Group
import sbt._
import Keys._
import org.typelevel.sbt.gha.JavaSpec.Distribution.Zulu

organization := "com.github.swagger-akka-http"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / organizationHomepage := Some(url("https://github.com/swagger-akka-http/swagger-scala3-enum-module"))

ThisBuild / scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked")

Test / publishArtifact := false

pomIncludeRepository := { x => false }

libraryDependencies ++= Seq(
  "io.swagger.core.v3" % "swagger-core-jakarta" % "2.2.17",
  "com.github.swagger-akka-http" %% "swagger-scala-module" % "2.11.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.15.3",
  "org.scalatest" %% "scalatest" % "3.2.17" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.9" % Test
)

homepage := Some(new URL("https://github.com/swagger-akka-http/swagger-scala3-enum-module"))

Test / parallelExecution := false

startYear := Some(2021)

licenses := Seq(("Apache License 2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.html")))

releasePublishArtifactsAction := PgpKeys.publishSigned.value

pomExtra := {
  pomExtra.value ++ Group(
      <issueManagement>
        <system>github</system>
        <url>https://github.com/swagger-api/swagger-scala3-enum-module/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>pjfanning</id>
          <name>PJ Fanning</name>
          <url>https://github.com/pjfanning</url>
        </developer>
      </developers>
  )
}

MetaInfLicenseCopy.settings

ThisBuild / tlSonatypeUseLegacyHost := true
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Zulu, "8"))
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      "CI_SNAPSHOT_RELEASE" -> "+publishSigned"
    )
  )
)

