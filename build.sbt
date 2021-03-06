name := "tweet-virality"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.typesafe.akka"      %% "akka-actor"            % "2.2.3",
  "com.typesafe.akka"      %% "akka-slf4j"            % "2.2.3",
  "io.spray"                % "spray-can"             % "1.2.0",
  "io.spray"                % "spray-client"          % "1.2.0",
  "io.spray"                % "spray-routing"         % "1.2.0",
  "io.spray"               %% "spray-json"            % "1.2.5",
  "org.neo4j"               % "neo4j"                 % "2.0.0",
  "org.scalacheck"         %% "scalacheck"            % "1.10.0"  % "test"
)

play.Project.playScalaSettings
