package com.github.ktonga.tweetvirality.models.twitter

import org.neo4j.graphdb._
import org.neo4j.cypher.javacompat.{ExecutionResult, ExecutionEngine}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import scala.util.{Failure, Success, Try}
import scala.compat.Platform

object Neo4j {

  case class NeoLabel(name: String) extends Label
  case class NeoRelType(name: String) extends RelationshipType

  val User = NeoLabel("User")
  val Tweet = NeoLabel("Tweet")

  val Follows = NeoRelType("Follows")
  val FollowedBy = NeoRelType("FollowedBy")
  val Twitted = NeoRelType("Twitted")
  val TwittedBy = NeoRelType("TwittedBy")

}

abstract class Neo4j(dbPath: String) {

  val graphDb = embedded(dbPath)
  lazy val executionEngine = new ExecutionEngine(graphDb)

  def embedded(path: String): GraphDatabaseService = new GraphDatabaseFactory().newEmbeddedDatabase(path)
  def shutdown() = graphDb.shutdown()

  def node(labels: Seq[Label] = Seq(), properties: Map[String, Any] = Map()) = {
    val node = graphDb.createNode(labels: _*)
    properties foreach {
      case (k, v) => node.setProperty(k, v)
    }
    node
  }

  implicit class NodeOps(val node: Node) {
    def -->(other: Node, relType: RelationshipType, properties: Map[String, Any] = Map()) = {
      val relationship = node.createRelationshipTo(other, relType)
      properties foreach {
        case (k, v) => relationship.setProperty(k, v)
      }
      relationship
    }
  }

  def doWithinTx[A](f: (Transaction, GraphDatabaseService) => A): Try[A] = {
    val tx = graphDb.beginTx
    val tryA = Try {
      f(tx, graphDb)
    }
    tryA match {
      case Success(_) => tx.success()
      case Failure(_) => tx.failure()
    }
    tx.close()
    tryA
  }

  def execute(query: String): Try[ExecutionResult] = {
    println("Executing: \n" + query)
    val start = Platform.currentTime
    val result = Try {
      executionEngine.execute(query)
    }
    result match {
      case Success(r) =>
        println(s"Execution time: ${Platform.currentTime - start} ms")
        println(r.getQueryStatistics)
      case Failure(t) => t.printStackTrace()
    }
    result
  }

}

