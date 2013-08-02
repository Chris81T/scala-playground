package de.geeksession

import org.joda.time.DateTime

// this is the needed import to provide classPath[..]
import scala.reflect._

trait BusinessObject
case class A(text: String, number: Int) extends BusinessObject
case class B(content: String, float: Float) extends BusinessObject

trait ServiceHelper[I] {

  protected implicit def t : ClassTag[I]

  protected var objects : List[I] = Nil

  import scala.collection.mutable.Map
  protected val objectsMap = Map[Int, I]()
  protected val anotherMap = Map[I, String]()

  val serviceName : String

  /**
   * will be used from the client ..
   */
  def incomingAnything(a: Any) {
    val partial : PartialFunction[Any, Unit] = {
      case _ : String => println("it is a String")
      case _ : Long => println("it is a Long")
      case o : I => // TODO abstract type pattern T is unchecked since it is eliminated by erasure case o : T =>        ^
        println(("%s: case o : I --> " format serviceName) + o)
        incomingBusinessObject(o)
      case _ => println("something else...")
    }
    partial(a)
  }

  private def incomingBusinessObject(o: I) {
    println("incoming business object = %s" format o)
    objects = objects :+ o
    objectsMap += objects.size -> o
    anotherMap += o -> new DateTime().toString
    println("current objects list = %s" format objects)
    println("current objects map = %s" format objectsMap)
    println("current another map = %s" format anotherMap)
    perform(o)
  }

  protected def perform(o: I)

}

object ServiceA extends ServiceHelper[A] {

  override protected def t = classTag[A]

  override val serviceName = "Service-A"
  override protected def perform(o: A) = println("ServiceA perform ---> %s" format o)
}

object ServiceB extends ServiceHelper[B] {

  def t = classTag[B]

  override val serviceName = "Service-B"
  override protected def perform(o: B) = println("ServiceB perform ---> %s" format o)
}

object Client extends App {
  println("start v4...")
  val a = A("A", 7)
  val b = B("Object B", 3.4f)
  ServiceA.incomingAnything("Hello String")
  ServiceA.incomingAnything(3L)
  ServiceA.incomingAnything(a);
  ServiceA.incomingAnything(b);
  ServiceB.incomingAnything(a);
  ServiceB.incomingAnything(b);
}
