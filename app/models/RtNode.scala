package models

class RtNode(
    val id: Long,
    val name: String,
    val screenName: String,
    val image: String,
    var children: List[RtNode]
)

object RtNode {
  def apply(id: Long, name: String, screenName: String, image: String, children: List[RtNode]): RtNode =
    new RtNode(id, name, screenName, image, children)
  def apply(id: Long, name: String, screenName: String, image: String): RtNode =
    new RtNode(id, name, screenName, image, List())
}
