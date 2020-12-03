package edu.evolution.varanovich.anki.adt

final case class User private(name: String, password: String, privileges: Privileges) extends Ordered[User] {
  override def equals(that: Any): Boolean = that match {
    case that: User => name.equalsIgnoreCase(that.name)
    case _ => false
  }

  override def compare(that: User): Int = this.name.compareToIgnoreCase(that.name)
}
object User {
  def from(name: String, password: String, privileges: Privileges): Option[User] =
    if (name.isEmpty || name.isBlank || password.isEmpty || password.isBlank) None else
      Some(User(name, password, privileges))

  def member(name: String, password: String): Option[User] =
    if (name.isEmpty || name.isBlank || password.isEmpty || password.isBlank) None else
      Some(User(name, password, Privileges.Member))
}