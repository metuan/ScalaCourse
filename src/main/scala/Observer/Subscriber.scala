package Observer

trait Subscriber {
  def handler(publisher: Publisher)
}
