class Button(val label: String) {
  def click() = { /* Logic to give the appearance of clicking a button... */ }
}

trait /*abstract class*/ IObserver {
  def receiveUpdate(subject: Any)
}

class ButtonCountObserver extends AnyRef with IObserver {
  var count = 0
  def receiveUpdate(subject: Any) = count += 1
}

trait Subject {
  // type Observer = { def receiveUpdate(subject: Any) }
  type Observer = IObserver
  private var observers = List[Observer]()
  def addObserver(observer:Observer) = observers ::= observer
  def notifyObservers = observers foreach (_.receiveUpdate(this))
}

class ObservableButton(name: String) extends Button(name) with Subject {
  override def click() = {
    super.click()
    notifyObservers
  }
}

object ButtonObserverTest {
  def main(args: Array[String]) = {
    val observableButton = new ObservableButton("Okay")
    val buttonObserver = new ButtonCountObserver
    observableButton.addObserver(buttonObserver)
    for (i <- 1 to 3) observableButton.click()
    printf("The button has been clicked %d times\n", buttonObserver.count)
  }
}
