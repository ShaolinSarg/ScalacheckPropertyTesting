import org.scalacheck._
import org.scalacheck.Prop.{forAll, classify}

class TestSpec extends Properties("my property") {


  case class Person(forename: String, surname: String, age: Int) {
    def fullName: String = s"$forename $surname"
  }

  val genPerson = for {
    forename <- Gen.oneOf("Dave", "Suzi", "Steve", "Dan", "Miffy")
    surname <- Gen.alphaStr
    age <- Gen.chooseNum(18, 80)
  } yield Person(forename, surname, age)

  property("should generate people with different details")  = forAll(genPerson :| "person") { (person: Person) =>
    classify(person.forename == "Dan", "Dan") {
      classify(person.surname == "", "no surname") {
        val result = person.fullName
        println(person)
        result.length == (person.forename + " " + person.surname).length
      }
    }
  }
}