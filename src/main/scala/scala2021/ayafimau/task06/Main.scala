package scala2021.ayafimau.task06

import scala2021.ayafimau.task06.Main.Sexes.{Female, Male, Sex}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {

  val testUsers = List(
    User("Misha", 20, "misha@tut.by", Male, 180),
    User("Sasha", 22, "sa sha@tut.by", Female, 165),
    User("Petya", 30, "", Male, 178),
    User("Kim Chong Il", 40, "kim ch @ il.by", Male, 178),
    User("Klusha", 125, "klu@gmail.com", Female, 178),
    User("Maryna", -3, "mary@gmail.com", Female, 160),
    User("Tallman", 50, "tall,man,,,@gmail.com", Male, 99),
  )

  println("Stop at first:")
  testUsers.map(x => (x.name, x.validateUntilFirst())).map(println)
  println("All sequential:")
  testUsers.map(x => (x.name, x.validateSeq())).map(println)
  println("All in parallel:")
  testUsers.map(x => (x.name, x.validateParallel())).map(println)

  case class User(name: String, age: Int, email: String, sex: Sex, height: Double) {

    def validateUntilFirst(): Option[String] = {
      validateName()
        .orElse(validateAge())
        .orElse(validateEmail())
        .orElse(validateSexVsHeight())
    }

    def validateSeq(): List[String] = {
      validateName() ++: validateAge() ++: validateEmail() ++: validateSexVsHeight() ++: List()
    }

    def validateParallel(): List[String] = {
      val aggregatedFuture = for {
        nameValidation <- Future(validateName())
        ageValidation <- Future(validateAge())
        emailValidation <- Future(validateEmail())
        sexVsHeightValidation <- Future(validateSexVsHeight())
      } yield List(nameValidation, ageValidation, emailValidation, sexVsHeightValidation)

      Await.result(aggregatedFuture, Duration.Inf).flatten
    }

    private def validateName(): Option[String] = {
      name match {
        case null => Some("Name is null.")
        case "" => Some("Name is an empty string.")
        case _ =>
          val leftOver = name.toCharArray.dropWhile(c => ('a' to 'z' contains c) || ('A' to 'Z' contains c))
          leftOver match {
            case Array() => None
            case suffix => Some(s"Name contains non-Latin character: '${suffix(0)}'")
          }
      }
    }

    private def validateAge(): Option[String] = {
      age match {
        case x if x <= 0 => Some("Age must be greater than zero")
        case x if x >= 100 => Some("Age must be less than 100")
        case _ => None
      }
    }

    //W3C recommendation: http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
    private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    private def validateEmail(): Option[String] = {
      email match {
        case null => Some("Email cannot be null")
        case e if e.isEmpty => None // empty emails are allowed by problem statement
        case e if e.trim.isEmpty => Some("Email cannot consist of whitespace")
        case e if emailRegex.findFirstMatchIn(e).isDefined => None
        case _ => Some("Email is in an incorrect/non-standard format")
      }
    }

    private def validateSexVsHeight(): Option[String] = {
      sex match {
        case Female => None
        case Male =>
          height match {
            case x if x <= 100 => Some("Male should have height greater than 100")
            case _ => None
          }
      }
    }
  }

  object Sexes extends Enumeration {
    type Sex = Value

    val Male, Female = Value
  }

}
