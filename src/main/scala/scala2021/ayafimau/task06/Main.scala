package scala2021.ayafimau.task06

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

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
  testUsers.map(x => x.validateParallel onComplete {
    case Success(result) => println(x.name, result)
  })

  case class User(name: String, age: Int, email: String, sex: Sex, height: Double) {

    def validateUntilFirst(): Either[String, User] = {
      for {
        _ <- validateName()
        _ <- validateAge()
        _ <- validateEmail()
        user <- validateSexVsHeight()
      } yield user
    }

    def validateSeq(): Either[List[String], User] = {
      val validationResults = List(validateName(), validateAge(), validateEmail(), validateSexVsHeight())
        .filter(_.isLeft)
      validationResults match {
        case List() => Right(this)
        case errors => Left(errors.flatMap(_.left.toOption))
      }
    }

    def validateParallel(): Future[Either[List[String], User]] = {
      val validateNameFuture = Future(validateName())
      val validateAgeFuture = Future(validateAge())
      val validateEmailFuture = Future(validateEmail())
      val validateSexVsHeightFuture = Future(validateSexVsHeight())

      val validationResults = for {
        nameValidation <- validateNameFuture
        ageValidation <- validateAgeFuture
        emailValidation <- validateEmailFuture
        sexVsHeightValidation <- validateSexVsHeightFuture
      } yield List(nameValidation, ageValidation, emailValidation, sexVsHeightValidation)

      validationResults.transformWith {
        case Success(results) =>
          val validationErrors = results.filter(_.isLeft)
          validationErrors match {
            case List() => Future(Right(this))
            case errors => Future(Left(errors.flatMap(_.left.toOption)))
          }
        case Failure(error) => Future.failed(error)
      }
    }

    private def validateName(): Either[String, User] = {
      name match {
        case null => Left("Name is null.")
        case "" => Left("Name is an empty string.")
        case _ =>
          val leftOver = name.toCharArray.dropWhile(c => ('a' to 'z' contains c) || ('A' to 'Z' contains c))
          leftOver match {
            case Array() => Right(this)
            case suffix => Left(s"Name contains non-Latin character: '${suffix(0)}'")
          }
      }
    }

    private def validateAge(): Either[String, User] = {
      age match {
        case x if x <= 0 => Left("Age must be greater than zero")
        case x if x >= 100 => Left("Age must be less than 100")
        case _ => Right(this)
      }
    }

    //W3C recommendation: http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
    private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    private def validateEmail(): Either[String, User] = {
      email match {
        case null => Left("Email cannot be null")
        case e if e.isEmpty => Right(this) // empty emails are allowed by problem statement
        case e if e.trim.isEmpty => Left("Email cannot consist of whitespace")
        case e if emailRegex.findFirstMatchIn(e).isDefined => Right(this)
        case _ => Left("Email is in an incorrect/non-standard format")
      }
    }

    private def validateSexVsHeight(): Either[String, User] = {
      sex match {
        case Female => Right(this)
        case Male =>
          height match {
            case x if x <= 100 => Left("Male should have height greater than 100")
            case _ => Right(this)
          }
      }
    }
  }

  sealed trait Sex {
    def name: String
  }

  case object Male extends Sex {
    val name = "Male"
  }

  case object Female extends Sex {
    val name = "Female"
  }

}
