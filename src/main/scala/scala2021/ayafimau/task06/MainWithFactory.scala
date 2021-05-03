package scala2021.ayafimau.task06

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
object MainWithFactory extends App {

  val testData = List(
    ("Misha", 20, "misha@tut.by", Male, 180),
    ("Sasha", 22, "sa sha@tut.by", Female, 165),
    ("Petya", 30, "", Male, 178),
    ("Kim Chong Il", 40, "kim ch @ il.by", Male, 178),
    ("Klusha", 125, "klu@gmail.com", Female, 178),
    ("Maryna", -3, "mary@gmail.com", Female, 160),
    ("Tallman", 50, "tall,man,,,@gmail.com", Male, 99),
  )

  println("Stop at first:")
  testData.map(x => (x._1, UserFactory.create(x._1, x._2, x._3, x._4, x._5, ValidateUntilFirst))).map(println)
  println("All sequential:")
  testData.map(x => (x._1, UserFactory.create(x._1, x._2, x._3, x._4, x._5, ValidateSeq))).map(println)
  println("All in parallel:")
  testData.map(x => (x._1, UserFactory.create(x._1, x._2, x._3, x._4, x._5, ValidateParallel))).map(println)

  object UserFactory {
    def create(name: String, age: Int, email: String, sex: Sex, height: Double, validation: ValidationOption = ValidateSeq): Either[List[String], User] = {
      val user = User(name, age, email, sex, height)
      validation match {
        case ValidateUntilFirst => validateUntilFirst(user) match {
          case Right(user) => Right(user)
          case Left(error) => Left(List(error))
        }
        case ValidateSeq => validateSeq(user)
        case ValidateParallel => Await.result(validateParallel(user), Duration.Inf)
      }
    }

    def validateUntilFirst(user: User): Either[String, User] = {
      for {
        _ <- validateName(user.name)
        _ <- validateAge(user.age)
        _ <- validateEmail(user.email)
        _ <- validateSexVsHeight(user.sex, user.height)
      } yield user
    }

    def validateSeq(user: User): Either[List[String], User] = {
      val validationResults = List(validateName(user.name), validateAge(user.age), validateEmail(user.email), validateSexVsHeight(user.sex, user.height))
        .filter(_.isLeft)
      validationResults match {
        case List() => Right(user)
        case errors => Left(errors.flatMap(_.left.toOption))
      }
    }

    def validateParallel(user: User): Future[Either[List[String], User]] = {
      val validateNameFuture = Future(validateName(user.name))
      val validateAgeFuture = Future(validateAge(user.age))
      val validateEmailFuture = Future(validateEmail(user.email))
      val validateSexVsHeightFuture = Future(validateSexVsHeight(user.sex, user.height))

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
            case List() => Future(Right(user))
            case errors => Future(Left(errors.flatMap(_.left.toOption)))
          }
        case Failure(error) => Future.failed(error)
      }
    }

    private val nameRegex = "^[a-zA-Z]+$".r

    private def validateName(name: String): Either[String, String] = {
      name match {
        case nameRegex(_*) => Right(name)
        case _ => Left("Name should only consist of Latin characters")
      }
    }

    private def validateAge(age: Int): Either[String, Int] = {
      age match {
        case x if x <= 0 => Left("Age must be greater than zero")
        case x if x >= 100 => Left("Age must be less than 100")
        case _ => Right(age)
      }
    }

    //W3C recommendation: http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
    private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    private def validateEmail(email: String): Either[String, String] = {
      email match {
        case null => Left("Email cannot be null")
        case e if e.isEmpty => Right(email) // empty emails are allowed by problem statement
        case e if e.trim.isEmpty => Left("Email cannot consist of whitespace")
        case e if emailRegex.findFirstMatchIn(e).isDefined => Right(email)
        case _ => Left("Email is in an incorrect/non-standard format")
      }
    }

    private def validateSexVsHeight(sex: Sex, height: Double): Either[String, (Sex, Double)] = {
      sex match {
        case Female => Right(sex, height)
        case Male =>
          height match {
            case x if x <= 100 => Left("Male should have height greater than 100")
            case _ => Right(sex, height)
          }
      }
    }
  }
}

  case class User(name: String, age: Int, email: String, sex: Sex, height: Double)

  sealed trait Sex {
    def name: String
  }

  case object Male extends Sex {
    val name = "Male"
  }

  case object Female extends Sex {
    val name = "Female"
  }

  sealed trait ValidationOption {
    def name: String
  }

  case object ValidateUntilFirst extends ValidationOption {
    val name = "ValidateUntilFirst"
  }

  case object ValidateSeq extends ValidationOption {
    val name = "ValidateSeq"
  }

  case object ValidateParallel extends ValidationOption {
    val name = "ValidateParallel"
  }
