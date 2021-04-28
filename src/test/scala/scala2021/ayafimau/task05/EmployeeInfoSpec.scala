package scala2021.ayafimau.task05

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import scala2021.ayafimau.task05.Main._

class EmployeeInfoSpec extends AsyncFlatSpec with TableDrivenPropertyChecks with BeforeAndAfterAll {

  private val employeeToManager =
    Table(
      ("employee", "manager"),
      ("John", None),
      ("Steve", Some("Steve")),
      ("Mark", Some("Steve")),
      ("Igor", Some("Igor")),
      ("Christy", None),
      ("Naveen", None),
      ("Megan", None)
    )

  private val employeeToManagerOrError =
    Table(
      ("employee", "managerOrError"),
      ("John", Left("Employee with name John is not present in the repository")),
      ("Steve", Right("Steve")),
      ("Mark", Right("Steve")),
      ("Igor", Right("Igor")),
      ("Christy", Left("Department with id 5 (for employee Christy) is not present in the repository")),
      ("Naveen", Left("Employee with id 14 who should be manager for department IT is not present in the repository")),
      ("Megan", Left("Manager for department Research (department id =  3) is not present in the repository"))
    )

  override def beforeAll() {
    // Arrange
    Main.main(Array())
  }

  "findManagerName" should "return expected values" in {
    // Act & Assert
    forAll(employeeToManager) { (employee: String, manager: Option[String]) =>
      assert(findManagerName(employee) === manager)
    }
  }

  "findManagerNameOrError" should "return expected values" in {
    // Act & Assert
    forAll(employeeToManagerOrError) { (employee: String, managerOrError: Either[String, String]) =>
      assert(findManagerNameOrError(employee) === managerOrError)
    }
  }

  "findManagerNameOrErrorAsync" should "return expected values" in {
    // Act & Assert
    forAll(employeeToManagerOrError) { (employee: String, managerOrError: Either[String, String]) =>
      findManagerNameOrErrorAsync(employee) map { result => assert(result === managerOrError) }
    }
  }

  "findEmployeeManagers" should "return all expected values" in {
    val notFound = "Not Found"
    // Arrange
    val expected = List(
      Info("Steve","Marketing","Steve"),
        Info("Mark","Marketing","Steve"),
        Info("Jane","Marketing","Steve"),
        Info("Samuel","Sales","Igor"),
        Info("Igor","Sales","Igor"),
        Info("Naveen","IT",notFound),
        Info("Christy",notFound,notFound),
        Info("Megan","Research",notFound)
    )
    // Act & Assert
    assert(findEmployeeManagers === expected)
  }
}
