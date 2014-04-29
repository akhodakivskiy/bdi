package interview.test

import interview._
import interview.Operation._

import org.scalatest._
import scala.io.Source

class BDITest extends FlatSpec with Matchers {

  val a1 = Account(1, 1)
  val a2 = Account(2, 2)
  val a3 = Account(3, 3)

  val transactions = List(
    Transaction(1, a1, a2, Credit, 100),
    Transaction(2, a2, a3, Debit, 200)
  )

  val batch = Batch(100, "Some Batch", transactions)

  "Account" should "be comparable" in {
    val a1 = Account(1, 2)
    val a2 = Account(1, 2)
    val a3 = Account(3, 4)

    assert(a1 == a2)
    assert(a1 != a3)
  }

  "Transactor" should "process a Batch" in {
    val batchBalance = Transact(batch)

    batchBalance.id should be (batch.id)
    batchBalance.description should be (batch.description)
    batchBalance.balances should have length 3

    batchBalance.balances.find(_.account == a1).map(_.balance) should be (Some(-100))
    batchBalance.balances.find(_.account == a2).map(_.balance) should be (Some(300))
    batchBalance.balances.find(_.account == a3).map(_.balance) should be (Some(-200))
    batchBalance.balances.foldLeft(0: Long)((total, b) => total + b.balance) should be (0)
  }

  "Serializer" should "produce proper json" in {
    val batchBalance = Transact(batch)

    val json = ToJson(batchBalance)
    json should include ("-100")
    json should include ("200")
  }

  "BDIParser" should "parse BDI format" in {
    BDIParser.parseAll(BDIParser.description, "asdf 12").get should equal ("asdf 12")
    BDIParser.parseAll(BDIParser.account, "23 / 123").get should equal (Account(23, 123))
    BDIParser.parseAll(BDIParser.operation, "Debit").get should equal (Debit)
    BDIParser.parseAll(BDIParser.operation, "Credit").get should equal (Credit)
    val bdiTransaction =
      """|Transaction: 302
         |Originator: 111222333 / 9991
         |Recipient: 123456789 / 55550
         |Type: Credit
         |Amount: 380100
         |""".stripMargin
    val transaction = Transaction(
      302,
      Account(111222333, 9991),
      Account(123456789, 55550),
      Credit, 380100
    )
    BDIParser.parseAll(BDIParser.transaction, bdiTransaction).get should equal (transaction)

    val bdi = Source.fromURL(getClass.getResource("/bdi.txt")).mkString
    val batch = BDIParser.parseAll(BDIParser.bdi, bdi) match {
      case BDIParser.Success(batch, _) => {
        batch.id should be (99)
        batch.description should be ("Payroll for January")

        val batchBalances = Transact(batch)
        batchBalances.balances should have length 5
      }
      case _ => assert(false)
    }

  }
}
