package interview

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object Main extends App {
  override def main(args: Array[String]) {
    var ok = true
    var input = ""
    while (ok) {
      val ln = readLine()
      ok = ln != null
      if (ok) input = input + ln + "\n"
    }

    val batch = BDIParser.parseAll(BDIParser.bdi, input).get
    val batchBalance = Transact(batch)
    println(ToJson(batchBalance))
  }
}

object Operation extends Enumeration {
  type Operation = Value
  val Credit, Debit = Value
}
import Operation._

object BDIParser extends RegexParsers {
  override def skipWhitespace = true

  def header: Parser[Any] = """.*BDI.*""".r ^^ { a => a }
  def description: Parser[String] = """\w+(\s+\w+)*""".r ^^ { a => a }
  def number: Parser[Long] = """\d+""".r ^^ { _.toLong }
  def account: Parser[Account] = number ~ ("/" ~> number) ^^ {
    case (a ~ b) => Account(a, b)
  }
  def operation: Parser[Operation] = """(Credit|Debit)""".r ^^ {
    _ match {
      case "Credit" => Credit
      case "Debit" => Debit
    }
  }
  def transaction: Parser[Transaction] =
    ("Transaction:" ~> number) ~
    ("Originator:" ~> account) ~
    ("Recipient:" ~> account) ~
    ("Type:" ~> operation) ~
    ("Amount:" ~> number) ^^ {
      case (id ~ originator ~ recipient ~ operation ~ amount) =>
        Transaction(id, originator, recipient, operation, amount)
    }

  def bdi: Parser[Batch] =
    header ~
    ("Batch:" ~> number) ~
    ("Description:" ~> description) ~
    rep("==" ~> transaction) ^^ {
      case (header ~ id ~ description ~ transactions) => Batch(id, description, transactions)
    }
}

object Transact {
  def apply(batch: Batch): BatchBalance = {
    val balances: List[Balance] = batch.transactions.foldLeft(List[Balance]()) { (balances, tr) =>
      val originator: Balance = balances
        .find(_.account == tr.originator)
        .getOrElse(Balance(tr.originator, 0))

      val updatedOriginator = originator.copy(balance = originator.balance + tr.signedAmount)

      val recipient: Balance = balances
        .find(_.account == tr.recipient)
        .getOrElse(Balance(tr.recipient, 0))

      val updatedRecipient = recipient.copy(balance = recipient.balance - tr.signedAmount)

      val filteredBalances = balances.filter(b => {
        b.account != originator.account && b.account != recipient.account
      })

      updatedOriginator :: updatedRecipient :: filteredBalances
    }
    BatchBalance(batch.id, batch.description, balances)
  }
}

object ToJson {
  import org.json4s._
  import org.json4s.jackson.Serialization

  private implicit val formats = Serialization.formats(NoTypeHints)

  def apply(batchBalance: BatchBalance, pretty: Boolean = true): String = {
    pretty match {
      case true => Serialization.writePretty(batchBalance)
      case false => Serialization.write(batchBalance)
    }
  }
}

case class Account(
  number: Long,
  routingNumber: Long
)

case class Transaction(
  id: Long,
  originator: Account,
  recipient: Account,
  operation: Operation,
  amount: Long
) {
  def signedAmount = operation match {
    case Debit => amount
    case Credit => -amount
  }
}

case class Batch(
  id: Long,
  description: String,
  transactions: List[Transaction]
)

case class Balance(
  account: Account,
  balance: Long
)


case class BatchBalance(
  id: Long,
  description: String,
  balances: List[Balance]
)
