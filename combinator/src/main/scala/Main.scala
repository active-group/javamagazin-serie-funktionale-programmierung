object LoanBot {
  enum Rule[A] {
    case Predicate(name: String, predicate: A => Boolean)
    case And(left: Rule[A], right: Rule[A])
    case Or(left: Rule[A], right: Rule[A])
    case EmptyAnd()

    // Returns whether the input A abides by the rules.
    def evaluate(input: A): Boolean =
      this match {
        case Predicate(_, p) => p(input)
        case And(left, right) => left.evaluate(input) && right.evaluate(input)
        case Or(left, right) => left.evaluate(input) || right.evaluate(input)
        case EmptyAnd() => true
      }

    // Returns a string describing the rules.
    def describe(): String =
      this match {
        case Predicate(name, _) => name
        case And(left, right) => "(" + left.describe() + " UND " + right.describe() + ")"
        case Or(left, right) => "(" + left.describe() + " ODER " + right.describe() + ")"
        case EmptyAnd() => ""
      }

    // Returns a list of descriptions of all requirements that are not met.
    def missingRequirements(input: A): List[String] =
      this match {
        case Predicate(name, p) => if p(input) then List.empty else List(name)
        case And(left, right) => left.missingRequirements(input).appendedAll(right.missingRequirements(input))
        case Or(left, right) => left.missingRequirements(input).appendedAll(right.missingRequirements(input))
        case EmptyAnd() => List.empty
      }
  }

  // A person that might want a loan
  case class Person(age: Int, equity: Double, income: Double, employed: Boolean)
  
  import Rule._

  // Um einen Kredit zu bekommen, grundsätzlich
  // - muss die Person mindestens 18 Jahre alt sein und angestellt sein, wenn der Betrag unter 5000€ ist
  // - muss die Person mindestens 23 Jahre alt sein und angestellt sein, wenn der Betrag über 5000€ ist
  // - entweder ein Einkommen von mehr als 60000€ und 10% Eigenkapital haben
  // - oder ein Eigenkapital von mindestens 20% der Vertragssumme und ein Einkommen von mehr als 45000€ haben
  def incomeRule(income: Double): Rule[Person] =
    Rule.Predicate("miniales Einkommen: " + income, _.income >= income)

  def equityRule(equity: Double): Rule[Person] =
    Rule.Predicate("minimales Eigenkapital: " + equity, _.equity >= equity)

  def employedRule: Rule[Person] =
    Rule.Predicate("muss angestellt sein", _.employed)

  def minimumAgeRule(age: Int): Rule[Person] =
    Rule.Predicate("Mindestalter: " + age + " Jahre", _.age >= age)

  def baselineRule(age: Int): Rule[Person] =
    Rule.And(minimumAgeRule(age), employedRule)

  def highIncomeLowEquity(loanAmount: Double): Rule[Person] =
    Rule.And(incomeRule(60000.0), equityRule(loanAmount * 0.1))

  def highEquityLowIncome(loanAmount: Double): Rule[Person] =
    Rule.And(incomeRule(45000.0), equityRule(loanAmount * 0.25))

  def equityIncomeRule(loanAmount: Double): Rule[Person] =
    Rule.Or(highEquityLowIncome(loanAmount), highIncomeLowEquity(loanAmount))

  def getsLoanRule(loanAmount: Double): Rule[Person] =
    Rule.And(if loanAmount > 5000 then baselineRule(23) else baselineRule(18), equityIncomeRule(loanAmount))

  // Die Mathematik dahinter - Implementierung von cats.Monoid
  // Dazu haben wir die Datendefinition um `EmptyAnd` oben überall erweitert
  import cats.Monoid

  given rulesAndMonoid: Monoid[Rule[Person]] with {
    def empty: Rule[Person] = EmptyAnd()
    def combine(x: Rule[Person], y: Rule[Person]): Rule[Person] = And(x, y)
  }

  // Unten stehendes, allgemeines `combineAll` ersetzt nun `manyAnds`
  // def manyAnds(rules: Seq[Rule[Person]]): Rule[Person] =
  //   rules.fold(EmptyAnd())((rule, otherRule) => And(rule, otherRule))

  def combineAll[A: Monoid](as: List[A]): A =
    as.foldLeft(Monoid[A].empty)(Monoid[A].combine)
}

@main def main(): Unit =
  val marco = LoanBot.Person(35, 30000.0, 45000.0, true)
  val mimi = LoanBot.Person(4, 0.0, 0.0, false)
  println(LoanBot.getsLoanRule(100000.0).evaluate(marco))
  println(LoanBot.getsLoanRule(100000.0).evaluate(mimi))
  println(LoanBot.getsLoanRule(200000.0).describe())
  println(LoanBot.getsLoanRule(2000.0).describe())
  println(LoanBot.getsLoanRule(100000.0).missingRequirements(marco))
  println(LoanBot.getsLoanRule(100000.0).missingRequirements(mimi))