package com.github.mideo
package inputs

object MonkeyInTheMiddleInput {
   val value =
     """
       |Monkey 0:
       |  Starting items: 98, 89, 52
       |  Operation: new = old * 2
       |  Test: divisible by 5
       |    If true: throw to monkey 6
       |    If false: throw to monkey 1
       |
       |Monkey 1:
       |  Starting items: 57, 95, 80, 92, 57, 78
       |  Operation: new = old * 13
       |  Test: divisible by 2
       |    If true: throw to monkey 2
       |    If false: throw to monkey 6
       |
       |Monkey 2:
       |  Starting items: 82, 74, 97, 75, 51, 92, 83
       |  Operation: new = old + 5
       |  Test: divisible by 19
       |    If true: throw to monkey 7
       |    If false: throw to monkey 5
       |
       |Monkey 3:
       |  Starting items: 97, 88, 51, 68, 76
       |  Operation: new = old + 6
       |  Test: divisible by 7
       |    If true: throw to monkey 0
       |    If false: throw to monkey 4
       |
       |Monkey 4:
       |  Starting items: 63
       |  Operation: new = old + 1
       |  Test: divisible by 17
       |    If true: throw to monkey 0
       |    If false: throw to monkey 1
       |
       |Monkey 5:
       |  Starting items: 94, 91, 51, 63
       |  Operation: new = old + 4
       |  Test: divisible by 13
       |    If true: throw to monkey 4
       |    If false: throw to monkey 3
       |
       |Monkey 6:
       |  Starting items: 61, 54, 94, 71, 74, 68, 98, 83
       |  Operation: new = old + 2
       |  Test: divisible by 3
       |    If true: throw to monkey 2
       |    If false: throw to monkey 7
       |
       |Monkey 7:
       |  Starting items: 90, 56
       |  Operation: new = old * old
       |  Test: divisible by 11
       |    If true: throw to monkey 3
       |    If false: throw to monkey 5
       |""".stripMargin
}
