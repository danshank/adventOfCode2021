package Day13

def partOne(): Int =
    val binaryString = "100011111"
    val a = Integer.parseInt(binaryString, 2)
    val b = 2 * a + 1
    val c = 3 * a + 1
    val d = (3 * a + 1) / 2
    println("-------------------------------")
    println(s"  ${addPadding(a)}")
    println(s"+ ${addPadding(b)}")
    println("-------------------------------")
    println(s"  ${addPadding(c)}")
    println(s"  <--")
    println(s"  ${addPadding(d)}")
    println(s"  ${addPadding(a)}")
    println("-------------------------------")
    1    

def addPadding(x: Int): String =
    "0000000000000000" + x.toBinaryString takeRight 16