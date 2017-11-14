object Main {
    private val size = 5

    def main(args: Array[String]): Unit = {
        val list = Array.ofDim[Array[Boolean]](size)
        initialize(list)
        lifecycle(list.flatten)
    }

    def initialize(list: Array[Array[Boolean]]): Unit = {
        for (i <- list.indices) {
            list(i) = Array.fill[Boolean](size)(false)
        }
        list(0)(0) = true
        list(1)(0) = true
        list(0)(1) = true
        list(1)(1) = true
    }

    def lifecycle(list: Array[Boolean]): Array[Boolean] = {
        show(list)
        if (list.contains(true)) {
            lifecycle(list.zipWithIndex.map { d =>
                check(list, d._2)
            })
        } else {
            Array.empty[Boolean]
        }
    }

    def check(list: Array[Boolean], now: Int): Boolean = {
        val surroundings = Array(-size - 1, -size, -size + 1, -1, 1, size - 1, size, size + 1)
        val length = surroundings.map { s =>
            if (0 > s + now || list.length < s + now) {
                false
            } else {
                true
            }
        }.count(_ == true)

        if (list(now)) {
            length match {
                case 2 | 3 => true
                case _ => false
            }
        } else {
            length == 3
        }
    }

    def show(list: Array[Boolean]): Unit = {
        for ((n, i) <- list.zipWithIndex) {
            printf("%s ", if (n) {
                "■"
            } else {
                "□"
            })
            if ((i + 1) % size == 0) {
                println
            }
        }
        println
    }
}