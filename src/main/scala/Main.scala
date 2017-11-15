object Main {
    private val size = 6

    def main(args: Array[String]): Unit = {
        val list = Array.ofDim[Array[Boolean]](size)
        initialize(list)
        lifecycle(list.flatten)
    }

    def initialize(list: Array[Array[Boolean]]): Unit = {
        for (i <- list.indices) {
            list(i) = Array.fill[Boolean](size)(false)
        }
        list(2)(2) = true
        list(3)(2) = true
        list(2)(3) = true
        list(3)(3) = true
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
        val surroundings = Array(
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1)
        )
        val checkSurround: ((Int, Int)) => Boolean = (s) => {
            val index = (now / 5) + s._1
            val row = (now % 5) + s._2

            if (index < 0 || index >= size || row < 0 || row >= size) {
                false
            } else {
                list(size * index + row)
            }
        }
        val length = surroundings.map { checkSurround }.count(_ == true)

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