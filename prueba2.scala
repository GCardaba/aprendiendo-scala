import scala.util.Random
import scala.io.StdIn // You can import the whole object
//import scala.util.{Try, Success, Failure}


object MayorMenorJuego {
    def main(args: Array[String]): Unit = {
        val nivel: Int = printMenu()
        
        val (max, tryis) = nivel match{
            case 1 => (10, 5)
            case 2 => (50, 7)
            case 3 => (100, 10)
            case _ => 
                println("Haber escrito algo en el rango, zopenco!")
                (0,0)
        }
        juego(max, tryis)  

    }
    def printMenu(): Int  = {
        var nivel = 0
        while(nivel < 1 || nivel > 3) {
        
            println("Selecciona la dificultad: \n <1> Facil: 1-10 (5 tryis) \n <2> Facil: 1-50 (7 tryis) \n <3> Facil: 1-100 (10 tryis)")
            try {
                nivel = StdIn.readInt()
                if (nivel < 1 || nivel > 3) println("Opcion invalida, el numero esta fuera del rango")

            } catch {
                case e: NumberFormatException => 
                    println("Error: Debes introducir un número entero")
                    StdIn.readLine()
                case e: Exception => 
                    println(s"Error inesperado: ${e.getMessage}")
                    StdIn.readLine()
            }
        }
        nivel // Valor que se retorna
    }
    def juego(max: Int, tryis: Int ) = {
        /*
        var n: scala.util.Random = new scala.util.Random.between(0, max: Int)
        */
        val random = new Random();
        val n = random.nextInt(max) + 1
        println(s"max$max, n$n")
        var ganado = false
        var intentos = tryis
        while (intentos > 0 && !ganado){
            println("Introduce tu numero: ")
            try {
                var nUsuario = StdIn.readInt();
                if (nUsuario == n) {
                    println(s"Has ganado! El numero era: $n, lo has conseguido en ${tryis - intentos} intentos")
                    ganado = true
                }else if(nUsuario < n) println("TU numero es MENOR que el numero secreto!")
                else println("TU numero es MAYOR que el numero secreto!")

                println(s"Te quedan $intentos intentos")

                intentos -= 1
            } catch {
                case e: NumberFormatException => 
                    println("Error: Debes introducir un número entero")
                    StdIn.readLine()
                case e: Exception => 
                    println(s"Error inesperado: ${e.getMessage}")
                    StdIn.readLine()
            }
        }
        if (!ganado) println(s"GAME OVER! El numero era: $n")
    }
}