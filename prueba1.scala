import scala.io.StdIn // You can import the whole object


object prueba1 {

    def main(args: Array[String]): Unit = {
        var prueba = true

        while (prueba) {
            println("Introduce el n1: ")
            var n1 = StdIn.readInt() 
            println("Introduce el n2: ")
            var n2 = StdIn.readInt()
            println(s"Tus numeros son $n1 y $n2")
            println( "Selecciona el operando: \n <1>-- + \n <2>-- -\n <3>-- *\n <4>-- /")
            var operando = StdIn.readChar() // Reads a char

            val n3 = operando match{
            case '+' | '1' => 
                operando = '+'
                n1 + n2;
            case '-' | '2' => 
                operando = '-'
                n1 - n2;
            case '*' | '3' => 
                operando = '*'
                n1 * n2;
            case '/' | '4' => 
                operando = '/'
                if (n2 != 0)  {
                    n1/ n2
                } else 0
            case _ => 0;
            }
            if (operando == '/' && n2 == 0 && n3 == 0) println("Excepcion division por 0")
            println(s"Has introducido $n1 $operando $n2 = $n3")
            println("¿seguimos? (s/n)")
            prueba = StdIn.readLine().toLowerCase() == "s"
        
        }

    }
}