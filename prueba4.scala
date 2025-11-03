//> using scala "2.13.12"
//> using dep "com.lihaoyi::upickle:4.4.0"
//> using dep "com.lihaoyi::os-lib:0.11.5"


import scala.io.StdIn
import upickle.default.{write => uWrite, read => uRead, _}
import os.{read => osRead, write => osWrite, _}

object prueba4 { //prueba3 + json

    sealed trait Estado
    case object Pendiente extends Estado
    case object EnProgreso extends Estado
    case object Completado extends Estado    
    
    case class Tarea(id: Int, titulo: String, estado: Estado)

    // ======== SERIALIZACIÓN JSON ==========
    implicit val estadoRW: ReadWriter[Estado] = readwriter[String].bimap[Estado](
        {
            case Pendiente   => "Pendiente"
            case EnProgreso  => "En Progreso"
            case Completado  => "Completado"
        },
        {
            case "Pendiente"   => Pendiente
            case "En Progreso" => EnProgreso
            case "Completado"  => Completado
            case _ => Pendiente
        }
    )
    implicit val tareaRW: ReadWriter[Tarea] = macroRW[Tarea]

    // ========= FUNCIONES DE PERSISTENCIA ===========
    val archivoTareas = os.pwd / "tareasPruebaScala.json"

    def guardarTareas(tareas: List[Tarea]): Unit = {
        val json = uWrite(tareas, indent = 2)
        osWrite.over(archivoTareas, json)
    }
    def cargarTareas(): List[Tarea] = {
        if (os.exists(archivoTareas)) {
            uRead[List[Tarea]](osRead(archivoTareas))
        }else {
            List.empty
        }
    }

    // ========= FUNCIONES DE UTILIDAD
    def mostrarMenu(): Unit = {
        println( "|------ Gestor de Tareas ------|")
        println("| 1. Añadir Tareas")
        println("| 2. Mostrar Tareas y su estado")
        println("| 3. Actualizar estado de tarea")
        println("| 4. Salir")
        println("|  seleccione su opcion: ")
    }
    def mostrarTareas(tareas: List[Tarea]): Unit = {
        if (tareas.isEmpty) println("No hay tareas disponibles")
        else tareas.foreach(t => println(s"Tarea ${t.id} - ${t.titulo} => ${parseEstadoToString(t.estado)}"))
    }
    def mostrarTituloTareas(tareas: List[Tarea]): Unit = {
        if (tareas.isEmpty) println("No hay tareas disponibles")
        else tareas.foreach(t => println(s"Tarea ${t.id} - ${t.titulo} "))
    }
    def parseEstado(s:String): Estado = {
        s.toLowerCase match {
            case "pendiente" | "1" => Pendiente
            case "en progreso" | "2" => EnProgreso
            case "completado" | "3" => Completado
            case _ => 
                println("Estado no reconocido, PENDIENTE por defecto ")
                Pendiente
        }
    }
    def parseEstadoToString(e: Estado): String = {
        e match{
            case Pendiente => "Pendiente"
            case EnProgreso => "En Progreso"
            case Completado => "Completado"
        }
    }

    def main( args: Array[String]) : Unit = {

        var tareas : List[Tarea] = cargarTareas()
        var siguienteId = if (tareas.isEmpty) 1 else tareas.map(_.id).max +1
        var salir = false 
        
        while (!salir){
            mostrarMenu()
            StdIn.readLine() match {
                case "1" => 
                    println("Introduce el título de la nueva tarea") 
                    val titulo = StdIn.readLine()
                    if (tareas.exists(_.titulo.equalsIgnoreCase(titulo))) println("Ya existe una tarea con ese título")
                    else {
                        tareas = tareas :+ Tarea(siguienteId, titulo, Pendiente) 
                        siguienteId += 1
                    }
                case "2" => 
                    println("Filtrar por: \n 1. Estado \n 2. Titulo \n 3. Todas")
                    StdIn.readLine() match {
                        case "1" => 
                            println(" 1. Pendiente ⏰")
                            println(" 2. En progreso ⚙️")
                            println(" 3. Completado ✅")
                            var estadoRaw = StdIn.readLine()
                            var estado: Estado = parseEstado(estadoRaw)
                            mostrarTareas(tareas.filter(_.estado == estado))
                        case "2" => 
                            mostrarTituloTareas(tareas)
                            println(" Introduce el TITULO de tu tarea")
                            var titulo = StdIn.readLine()
                            mostrarTareas(tareas.filter(_.titulo == titulo))
                        case "3" => mostrarTareas(tareas)
                        case _ => println(" 🤫 ☝️🤓☝️Haber esudiado")
                    }
                case "3" => 
                    mostrarTituloTareas(tareas)
                    println("Buscar por ID o TITULO? (1-id/2-titulo)")
                    var buscarOp: Option[Tarea] = StdIn.readLine().toLowerCase() match {
                        case "id" | "1" => 
                            println("Introduce el ID de la tarea")
                            var id = StdIn.readLine().toInt
                            tareas.find(_.id == id) 
                        case "titulo" | "2" =>
                            println("Introduce el TITULO de la tarea")
                            var t = StdIn.readLine()
                            tareas.find(_.titulo.toLowerCase == t.toLowerCase) 
                        case _=> 
                            println("No reconocido")
                            None
                    }
                    buscarOp match {
                        case Some(tarea) => 
                            mostrarTareas(tareas.filter(_.id == tarea.id))
                            println(" 1. Pendiente ⏰")
                            println(" 2. En progreso ⚙️")
                            println(" 3. Completado ✅")
                            var eRaw = StdIn.readLine()
                            var e = parseEstado(eRaw)
                            tareas = tareas.map{
                                case t if t.id == tarea.id => t.copy(estado = e)
                                case t => t
                            }
                            println(s"Tarea ${tarea.titulo} ha sido actualizada a estado: ${parseEstadoToString(e)}")
                        case None => println("No se encontro la tarea")
                        case null => println("No se kapachao")
                    }
                case "4" =>
                    guardarTareas(tareas)
                    salir =true
                case _ => println("Opcion inválida, pedazo de listo")
            }
        }
    }
}

