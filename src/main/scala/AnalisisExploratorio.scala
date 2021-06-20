import scala.math.BigDecimal

object AnalisisExploratorio extends App {

  val dataset = Utilidades.readFile(fichero = "src/adult.data.clean.csv")
  // Implementa la función
  // ejercicio-1:
  // Número total de registros en el dataset.
  def totalDeRegistros(c: Seq[Contribuyente]): Int = {
    val n_registros = c.length
    n_registros
  }

  // Implementa la función
  // ejercicio-2:
  // Calcular la media de edad de todos los contribuyentes
  def calculaEdadMedia(c: Seq[Contribuyente]): Double = {
    val edades: Seq[Double] = c.map(_.age)
    val edad_media: Double = edades.sum / edades.length
    BigDecimal(edad_media).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  // Implementa la función
  // ejercicio-3:
  // Calcular la media de edad de todos los contribuyentes sin contar aquellos cuya edad sea 0
  def calculaEdadMediaNoZeros(c: Seq[Contribuyente]): Double = {
    val edades: Seq[Double] = c.map(_.age)
    val edades_nz = edades.filterNot(_ == 0)
    val edad_media_nz: Double = edades_nz.sum / edades_nz.length
    BigDecimal(edad_media_nz).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  // Implementa la función
  // ejercicio-4:
  // Descubrir de cuántos países distintos provienen los contribuyentes
  def paisesOrigenUnicos(c: Seq[Contribuyente]): Seq[String] = {
    val paises: Seq[String] = c.map(_.nativeCountry).distinct
    paises
  }

  // Implementa la función
  // ejercicio-5:
  // De todos los contribuyentes, ¿cómo se distribuye por género?. Devuelve el porcentaje de hombres
  // y el de mujeres en ese orde, (porcentajeDeHombres, porcentajeDeMujeres) //Unit ->(Double, Double)
  def distribucionPorGeneros(c: Seq[Contribuyente]): (Double, Double) = {
    val n_total: Double = c.length
    val n_hombres: Double = c.map(_.sex).filter(x=>x.equals("Male")).length
    val n_mujeres: Double = c.map(_.sex).filter(x=>x.equals("Female")).length
    val per_h: Double = BigDecimal(n_hombres/n_total).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    val per_m: Double = BigDecimal(n_mujeres/n_total).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    (per_h, per_m)
  }

  // Implementa la función
  // ejercicio-6:
  // Cuál es el tipo de trabajo (workclass) cuyos ingresos son mayoritariamente superiores a ">50K"
  def trabajoMejorRemunerado(c: Seq[Contribuyente]): String = {
    val work_classes: Seq[String] = c.map(_.workclass).distinct // listado de workclasses
    var high_income_wc: String = "_" //inicializar variable resultado
    for(work_class <- work_classes){
      //para cada work_class, se calcula cuantas personas ingresan más y menos de 50k y se compara
      val wc_c = c.filter(_.workclass.equals(work_class))
      val mayor_50: Int = wc_c.filter(_.income.equals(">50K")).length
      val menor_50: Int = wc_c.filter(_.income.equals("<=50K")).length
      if (mayor_50 > menor_50) {
        high_income_wc = work_class
      }
    }
    high_income_wc
  }

  // Implementa la función
  // ejercicio-7:
  // Cuál es la media de años de educación (education-num) de aquellos contribuyentes cuyo país de origen no es
  // United-States Unit --> Double
  def aniosEstudiosMedio(c: Seq[Contribuyente]): Double = {
    val c_no_US = c.filterNot(_.nativeCountry.equals("United-States"))
    val n: Double = c_no_US.length
    val suma_edu: Double = c_no_US.map(_.educationNum).sum
    BigDecimal(suma_edu/n).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


  // println(s" -> Dataset tiene un total de registros: ${totalDeRegistros(c = dataset)}")
  // println(s" -> En el dataset, los contribuyentes tienen una edad media: ${calculaEdadMedia(c = dataset)}")
  // println(s" -> En el dataset, los contribuyentes tienen una edad media (sin contar aquellos con age = 0): ${calculaEdadMediaNoZeros(c = dataset)}")
  // println(s" -> Los contribuyentes proviende de distintos países como: ${paisesOrigenUnicos(c = dataset).foreach(println)}")
  // println(s" -> Los contribuyentes se distribuyen en (hombres - mujeres): ${distribucionPorGeneros(c = dataset)}")
  // println(s" -> El tipo de trabajo mejor remunerado en el dataset es: ${trabajoMejorRemunerado(c = dataset)}")
  // println(s" -> La media de años de estudio de los contribuyenes de origen distinto a United States es: ${aniosEstudiosMedio(c = dataset)}")

  // ejercicio-11
  // llama a la función impimeContribuyentes

}
