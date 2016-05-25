object ArnoldC {
  def main(args: Array[String]){
    var p = new ArnoldCParser
    args.foreach { filename =>
      var src = scala.io.Source.fromFile(filename)
      p.eval(src.mkString)
    }
  }
}
