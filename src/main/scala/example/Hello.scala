package example

import jvmylib._

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

case class Point(x: Int, y:Int)
case class Colour(r: Int, g:Int, b:Int)
case class TopPanel(LPoint: Point, RPoint: Point)
case class Panel(LPoint: Point, RPoint: Point, BPoint: Point)

object Hello extends Greeting with App {

  def getColour(img: BufferedImage, x: Int, y: Int): Colour = {
    val warna = img.getRGB(x, y)
    val blue = (warna & 0xff)
    val green = (warna & 0xff00) >> 8
    val red = (warna & 0xff0000) >> 16
    val nil = new Colour(red, green, blue)
    nil
  }

  def isBlack(x: Colour): Boolean = {
      val black = new Colour(10, 10, 10)
      return (x.r < black.r) && (x.g < black.g) && (x.b < black.b)
  }

  def isWhite(x: Colour): Boolean = {
      val white = new Colour(250, 250, 250)
      return (x.r > white.r) && (x.g > white.g) && (x.b > white.b)
  }

  def getFirstEdge(img: BufferedImage, w: Int): Point = {
    var xtemp = 0
    var ytemp = 0
    var x = 0
    var temp = 0

    for(x<-0 until w if(xtemp == 0)){
      if( isBlack(getColour(img, x, x) ) ){
        // println(x)
        xtemp = x
        ytemp = x
      }
    }

    temp = xtemp
    for(x<-xtemp to (0,-1) ){
      if( isBlack(getColour(img, x, ytemp) ) ){
        // println(x)
        xtemp = x
      }
    }

    temp = ytemp
    for(y<-ytemp to (0,-1) ){
      if( isBlack(getColour(img, xtemp, y) ) ){
        // println(y)
        ytemp = y
      }
    }

    val point = Point(xtemp, ytemp)
    point
  }

  def getTopPanel(img: BufferedImage, w: Int, point: Point) = {
    var toplist : List[TopPanel] = List()
    var margin = 0
    var prevmargin = 0
    var startP = 0

    for(x<-point.x until w){
      if( isBlack( getColour(img, x, point.y) ) ){
        if( isWhite( getColour(img, x-1, point.y) ) ){
          startP = x;
        }else{
          // ...
        }
      }
      else if( isWhite( getColour(img, x, point.y) ) ){
        if( isBlack(getColour(img, x-1, point.y) ) ){
          toplist = TopPanel( Point(startP, point.y), Point(x-1, point.y) )::toplist
          prevmargin = margin
          margin = 0
        }else{
          margin += 1
        }
      }
    }

    margin = prevmargin
    (toplist, margin)
  }

  def getListPanel(img: BufferedImage, toplist: List[TopPanel], h: Int) = {
    var leftpanel : List[Panel] = List()
    var rightpanel : List[Panel] = List()
    var startP = 0
    var el = 0

    toplist.reverse.foreach((i:TopPanel)=>{
      for(y<-point.y until h){
        if( isBlack( getColour(img, i.LPoint.x, y) ) && isWhite( getColour(img, i.LPoint.x, y-1) ) ){
          startP = y;
        }
        if( isWhite( getColour(img, i.LPoint.x, y) ) && isBlack( getColour(img, i.LPoint.x, y-1) ) ){
          if(el==0){
            leftpanel = Panel( 
              Point(i.LPoint.x, startP),
              Point(i.RPoint.x, startP),
              Point(i.LPoint.x, y-1)
            )::leftpanel
          }else{
            rightpanel = Panel( 
              Point(i.LPoint.x, startP),
              Point(i.RPoint.x, startP),
              Point(i.LPoint.x, y-1)
            )::rightpanel
          }
        }
      }
      el += 1
    })

    var totpanel = rightpanel:::leftpanel
    totpanel
  }

  def cetakAll(totpanel: List[Panel], img: BufferedImage, margin: Int, atasbawah: Int) = {
    var numbering = 0
    var numbering2 = 0
    var newW = 0
    var newH = 0

    totpanel.reverse.foreach((i:Panel)=>{
      newW = i.RPoint.x - i.LPoint.x + 1 + (margin*2)
      newH = i.BPoint.y - i.LPoint.y + 1 + (atasbawah*2)
      var output = new BufferedImage(newW, newH, BufferedImage.TYPE_INT_RGB)
      for (y <- 0 until newH){
        for (x <- 0 until newW){
          if(  
            ( (x>margin) && (x<newW-margin) ) &&
            ( (y>atasbawah) && (y<newH-atasbawah) )
          ){
            output.setRGB(x, y, img.getRGB(x-margin + i.LPoint.x, y-atasbawah + i.LPoint.y))
          }else{
            output.setRGB(x, y, img.getRGB(0,0))
          }
          // println(x+", "+y)
        }
      }

      val hrasio = newW*1280/800
      var currH = 0
      if( newH > hrasio ){
        var divider = 2
        var newHsub = newH/divider
        while(newHsub > hrasio){
          divider += 1
          newHsub = newH/divider
        }
        for(c<-1 to divider){
          var outputsub = new BufferedImage(newW, newHsub, BufferedImage.TYPE_INT_RGB)
          for (y <- 0 until newHsub){
            for (x <- 0 until newW){
              outputsub.setRGB(x, y, output.getRGB(x, y+currH))
            }
          }
          println(newW+" "+newHsub)
          ImageIO.write(outputsub, "jpg", new File(numbering+"-"+numbering2+".jpg"))
          currH += newHsub
          numbering2 += 1
        }

      }else{
        println(newW+" "+newH)
        ImageIO.write(output, "jpg", new File(numbering+".jpg"))
      }
      numbering += 1
    })
  }

  val photo1 = ImageIO.read(new File(namafile))

  val w = photo1.getWidth
  val h = photo1.getHeight
  println(w+" "+h)

  val point = getFirstEdge(photo1, w)
  println(point.toString)

  var (toplist, margin) = getTopPanel(photo1, w, point)
  // println(toplist.reverse.toString)
  
  var totpanel : List[Panel] = List()
  totpanel = getListPanel(photo1, toplist, h)

  println( totpanel.reverse.toString )

  cetakAll(totpanel, photo1, margin, atasbawah)
}

trait Greeting {
  val atasbawah: Int = 600
  val namafile: String = "tes.jpg"
}
