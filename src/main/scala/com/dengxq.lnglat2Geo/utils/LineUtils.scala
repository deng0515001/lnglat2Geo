package com.dengxq.lnglat2Geo.utils

/**
  * Created by dengxq on 18-8-3.
  */
object LineUtils {

  /**
    * 已知两点，求两点直线
    *
    * @param p1
    * @param p2
    * @return 　直线
    */
  def getLine(p1: Point, p2: Point): FitLine = {
    val k = (p2.y - p1.y) / (p2.x - p1.x)
    val b = p1.y - k * p1.x
    FitLine(k, b)
  }

  /**
    * 计算两条直线的交点, 直线为 y = kx + b 型直线
    *
    * @param fitLine  　第一条直线
    * @param fitLine2 第二条直线
    * @return 　交点
    */
  def getLineIntersection(fitLine: FitLine, fitLine2: FitLine): (Double, Double) = {
    val x = -(fitLine.b - fitLine2.b) / (fitLine.k - fitLine2.k)
    val y = fitLine.k * x + fitLine.b
    (x, y)
  }

  /**
    * 点到直线的距离，直线为 y = kx + b 型直线
    *
    * @param fitLine
    * @param p
    * @return
    */
  def pointToLineDistance(fitLine: FitLine, p: Point): Double = {
    val dis = Math.abs(fitLine.k * p.x - p.y + fitLine.b) / Math.sqrt(fitLine.k * fitLine.k + 1)
    dis
  }

  /**
    * 平行直线之间的距离，直线为 y = kx + b 型直线
    *
    * @param fitLine
    * @param fitLine2
    * @return
    */
  def lineToLineDistance(fitLine: FitLine, fitLine2: FitLine): Double = {
    var dis = -1.0
    if (fitLine.k == fitLine2.k)
      dis = Math.abs(fitLine.b - fitLine2.b) / Math.sqrt(fitLine.k * fitLine.k + 1)
    dis
  }

  /**
    * 两条直线直线的夹角，tan值，如需具体角度，需做arctan处理
    *
    * @param fitLine
    * @param fitLine2
    * @return 　两条直线直线的夹角的tan值
    */
  def lineToLineAngle(fitLine: FitLine, fitLine2: FitLine): Double = {
    Math.atan(Math.abs(fitLine.k - fitLine2.k) / (fitLine.k * fitLine2.k + 1))
  }

  /**
    * 点在直线上的投影
    *
    * @param fitLine 　直线
    * @param p       　点
    * @return 　投影点
    */
//  def projectionOnLine(fitLine: FitLine, p: MPoint): (Double, Double) = {
//    val xx = (fitLine.k * p.y - fitLine.k * fitLine.b + p.x) / (fitLine.k * fitLine.k + 1)
//    val yy = (fitLine.k * fitLine.k * p.y + fitLine.k * p.x + fitLine.b) / (fitLine.k * fitLine.k + 1)
//    (xx, yy)
//  }

  def projectionOnLine(fitLine: FitLine, p: Point): (Double, Double) = {
    val xx = (fitLine.k * p.y - fitLine.k * fitLine.b + p.x) / (fitLine.k * fitLine.k + 1)
    val yy = (fitLine.k * fitLine.k * p.y + fitLine.k * p.x + fitLine.b) / (fitLine.k * fitLine.k + 1)
    (xx, yy)
  }

  /**
    * 判断两条线段是否相交
    * @param line1
    * @param line2
    * @return
    */
  def isLineSegmentIntersecting(line1: LineSegment, line2: LineSegment): Boolean = {
    val a = line1.p1
    val b = line1.p2
    val c = line2.p1
    val d = line2.p2

    //    快速排斥：
    //    两个线段为对角线组成的矩形，如果这两个矩形没有重叠的部分，那么两条线段是不可能出现重叠的
    //    1.线段ab的低点低于cd的最高点（可能重合）
    //    2.cd的最左端小于ab的最右端（可能重合）
    //    3.cd的最低点低于ab的最高点（加上条件1，两线段在竖直方向上重合）
    //    4.ab的最左端小于cd的最右端（加上条件2，两直线在水平方向上重合）
    //    综上4个条件，两条线段组成的矩形是重合的,特别要注意一个矩形含于另一个矩形之内的情况
    if (!(Math.min(a.x, b.x) <= Math.max(c.x, d.x) && Math.min(c.y, d.y) <= Math.max(a.y, b.y)
      && Math.min(c.x, d.x) <= Math.max(a.x, b.x) && Math.min(a.y, b.y) <= Math.max(c.y, d.y)))
      return false

    //    跨立实验：
    //    如果两条线段相交，那么必须跨立，就是以一条线段为标准，另一条线段的两端点一定在这条线段的两段
    //    也就是说a b两点在线段cd的两端，c d两点在线段ab的两端
    val u = (c.x - a.x) * (b.y - a.y) - (b.x - a.x) * (c.y - a.y)
    val v = (d.x - a.x) * (b.y - a.y) - (b.x - a.x) * (d.y - a.y)
    val w = (a.x - c.x) * (d.y - c.y) - (d.x - c.x) * (a.y - c.y)
    val z = (b.x - c.x) * (d.y - c.y) - (d.x - c.x) * (b.y - c.y)
    u * v <= 0.00000001 && w * z <= 0.00000001
  }

  /**
    * 点到线段的距离
    * @param x1
    * @param y1
    * @param x2
    * @param y2
    * @param x0
    * @param y0
    * @return
    */
  def pointToLineDis(x1: Double, y1: Double, x2: Double, y2: Double, x0: Double, y0: Double): Double = {
    var a = .0
    var b = .0
    var c = .0
    a = lineDis(x1, y1, x2, y2) // 线段的长度
    b = lineDis(x1, y1, x0, y0) // 点到起点的距离
    c = lineDis(x2, y2, x0, y0) // 点到终点的距离
    //点在端点上
    if (c <= 0.000001 || b <= 0.000001) {
      return 0
    }
    //直线距离过短
    if (a <= 0.000001) {
      return b
    }
    // 点在起点左侧，距离等于点到起点距离
    if (c * c >= a * a + b * b) {
      return b
    }
    //点在终点右侧，距离等于点到终点距离
    if (b * b >= a * a + c * c) {
      return c
    }
    //点在起点和终点中间，为垂线距离
    val k = (y2 - y1) / (x2 - x1)
    val z = y1 - k * x1
    val p = (a + b + c) / 2
    // 半周长
    val s = Math.sqrt(p * (p - a) * (p - b) * (p - c)) //海伦公式求面积
    2 * s / a // 返回点到线的距离（利用三角形面积公式求高）
  }

  // 计算两点之间的距离
  def lineDis(x1: Double, y1: Double, x2: Double, y2: Double): Double =
    Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

  case class Point(x: Double,
                   y: Double
                  )

  case class LineSegment(p1: Point,
                         p2: Point
                        )

  case class FitLine(k: Double,
                     b: Double
                    )

}
