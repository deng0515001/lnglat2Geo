package com.dengxq.lnglat2Geo.utils

import com.google.common.geometry._

import scala.collection.mutable.ArrayBuffer
import collection.JavaConverters._

object S2Utils {

  // 预算，提升速度
  val capHeightMap = List(2, 4, 8, 16, 32, 64, 128, 256).map(radius => {
    val rad = earthMeters2Radians(radius * 1000)
    (radius * 1000, rad * rad * 2)
  }).toMap

  def getCellId(s2LatLng: S2LatLng, radius: Int, desLevel: Int): List[Long] = {
    val capHeight = capHeightMap.getOrElse(radius, 0d)
    val cap = S2Cap.fromAxisHeight(s2LatLng.toPoint, capHeight)
    val coverer = new S2RegionCoverer
    coverer.setMaxLevel(desLevel)
    coverer.setMinLevel(desLevel)
    //圆形内的cell会自动做聚合，手动拆分
    import scala.collection.JavaConversions._

    coverer.getCovering(cap)
      .cellIds
      .flatMap(s2CellId => {
        val cellLevel = getLevel(s2CellId.id)
        if (cellLevel == desLevel) List(s2CellId.id())
        else childrenCellId(s2CellId, cellLevel, desLevel).map(_.id())
      })
      .toList
  }

  def childrenCellId(s2CellId: S2CellId, curLevel: Int, desLevel: Int): ArrayBuffer[S2CellId] = {
    val list = ArrayBuffer.empty[S2CellId]
    if (curLevel < desLevel) {
      val interval = (s2CellId.childEnd.id - s2CellId.childBegin.id) / 4
      for (i <- 0 to 4) {
        val id = s2CellId.childBegin.id + interval * i
        val cellId = new S2CellId(id)
        list ++= childrenCellId(cellId, curLevel + 1, desLevel)
      }
    } else list.append(s2CellId)
    list
  }

  def getLevel(inputs: Long): Int = {
    var n = 0
    var input = inputs
    while (input % 2 == 0) {
      input = input / 2
      n += 1
    }
    30 - n / 2
  }

  private val kEarthCircumferenceMeters = 1000 * 40075.017

  private def earthMeters2Radians(meters: Double) :Double = (2 * S2.M_PI) * (meters / 40075017)

  def loadS2CellUnion(polylineString: String, minLevel: Int = -1, maxLevel: Int = -1, maxCells: Int = -1): S2CellUnion = {
    val s2PolygonBuilder = new S2PolygonBuilder()

    // polyline 中 | 分割多个polygon, 每个polygon的点用 ; 分割.
    // 每个 polygon 构建 一个 S2Loop, 合并到 S2PolygonBuilder中生成 multi polygon
    polylineString.split('|').foreach(loopStr => {
      val points: Array[S2Point] = loopStr.split(';').map(coordStr => {
        val parts = coordStr.split(',')
        val lng = parts(0).toDouble
        val lat = parts(1).toDouble
        S2LatLng.fromDegrees(lat, lng).toPoint
      })
      val jPoints = points.toList.asJava

      s2PolygonBuilder.addLoop(new S2Loop(jPoints))
    })
    val polygon = s2PolygonBuilder.assemblePolygon()
    // 栅格化处理区域多边形
    val coverer = new S2RegionCoverer()
    if (minLevel > 0) coverer.setMinLevel(minLevel)
    if (maxLevel > 0) coverer.setMaxLevel(maxLevel)
    if (maxCells > 0) coverer.setMaxCells(maxCells)
    val s2CellUnion = coverer.getCovering(polygon)
    s2CellUnion.normalize()
    s2CellUnion
  }

  def loadS2CellUnionFromId(polyline: Array[Array[Long]], minLevel: Int = -1, maxLevel: Int = -1, maxCells: Int = -1): S2CellUnion = {
    val s2PolygonBuilder = new S2PolygonBuilder()
    polyline.foreach(loopStr => {
      val points: Array[S2Point] = loopStr.map(id => new S2CellId(id).toPoint)
      val jPoints = points.toList.asJava
      s2PolygonBuilder.addLoop(new S2Loop(jPoints))
    })
    val polygon = s2PolygonBuilder.assemblePolygon()
    // 栅格化处理区域多边形
    val coverer = new S2RegionCoverer()
    if (minLevel > 0) coverer.setMinLevel(minLevel)
    if (maxLevel > 0) coverer.setMaxLevel(maxLevel)
    if (maxCells > 0) coverer.setMaxCells(maxCells)
    val s2CellUnion = coverer.getCovering(polygon)
    s2CellUnion.normalize()
    s2CellUnion
  }
}
