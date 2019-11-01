package com.dengxq.lnglat2Geo.utils

import java.util

import com.google.common.geometry._
import com.dengxq.lnglat2Geo.entity.Azimuth.Azimuth
import com.dengxq.lnglat2Geo.entity.{Azimuth, Bound, CoordinateSystem, Location}
import com.dengxq.lnglat2Geo.entity.EnumImplicit._

import scala.collection.JavaConverters._

object GeoUtils {
  private val x_PI = Math.PI * 3000.0 / 180.0
  private val EE = 0.00669342162296594323
  private val A = 6378245.0 // BJZ54坐标系地球长半轴, m
  private val EQUATOR_C = 20037508.3427892 // 赤道周长, m
  private val EARTH_RADIUS = 6378137.0 //WGS84, CGCS2000坐标系地球长半轴, m
  private val EARTH_POLAR_RADIUS = 6356725.0; //极半径, m

  private val SQRT2 = 1.414213562

  private def rad(d: Double) = d * Math.PI / 180.0

  def distance(locA: Location, locB: Location): Double = {
    val lngA = locA.lng
    val latA = locA.lat
    val lngB = locB.lng
    val latB = locB.lat
    val f = rad((latA + latB) / 2)
    val g = rad((latA - latB) / 2)
    val l = rad((lngA - lngB) / 2)
    if (g == 0 && l == 0) return 0
    var sg = Math.sin(g)
    var sl = Math.sin(l)
    var sf = Math.sin(f)
    var s = .0
    var c = .0
    var w = .0
    var r = .0
    var d = .0
    var h1 = .0
    var h2 = .0
    var dis = .0
    val a = EARTH_RADIUS
    val fl = 1 / 298.257
    sg = sg * sg
    sl = sl * sl
    sf = sf * sf
    s = sg * (1 - sl) + (1 - sf) * sl
    c = (1 - sg) * (1 - sl) + sf * sl
    w = Math.atan(Math.sqrt(s / c))
    r = Math.sqrt(s * c) / w
    d = 2 * w * a
    h1 = (3 * r - 1) / 2 / c
    h2 = (3 * r + 1) / 2 / s
    dis = d * (1 + fl * (h1 * sf * (1 - sg) - h2 * (1 - sf) * sg))

    dis.formatted("%.2f").toDouble
  }

  def gcj02ToWgs84(lng: Double, lat: Double): (Double, Double) = {
    if (outOfChina(lng, lat)) return new Tuple2[Double, Double](lng, lat)
    var dlat = transformLat(lng - 105.0, lat - 35.0)
    var dlng = transformLng(lng - 105.0, lat - 35.0)
    val radlat = lat / 180.0 * Math.PI
    var magic = Math.sin(radlat)
    magic = 1 - EE * magic * magic
    val sqrtmagic = Math.sqrt(magic)
    dlat = (dlat * 180.0) / ((A * (1 - EE)) / (magic * sqrtmagic) * Math.PI)
    dlng = (dlng * 180.0) / (A / sqrtmagic * Math.cos(radlat) * Math.PI)
    val mglat = lat + dlat
    val mglng = lng + dlng
    new Tuple2[Double, Double](lng * 2 - mglng, lat * 2 - mglat)
  }

  def gcj02ToBD09(lng: Double, lat: Double): (Double, Double) = {
    if (outOfChina(lng, lat)) return new Tuple2[Double, Double](lng, lat)
    val z = Math.sqrt(lng * lng + lat * lat) + 0.00002 * Math.sin(lat * x_PI)
    val theta = Math.atan2(lat, lng) + 0.000003 * Math.cos(lng * x_PI)
    val bd_lng = z * Math.cos(theta) + 0.0065
    val bd_lat = z * Math.sin(theta) + 0.006
    new Tuple2[Double, Double](bd_lng, bd_lat)
  }

  def bd09ToGCJ02(lng: Double, lat: Double): (Double, Double) = {
    if (outOfChina(lng, lat)) return new Tuple2[Double, Double](lng, lat)
    val x = lng - 0.0065
    val y = lat - 0.006
    val z = Math.sqrt(x * x + y * y) - 0.00002 * Math.sin(y * x_PI)
    val theta = Math.atan2(y, x) - 0.000003 * Math.cos(x * x_PI)
    val gg_lng = z * Math.cos(theta)
    val gg_lat = z * Math.sin(theta)
    new Tuple2[Double, Double](gg_lng, gg_lat)
  }

  def wgs84ToGCj02(lng: Double, lat: Double): (Double, Double) = {
    var mglat = .0
    var mglng = .0
    if (outOfChina(lng, lat)) {
      mglat = lat
      mglng = lng
    }
    else {
      var dLat = transformLat(lng - 105.0, lat - 35.0)
      var dLon = transformLng(lng - 105.0, lat - 35.0)
      val radLat = lat / 180.0 * Math.PI
      var magic = Math.sin(radLat)
      magic = 1 - EE * magic * magic
      val sqrtMagic = Math.sqrt(magic)
      dLat = (dLat * 180.0) / ((A * (1 - EE)) / (magic * sqrtMagic) * Math.PI)
      dLon = (dLon * 180.0) / (A / sqrtMagic * Math.cos(radLat) * Math.PI)
      mglat = lat + dLat
      mglng = lng + dLon
    }
    new Tuple2[Double, Double](mglng, mglat)
  }

  private def transformLng(lng: Double, lat: Double) = {
    var ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * Math.sqrt(Math.abs(lng))
    ret += (20.0 * Math.sin(6.0 * lng * Math.PI) + 20.0 * Math.sin(2.0 * lng * Math.PI)) * 2.0 / 3.0
    ret += (20.0 * Math.sin(lng * Math.PI) + 40.0 * Math.sin(lng / 3.0 * Math.PI)) * 2.0 / 3.0
    ret += (150.0 * Math.sin(lng / 12.0 * Math.PI) + 300.0 * Math.sin(lng / 30.0 * Math.PI)) * 2.0 / 3.0
    ret
  }

  private def transformLat(lng: Double, lat: Double) = {
    var ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * Math.sqrt(Math.abs(lng))
    ret += (20.0 * Math.sin(6.0 * lng * Math.PI) + 20.0 * Math.sin(2.0 * lng * Math.PI)) * 2.0 / 3.0
    ret += (20.0 * Math.sin(lat * Math.PI) + 40.0 * Math.sin(lat / 3.0 * Math.PI)) * 2.0 / 3.0
    ret += (160.0 * Math.sin(lat / 12.0 * Math.PI) + 320 * Math.sin(lat * Math.PI / 30.0)) * 2.0 / 3.0
    ret
  }

  def isInChina(lng: Double, lat: Double): Boolean = !outOfChina(lng, lat)

  def outOfChina(lng: Double, lat: Double): Boolean = {
    lng < 72.004 || lng > 137.8347 || lat < 0.8293 || lat > 55.8271
  }

  def checkLoc(lng: Double, lat: Double) : Boolean = lng <= 180 && lng >= -180 && lat <=90 && lat >= -90

  def toGCJ02(lng: Double, lat:Double, coordType: CoordinateSystem.CoordinateSystem): (Double, Double) = {
    coordType match {
      case CoordinateSystem.WGS84 =>
        val d = GeoUtils.wgs84ToGCj02(lng, lat)
        (d._1, d._2)
      case CoordinateSystem.BD09 =>
        val d = GeoUtils.bd09ToGCJ02(lng, lat)
        (d._1, d._2)
      case _ => (lng, lat)
    }
  }

  def toWGS84(lng: Double, lat:Double, coordType: CoordinateSystem.CoordinateSystem): (Double, Double) = {
    coordType match {
      case CoordinateSystem.GCJ02 =>
        val d = GeoUtils.gcj02ToWgs84(lng, lat)
        (d._1, d._2)
      case CoordinateSystem.BD09 =>
        val d02 = GeoUtils.bd09ToGCJ02(lng, lat)
        val d = GeoUtils.gcj02ToWgs84(d02._1, d02._2)
        (d._1, d._2)
      case _ => (lng, lat)
    }
  }

  @deprecated
  def nearest[A](source: Array[A], loc: Location, locExpression: A => Location) :Option[A] = {
    if(source == null || source.isEmpty) {
      None
    } else {
      val locId = S2CellId.fromLatLng(S2LatLng.fromDegrees(loc.lat, loc.lng)).id
      val MARK_ITEM = (locId, None)
      //      (source.map {item=>
      //        val itemLoc = locExpression(item)
      //        val id = S2CellId.fromLatLng(S2LatLng.fromDegrees(itemLoc.lat, itemLoc.lng)).id
      //        (GeoUtils.distance(itemLoc, loc), item, id, id.toBinaryString)
      //      } :+ (0, None, locId, locId.toBinaryString))
      //        .sortBy(_._4).foreach(println)

      val sortedLs = (source.map{ item=>
        val itemLoc = locExpression(item)
        val id = S2CellId.fromLatLng(S2LatLng.fromDegrees(itemLoc.lat, itemLoc.lng)).id
        (id, Some(item))
      } :+ MARK_ITEM).sortBy(_._1)
      val locIndex = sortedLs.indexOf(MARK_ITEM)
      val nearestLeft: Option[A] = if(locIndex != 0)  sortedLs(locIndex - 1)._2 else None
      val nearestRight: Option[A] = if(locIndex == sortedLs.length - 1) None else sortedLs(locIndex + 1)._2

      val distanceLs : Array[(Double, A)] = Array(nearestLeft, nearestRight).flatMap {
        case Some(nearest) =>
          val d = GeoUtils.distance(locExpression(nearest), loc)
          Some(d, nearest)
        case None => None
      }

      if(distanceLs.nonEmpty) {
        Some(distanceLs.minBy(_._1)._2)
      } else None
    }
  }

  private val kEarthCircumferenceMeters = 1000 * 40075.017
  private def earthMeters2Radians(meters: Double) = (2 * S2.M_PI) * (meters / kEarthCircumferenceMeters)

  def genS2Cap(loc: Location, radius: Double): S2Cap = {
    val s2LatLng = S2LatLng.fromDegrees(loc.lat, loc.lng)
    val radiusRadians = earthMeters2Radians(radius)
    S2Cap.fromAxisHeight(s2LatLng.normalized.toPoint, (radiusRadians * radiusRadians) / 2)
  }

  def genS2CellUnion[T<: S2Region](regions: Seq[T], minLevel:Int = 0, maxLevel:Int = 0, maxCells:Int=0) :S2CellUnion = {
    val regionCoverer = new S2RegionCoverer()
    if(maxCells!=0) regionCoverer.setMaxCells(maxCells)
    if(maxLevel!=0) regionCoverer.setMaxLevel(maxLevel)
    if(minLevel!=0) regionCoverer.setMinLevel(minLevel)
    val s2CellIds = new util.ArrayList[S2CellId]()
    regions.flatMap(x=>regionCoverer.getCovering(x).cellIds().asScala)
      .foreach(s2CellIds.add)
    val cellUnion = new S2CellUnion()
    cellUnion.initFromCellIds(s2CellIds)
    cellUnion
  }

  def move(loc:Location, distance:Double, azimuth: Azimuth) :Location = {
    val radLat = rad(loc.lat)
    val radLng = rad(loc.lng)

    val ec = EARTH_POLAR_RADIUS + (EARTH_RADIUS - EARTH_POLAR_RADIUS) * (90 - loc.lng) / 90
    val ed = ec * Math.cos(radLat)

    val dx = distance * Math.sin(azimuth * Math.PI / 180)
    val dy = distance * Math.cos(azimuth * Math.PI / 180)

    val lng = (dx / ed + radLng) * 180 / Math.PI
    val lat = (dy / ec + radLat) * 180 / Math.PI

    Location(lng, lat)
  }

  def genCapBound(loc:Location, radius:Double) :Bound= {
    val swDistance = SQRT2 * radius
    val sw = move(loc, swDistance, Azimuth.SouthWest)
    val ne = move(loc, swDistance, Azimuth.NorthEast)
    Bound(sw, ne)
  }

  def genCapInnerBound(loc:Location, radius:Double) :Bound= {
    // val swDistance = SQRT2 / 2d * radius
    val sw = move(loc, radius, Azimuth.SouthWest)
    val ne = move(loc, radius, Azimuth.NorthEast)
    Bound(sw, ne)
  }

  def genCellBound(center: Location, radius: Double, level: Int): Seq[String] = {
    val cap = genS2Cap(center, radius)
    val cu = genS2CellUnion(Seq(cap), level, level, 5)
    cu.cellIds().asScala.map{_.id.toHexString}
  }

  def toCellBoundId(lng:Double, lat:Double, level:Int): String =
    S2CellId.fromLatLng(S2LatLng.fromDegrees(lat, lng)).parent(9).id.toHexString
}
