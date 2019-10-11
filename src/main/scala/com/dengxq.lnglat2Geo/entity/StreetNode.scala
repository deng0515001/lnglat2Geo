package com.dengxq.lnglat2Geo.entity

import com.dengxq.lnglat2Geo.entity.DistrictLevel.DistrictLevel
import com.dengxq.lnglat2Geo.utils.GeoUtils

case class StreetNode(citycode: Int, adcode: Int, name: String, level: DistrictLevel, center: Location) extends Serializable

case class StreetMap(data: Map[Int, Array[StreetNode]])

object Func {
  implicit def fun(streetMap: StreetMap): _sm = new _sm(streetMap)
  class _sm(streetMap: StreetMap) {
    def findNearStreet(districtAdmin: Admin, lonlat: (Double, Double)) :Option[StreetNode] = {
      val loc = Location(lonlat._1, lonlat._2)
      val districtId = if(districtAdmin.districtCode != -1) districtAdmin.districtCode else districtAdmin.cityCode
      if(streetMap.data.contains(districtId)) {
        val streetDistanceLs = streetMap.data(districtId)
          .flatMap { sn =>
            val d = GeoUtils.distance(sn.center, loc)
            Some(d, sn)
          }
        if(streetDistanceLs.nonEmpty) {
          Some(streetDistanceLs.minBy(_._1)._2)
        } else None
      } else None
    }
  }
}