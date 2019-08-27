package com.dengxq.lnglat2Geo.entity

import com.google.common.geometry.{S2CellId, S2LatLng}

@SerialVersionUID(678776497797479999L)
case class Location(var lng: Double, var lat: Double) extends Serializable {
  def id: Long = S2CellId.fromLatLng(S2LatLng.fromDegrees(lat, lng)).id
  def parendId(level:Int): Long = S2CellId.fromLatLng(S2LatLng.fromDegrees(lat, lng)).parent(level).id
}

object Location {
  private def verifyLngLat(lng: Double, lat: Double) :Boolean = lng >= -180.0 && lng <= 180.0 && lat >= -90.0 && lat <= 90.0

  def fromLngLatString(strLngLat: String) :Location = {
    if(null != strLngLat && strLngLat.contains(",")) {
      val Array(lng, lat) = strLngLat.split(",").map(x=>x.toDouble)

      if(verifyLngLat(lng, lat)) {
        Location(lng, lat)
      } else null
    } else null
  }

  def fromLatLngString(strLatLng: String) :Location = {
    if(null != strLatLng && strLatLng.contains(",")) {
      val Array(lat, lng) = strLatLng.split(",").map(x=>x.toDouble)

      if(verifyLngLat(lng, lat)) {
        Location(lng, lat)
      } else null
    } else null
  }
}

case class Bound(min:Location, max: Location) {
  def contains(lng:Double, lat:Double) :Boolean =
    lng >= min.lng && lat >= min.lat && lng <= max.lng && lat <= max.lat
}