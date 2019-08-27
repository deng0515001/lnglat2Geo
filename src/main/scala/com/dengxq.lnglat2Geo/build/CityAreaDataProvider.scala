package com.dengxq.lnglat2Geo.build

import com.dengxq.lnglat2Geo.entity._
import com.dengxq.lnglat2Geo.utils.ObjectSerializer

import scala.io.{Codec, Source}

object CityAreaDataProvider {

  final val BUSINESS_AREA_DATA = "src/main/resources/china/area.data"
  final val RESOURCE_BUSINESS_AREA_DATA = "/china/area.data"

  /**
    * 加载全国商圈数据
    *
    * @return
    */
  def loadBusinessAreaData: Map[Int, Array[BusinessAreaData]] = {
    val stream = this.getClass.getResourceAsStream(RESOURCE_BUSINESS_AREA_DATA)
    ObjectSerializer.deserialize[Array[BusinessAreaGroup]](stream)
      .map(s => (s.cityAdCode, s.areas))
      .toMap
  }

  def loadBusinessArea: Array[BusinessAreaGroup] = {
    val areaCodeMap: Map[String, Int] = loadBusinessAreaCode
    Source.fromFile("data/china/cityareas")(Codec.UTF8)
      .getLines()
      .map(line => {
        val Array(cityAdcode, province, city, district, area_name, strLng, strLat) = line.split(",")
        val areaCode = areaCodeMap.getOrElse(List(province, city, area_name).mkString(","), -1)
        (cityAdcode.toInt, BusinessAreaData(area_name, Location(strLng.toDouble, strLat.toDouble), areaCode))
      })
      .toArray
      .groupBy(_._1)
      .map(s => BusinessAreaGroup(s._1, s._2.map(_._2)))
      .toArray
  }

  def loadBusinessAreaCode: Map[String, Int] = {
    Source.fromFile("data/china/busineesAreaCode")(Codec.UTF8)
      .getLines()
      .map(line => {
        val Array(adCode, area) = line.split(",")
        val Array(country, province, city, areaName) = area.split("/")
        (List(province, city, areaName).mkString(","), adCode.substring(2).toInt)
      }).toMap
  }
}
