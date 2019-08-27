package com.dengxq.lnglat2Geo.build

import java.util.Date

import com.google.common.geometry._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import scala.collection.JavaConverters._
import scala.io.{Codec, Source}
import scala.util.Try
import com.dengxq.lnglat2Geo.entity.{AdminBoundary, AdminNode, CellAdmin}
import com.dengxq.lnglat2Geo.utils.ObjectSerializer

object AdminDataProvider {

  final val CHINA_DISTRICT_BOUNDARY = "src/main/resources/china/boundary.data"
  final val RESOURCE_CHINA_DISTRICT_BOUNDARY = "/china/boundary.data"

  final val CHINA_DISTRICT_BOUNDARY_CELL = "src/main/resources/china/boundaryCell.data"
  final val RESOURCE_CHINA_DISTRICT_BOUNDARY_CELL = "/china/boundaryCell.data"

  final val CHINA_DISTRICT_ADMIN = "src/main/resources/china/admin.data"
  final val RESOURCE_CHINA_DISTRICT_ADMIN = "/china/admin.data"

  final val CHINA_FILE_NAME_STREETS = "src/main/resources/china/street.data"
  final val RESOURCE_CHINA_FILE_NAME_STREETS = "/china/street.data"

  final val RESOURCE_COUNTRY_CODE = "/china/countryCode"

  private[lnglat2Geo] case class AMapData(
                                           citycode: Option[String],
                                           adcode: String,
                                           name: String,
                                           polyline: Option[String],
                                           center: String,
                                           level: String,
                                           var districts: Array[AMapData]
                                         )

  object DistrictLoader {
    def loadAMapJson(fileName: String)(implicit mf: scala.reflect.Manifest[AMapData]): Option[AMapData] = {
      Try {
        implicit val formats = DefaultFormats
        val jsonStr = Source.fromFile("data/china/" + fileName)(Codec.UTF8).mkString
        val data = parse(jsonStr).extract[AMapData]
        Option(data)
      }.getOrElse(None)
    }
  }

  object AdminLoader {
    def loadBoundarySrc: Map[Int, Array[Array[Long]]] = {
      val stream = this.getClass.getResourceAsStream(RESOURCE_CHINA_DISTRICT_BOUNDARY)
      ObjectSerializer
        .deserialize[Array[AdminBoundary]](stream)
        .map(s => (s.code, s.boundary))
        .toMap
    }

    /**
      * 加载全国行政区划边界
      *
      * @return
      */
    def loadBoundaryData: Map[Long, List[(Long, Int, Boolean)]] = {
      val stream = this.getClass.getResourceAsStream(RESOURCE_CHINA_DISTRICT_BOUNDARY)
      ObjectSerializer
        .deserialize[Array[AdminBoundary]](stream)
        .flatMap(dis => dis.boundary.flatMap(line =>
          line.toList.sliding(2, 1).flatMap(s =>
            List((s.head, s.last, dis.code, true), (s.last, s.head, dis.code, false)))
        ))
        .toList
        .groupBy(_._1)
        .map(s => (s._1, s._2.map(ss => (ss._2, ss._3, ss._4))))
    }

    def loadBoundaryCellData: Map[Long, Int] = {
      val stream = this.getClass.getResourceAsStream(RESOURCE_CHINA_DISTRICT_BOUNDARY_CELL)
      ObjectSerializer
        .deserialize[Array[CellAdmin]](stream)
        .map(cellAdmin => {
          (cellAdmin.cellId, cellAdmin.adCode)
        })
        .toMap
    }

    /**
      * 加载全国行政区划数据
      *
      * @return
      */
    def loadAdminData: Map[Int, AdminNode] = {
      val stream = this.getClass.getResourceAsStream(RESOURCE_CHINA_DISTRICT_ADMIN)
      ObjectSerializer.deserialize[Array[AdminNode]](stream)
        .map(s => (s.id, s)).toMap
    }

    /**
      * 加载全国街道数据
      *
      * @return
      */
    def loadStreetData: Array[AdminNode] = {
      val stream = this.getClass.getResourceAsStream(RESOURCE_CHINA_FILE_NAME_STREETS)
      ObjectSerializer.deserialize[Array[AdminNode]](stream)
    }

    /**
      * 加载世界国家code
      * @return
      */
    def loadCountryCode : Map[String, String] = {
      val url = getClass.getResource(RESOURCE_COUNTRY_CODE)
      Source.fromURL(url)(Codec.UTF8)
        .getLines()
        .map(s => {
          val Array(code:String, name:String) = s.split(",")
          (code, name)
        })
        .toMap
    }
  }

}
