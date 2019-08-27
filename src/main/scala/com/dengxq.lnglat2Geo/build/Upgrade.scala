package com.dengxq.lnglat2Geo.build

import java.nio.file.Paths
import java.text.Collator

import com.dengxq.lnglat2Geo.build.AdminDataProvider.{AMapData, DistrictLoader}
import com.google.common.geometry._
import com.dengxq.lnglat2Geo.entity._
import com.dengxq.lnglat2Geo.utils.{AdminUtils, ObjectSerializer, S2Utils}
import com.dengxq.lnglat2Geo.utils.S2Utils.{childrenCellId, getLevel}
import com.dengxq.lnglat2Geo.GeoTransImpl.min_level
import AdminDataProvider._

import collection.JavaConversions._

object Upgrade {

  private val MAX_INTERVER = 500

  /**
    * 加载全部区县的边界数据，拆分过长线段，每段不超过500m
    *
    * @return
    */
  private def loadBoundary(): Array[AdminBoundary] = {
    DistrictLoader.loadAMapJson("country/" + 100000 + ".json")
      .get
      .districts
      .flatMap(province => {
        val provinceData = DistrictLoader.loadAMapJson("province/" + province.adcode + ".json")
        if (provinceData.nonEmpty) {
          val cityDatas = provinceData.get
            .districts
            .flatMap(city => {
              val cityData = DistrictLoader.loadAMapJson("city/" + city.adcode + ".json")
              if (cityData.nonEmpty) {
                val districtDatas = cityData.get.districts.map(dis => DistrictLoader.loadAMapJson("district/" + dis.adcode + ".json"))
                if (districtDatas.exists(s => s.nonEmpty && s.get.polyline.nonEmpty)) districtDatas
                else List(cityData)
              } else {
                val districtData = DistrictLoader.loadAMapJson("district/" + city.adcode + ".json")
                if (districtData.nonEmpty && districtData.get.polyline.nonEmpty) List(districtData)
                else List.empty
              }
            })
          if (cityDatas.exists(s => s.nonEmpty && s.get.polyline.nonEmpty)) cityDatas
          else List(provinceData)
        } else List.empty
      })
      .filter(_.nonEmpty)
      .flatMap(district => {
        if (district.get.polyline.nonEmpty) {
          val data = district.get.polyline.get.split('|').map(loopStr => {
            val ss = loopStr.split(';')
              .map(coordStr => {
                val parts = coordStr.split(',')
                val lng = parts(0).toDouble
                val lat = parts(1).toDouble
                (lng, lat, S2LatLng.fromDegrees(lat, lng))
              })
              .sliding(2, 1)
              .flatMap(slice => {
                val dis = slice.head._3.getEarthDistance(slice.last._3).toInt
                val n = dis / MAX_INTERVER + 1
                (0 to n)
                  .map(i => (slice.head._1 + (slice.last._1 - slice.head._1) * i / n, slice.head._2 + (slice.last._2 - slice.head._2) * i / n))
                  .sliding(2, 1)
              })
              .toArray
            (ss.map(_.head) ++ List(ss.last.last))
              .map(s => S2CellId.fromLatLng(S2LatLng.fromDegrees(s._2, s._1)).id())
          })
          List(AdminBoundary(district.get.adcode.toInt, data))
        }
        else List.empty
      })
  }

  def genCellAdmin(): Array[CellAdmin] = {

    // 国内区县边界S2块
    val boundaryIndex: Map[Long, List[Long]] =
      AdminDataProvider.AdminLoader.loadBoundaryData
        .keySet
        .map(s => (new S2CellId(s).parent(min_level).id(), s))
        .groupBy(_._1)
        .map(s => (s._1, s._2.map(_._2).toList))

    // 计算国内区县非边界线区域的S2块
    val chinaCellAdmin = DistrictLoader.loadAMapJson("country/" + 100000 + ".json")
      .get
      .districts
      .flatMap(province => {
        val provinceData = DistrictLoader.loadAMapJson("province/" + province.adcode + ".json")
        if (provinceData.nonEmpty) {
          val cityDatas = provinceData.get
            .districts
            .flatMap(city => {
              val cityData = DistrictLoader.loadAMapJson("city/" + city.adcode + ".json")
              if (cityData.nonEmpty) {
                val districtDatas = cityData.get.districts.map(dis => DistrictLoader.loadAMapJson("district/" + dis.adcode + ".json"))
                if (districtDatas.exists(s => s.nonEmpty && s.get.polyline.nonEmpty)) districtDatas
                else List(cityData)
              } else {
                val districtData = DistrictLoader.loadAMapJson("district/" + city.adcode + ".json")
                if (districtData.nonEmpty && districtData.get.polyline.nonEmpty) List(districtData)
                else List.empty
              }
            })
          if (cityDatas.exists(s => s.nonEmpty && s.get.polyline.nonEmpty)) cityDatas
          else List(provinceData)
        } else List.empty
      })
      .flatMap(admin => {
        if (admin.nonEmpty && admin.get.polyline.nonEmpty)
          S2Utils.loadS2CellUnion(admin.get.polyline.get, min_level, min_level, 1000, true)
            .cellIds()
            .flatMap(s2CellId => {
              val cellLevel = getLevel(s2CellId.id)
              if (cellLevel == min_level) List(s2CellId.id())
              else childrenCellId(s2CellId, cellLevel, min_level).map(_.id()).toList
            })
            .distinct
            .map(s => (s, boundaryIndex.getOrElse(s, List.empty)))
            .filter(_._2.isEmpty)
            .map(s => (s._1, admin.get.adcode.toInt))
            .toList
        else List.empty
      })
      .groupBy(_._1)
      .filter(_._2.length == 1)
      .flatMap(s => s._2)
      .toArray

    // 计算中国国界线包含的所有区域
    val chinaPoly = DistrictLoader.loadAMapJson("country/" + 100000 + ".json")
      .get.polyline.get
    val chinaCell = S2Utils.loadS2CellUnion(chinaPoly, min_level, min_level, 10000, false)
      .cellIds()
      .flatMap(s2CellId => {
        val cellLevel = getLevel(s2CellId.id)
        if (cellLevel == min_level) List(s2CellId.id())
        else if (cellLevel < min_level) childrenCellId(s2CellId, cellLevel, min_level).map(_.id()).toList
        else List.empty
      })
      .distinct
      .map(s => (s, 1))
      .toMap

    // 计算中国最小外接矩形内的非中国区域
    val startS2: S2LatLng = S2LatLng.fromDegrees(0.8293, 72.004) //左下角
    val endS2: S2LatLng = S2LatLng.fromDegrees(55.8271, 137.8347) //右上角
    val rect: S2LatLngRect = new S2LatLngRect(startS2, endS2)
    val coverer: S2RegionCoverer = new S2RegionCoverer
    coverer.setMaxLevel(7)
    coverer.setMinLevel(7)
    coverer.getCovering(rect).cellIds()
      .flatMap(s2CellId => {
        val cellLevel = getLevel(s2CellId.id)
        if (cellLevel == min_level) List(s2CellId.id())
        else if (cellLevel < min_level) childrenCellId(s2CellId, cellLevel, min_level).map(_.id()).toList
        else List.empty
      })
      .distinct
      .map(s => (s, chinaCell.getOrElse(s, -1)))
      .filter(_._2 == -1)
      .union(chinaCellAdmin)
      .map(s => {
        val parent = new S2CellId(s._1).parent(10).id()
        (parent, s._2, s._1)
      })
      .groupBy(s => (s._1, s._2))
      .flatMap(s => {
        if (s._2.size == 16) List(CellAdmin(s._1._2, s._1._1))
        else s._2.map(ss => CellAdmin(ss._2, ss._3))
      })
      .toArray
  }

  private def encodeTownship(): Array[AdminNode] = {
    loadChinaAdmin(AMapData(None, "100000", "", None, "", "country", Array.empty), 0)
      .filter(s => s.level.equals(DistrictLevel.Street))
      .groupBy(_.id)
      .flatMap(admin =>
        admin._2
          .sortWith((a, b) => Collator.getInstance(java.util.Locale.SIMPLIFIED_CHINESE).compare(a.name, b.name) < 0)
          .zipWithIndex
          .map(s => {
            s._1.id = s._1.id * 100 + s._2 + 1
            s._1
          })
      ).toArray
  }

  private def loadChinaDistrict(): Array[AdminNode] = {
    val streetMap = AdminDataProvider.AdminLoader.loadStreetData
      .map(s => (s.parentId, s.id))
      .groupBy(_._1)
      .map(s => (s._1, s._2.map(_._2).toList))

    Upgrade.loadChinaAdmin(AMapData(None, "100000", "", None, "", "country", Array.empty), 0)
      .filter(s => !s.level.equals(DistrictLevel.Street))
      .map(s => {
        if (s.children.isEmpty) s.children = streetMap.getOrElse(s.id, List.empty)
        s
      }).toArray
  }

  def loadChinaAdmin(aMapData: AMapData, parentId: Int): List[AdminNode] = {
    if (aMapData.level != "street") {
      val nodeData = DistrictLoader.loadAMapJson(aMapData.level + "/" + aMapData.adcode + ".json").orNull
      if (nodeData != null) {
        val name = if (nodeData.level == "country") "中国" else nodeData.name // 硬编码 "中国", 踢掉json中"中华人民共和国"
        val level = DistrictLevel.withName(nodeData.level)
        List(AdminNode(nodeData.adcode.toInt, name, AdminUtils.shortAdmin(name, level), Location.fromLngLatString(nodeData.center), level,
          parentId, if (nodeData.districts.nonEmpty && nodeData.districts.head.level != "street") nodeData.districts.map(_.adcode.toInt).toList else List.empty)) ++
          nodeData.districts.flatMap(admin => loadChinaAdmin(admin, nodeData.adcode.toInt)).toList
      } else List.empty
    } else {
      val level = DistrictLevel.withName(aMapData.level)
      List(AdminNode(aMapData.adcode.toInt, aMapData.name, AdminUtils.shortAdmin(aMapData.name, level), Location.fromLngLatString(aMapData.center), level, parentId, List.empty))
    }
  }

  def upgradeAdminBoundary(): Unit = {
    val data = loadBoundary()
    ObjectSerializer.serialize(data, Paths.get(CHINA_DISTRICT_BOUNDARY).toAbsolutePath.toString)
  }

  def upgradeAdminBoundaryCell(): Unit = {
    val data = genCellAdmin()
    ObjectSerializer.serialize(data, Paths.get(CHINA_DISTRICT_BOUNDARY_CELL).toAbsolutePath.toString)
  }

  def upgradeChinaAdmin(): Unit = {
    val towns: Array[AdminNode] = loadChinaDistrict()
    ObjectSerializer.serialize(towns, Paths.get(CHINA_DISTRICT_ADMIN).toAbsolutePath.toString)
  }

  def upgradeTownship(): Unit = {
    val towns: Array[AdminNode] = encodeTownship()
    ObjectSerializer.serialize(towns, Paths.get(CHINA_FILE_NAME_STREETS).toAbsolutePath.toString)
  }

  def upgradeBusinessAreaData(): Unit = {
    val obj = CityAreaDataProvider.loadBusinessArea
    ObjectSerializer.serialize(obj, Paths.get(CityAreaDataProvider.BUSINESS_AREA_DATA).toAbsolutePath.toString)
  }

  def main(args: Array[String]): Unit = {
    Upgrade.upgradeAdminBoundary() // 生成行政区划边界数据
    Upgrade.upgradeAdminBoundaryCell() // 生成行政区划边界数据2
    Upgrade.upgradeChinaAdmin()  // 生成行政区划关系数据（不含街道）
    Upgrade.upgradeTownship()  // 生成行政区划街道数据
    Upgrade.upgradeBusinessAreaData() // 生成商圈数据
  }
}
