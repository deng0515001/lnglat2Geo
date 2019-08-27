package com.dengxq.lnglat2Geo

import com.dengxq.lnglat2Geo.build.{AdminDataProvider, CityAreaDataProvider, CityLevelDataProvider}
import com.dengxq.lnglat2Geo.entity.CoordinateSystem.CoordinateSystem
import com.dengxq.lnglat2Geo.entity.DistrictLevel.DistrictLevel
import com.dengxq.lnglat2Geo.entity._
import com.dengxq.lnglat2Geo.utils.{GeoUtils, LineUtils, S2Utils}
import com.google.common.geometry.{S2CellId, S2LatLng}
import org.apache.commons.lang3.StringUtils

private[lnglat2Geo] object GeoTransImpl {

  final val min_level = 12

  lazy val cityLevelData: Map[String, String] = CityLevelDataProvider.csv2Map
  lazy val cityBusinessArea: Map[Int, Array[BusinessAreaData]] = CityAreaDataProvider.loadBusinessAreaData

  lazy val adminData: Map[Int, AdminNode] = AdminDataProvider.AdminLoader.loadAdminData
  lazy val streetData: Map[Int, AdminNode] = AdminDataProvider.AdminLoader.loadStreetData.map(s => (s.id, s)).toMap
  lazy val countryCode: Map[String, String] = AdminDataProvider.AdminLoader.loadCountryCode

  lazy val boundaryData: Map[Long, List[(Long, Int, Boolean)]] = AdminDataProvider.AdminLoader.loadBoundaryData
  lazy val boundaryIndex: Map[Long, List[Long]] = boundaryData
    .keySet
    .map(s => (new S2CellId(s).parent(min_level).id(), s))
    .groupBy(_._1)
    .map(s => (s._1, s._2.map(_._2).toList))
  lazy val boundaryAdminCell: Map[Long, Int] = AdminDataProvider.AdminLoader.loadBoundaryCellData

  /**
    * 按需初始化数据
    *
    * @param needBoundary 是否需要加载边界数据，用于经纬度转换省市区县
    * @param needArea     是否需要加载商圈数据
    * @param needStreet   是否需要加载街道数据
    * @return
    */
  def init(needBoundary: Boolean = true, needArea: Boolean = false, needStreet: Boolean = true, needCityLevel: Boolean = false): Unit = {
    adminData
    if (needBoundary) {
      boundaryData
      boundaryIndex
      boundaryAdminCell
    }
    if (needStreet) streetData
    if (needArea) cityBusinessArea
    if (needCityLevel) cityLevelData
  }

  def determineAdmin(lon: Double, lat: Double, needStreet: Boolean = false, coordSys: CoordinateSystem = CoordinateSystem.GCJ02): Admin = {
    val gcj02LonLat = GeoUtils.toGCJ02(lon, lat, coordSys)
    val code = determineAdminCode(gcj02LonLat._1, gcj02LonLat._2)
    if (code != -1) {
      val district = adminData.get(code).orNull
      val city = if (district.level == DistrictLevel.District) adminData.get(district.parentId).orNull else district
      val province = if (city.level == DistrictLevel.City) adminData.get(city.parentId).orNull else city

      var streetCode = 0
      var streetName = ""
      if (needStreet) {
        if (district.children.nonEmpty) {
          val street = district.children.map(s => streetData.get(s))
            .minBy(s => GeoUtils.distance(s.get.center, Location(gcj02LonLat._1, gcj02LonLat._2))).get
          streetCode = street.id
          streetName = street.name
        }
      }
      if (streetCode > 0) Admin.createStreet(province.name, city.name, district.name, streetName, province.id, city.id, district.id, streetCode, district.center)
      else Admin.createDistrict(province.name, city.name, district.name, province.id, city.id, district.id, district.center)
    } else Admin.createOversea
  }

  def getCityLevel(admin: Admin): String = {
    getCityLevel(admin.cityID.toString)
  }

  def getCityLevel(adcode_or_name: String): String = {
    cityLevelData.getOrElse(adcode_or_name, "未知")
  }

  def normalizeName(adcode: Int): AdminNode = {
    adminData.getOrElse(adcode, null)
  }

  def normalizeName(name: String, level: DistrictLevel): Seq[AdminNode] = {
    adminData.values.filter(_.level.equals(level))
      .filter(s => s.shortName.contains(name) || s.name.contains(name))
      .toSeq
  }

  def normalizeName(provinceIn: String = "", cityIn: String = "", districtIn: String = "", streetIn: String = "", isFullMatch: Boolean = false): Seq[Admin] = {
    val province = if (provinceIn == null || provinceIn.equals("未知")) "" else provinceIn
    val city = if (cityIn == null || cityIn.equals("未知")) "" else cityIn
    val district = if (districtIn == null || districtIn.equals("未知")) "" else districtIn
    val street = if (streetIn == null || streetIn.equals("未知")) "" else streetIn

    val provinceAd = adminData.values.filter(s => s.level.equals(DistrictLevel.Province)).filter(s => StringUtils.isEmpty(province) || s.name.equals(province) || (!isFullMatch && s.shortName.equals(province)))
    val adminNodes = if (StringUtils.isEmpty(city) && StringUtils.isEmpty(district) && StringUtils.isEmpty(street)) provinceAd
    else {
      val cityAd = provinceAd.flatMap(s => s.children.map(adCode => adminData.getOrElse(adCode, streetData.get(adCode).orNull)))
        .filter(s => !s.level.equals(DistrictLevel.City) || s.level.equals(DistrictLevel.City) && (StringUtils.isEmpty(city) || s.name.equals(city) || (!isFullMatch && s.shortName.equals(city))))
      if (cityAd.isEmpty) provinceAd
      else {
        if (StringUtils.isEmpty(district) && StringUtils.isEmpty(street)) cityAd.filter(s => s.level.equals(DistrictLevel.Province) || s.level.equals(DistrictLevel.City))
        else {
          var districtAd = cityAd.flatMap(s => {
            if (s.level.equals(DistrictLevel.City) && StringUtils.isEmpty(street)) s.children.map(adCode => adminData.get(adCode).orNull)
            else if (s.level.equals(DistrictLevel.City)) s.children.map(adCode => adminData.getOrElse(adCode, streetData.get(adCode).orNull))
            else List(s)
          })
            .filter(s => s != null)
            .filter(s => !s.level.equals(DistrictLevel.District) ||
              s.level.equals(DistrictLevel.District) &&
                (StringUtils.isEmpty(district) || s.name.equals(district) || (!isFullMatch && s.shortName.equals(district))))

          if (districtAd.isEmpty) districtAd = cityAd
          if (StringUtils.isEmpty(street)) districtAd.filter(s => s.level.equals(DistrictLevel.Province) || s.level.equals(DistrictLevel.City) || s.level.equals(DistrictLevel.District))
          else {
            var streetAd = districtAd.flatMap(s => if (s.level.equals(DistrictLevel.District)) s.children.map(adCode => streetData.get(adCode).orNull) else List(s))
              .filter(s => s.name.equals(street) || (!isFullMatch && s.shortName.equals(street)))
            if (streetAd.isEmpty) streetAd = districtAd
            streetAd
          }
        }
      }
    }

    adminNodes.map(admin => {
      admin.level match {
        case DistrictLevel.Province => Admin.createProvince(admin.name, admin.id, admin.center)
        case DistrictLevel.City =>
          val province = adminData.get(admin.parentId).orNull
          Admin.createCity(province.name, admin.name, province.id, admin.id, admin.center)
        case DistrictLevel.District =>
          val city = adminData.get(admin.parentId).orNull
          val province = if (city.level == DistrictLevel.City) adminData.get(city.parentId).orNull else city
          Admin.createDistrict(province.name, city.name, admin.name, province.id, city.id, admin.id, admin.center)
        case DistrictLevel.Street =>
          val district = adminData.get(admin.parentId).orNull
          val city = if (district.level == DistrictLevel.District) adminData.get(district.parentId).orNull else district
          val province = if (city.level == DistrictLevel.City) adminData.get(city.parentId).orNull else city
          Admin.createStreet(province.name, city.name, district.name, admin.name, province.id, city.id, district.id, admin.id, admin.center)
        case _ => Admin.createOversea
      }
    }).toSeq
  }

  private def determineAdminCode(lonIn: Double, latIn: Double, coordSys: CoordinateSystem = CoordinateSystem.GCJ02): Int = {
    val gcj02LonLat = GeoUtils.toGCJ02(lonIn, latIn, coordSys)
    val lon = gcj02LonLat._1
    val lat = gcj02LonLat._2

    val s2LatLng = S2LatLng.fromDegrees(lat, lon)
    val id = S2CellId.fromLatLng(s2LatLng).parent(min_level).id()
    val id2 = S2CellId.fromLatLng(s2LatLng).parent(min_level - 2).id()
    if (GeoUtils.outOfChina(lon, lat)) {
      -1
    } else if (boundaryAdminCell.contains(id)) {
      boundaryAdminCell.getOrElse(id, -1)
    } else if (boundaryAdminCell.contains(id2)) {
      boundaryAdminCell.getOrElse(id2, -1)
    } else {
      var keys = List.empty[Long]
      var maxLevel = 2000  //必须大于2000m，否则会出现格子半径过小选择错误问题

      // 最远距离 为新疆哈密市80公里
      while (keys.isEmpty && maxLevel < 200000) {
        keys = S2Utils.getCellId(s2LatLng, maxLevel, min_level)
          .flatMap(key => boundaryIndex.getOrElse(key, List.empty))
        maxLevel = maxLevel * 2
      }
      if (keys.nonEmpty) {
        val lines1 = keys
          .map(key => (key, new S2CellId(key).toLatLng.getEarthDistance(s2LatLng)))
          .sortBy(_._2)
          .take(5)
          .flatMap(startPoint => boundaryData.getOrElse(startPoint._1, List.empty).map(value => (startPoint._1, value._1, value._2, value._3)))
          .map(line => {
            val start = new S2CellId(line._1).toLatLng
            val end = new S2CellId(line._2).toLatLng
            val dis = LineUtils.pointToLineDis(start.lngDegrees(), start.latDegrees(), end.lngDegrees(), end.latDegrees(), lon, lat)
            (((start.lngDegrees(), start.latDegrees()), (end.lngDegrees(), end.latDegrees()), line._3, line._4), dis)
          })

        // 取出所有距离最短的线段
        val minDis = lines1.map(_._2).min
        val lines = lines1.filter(s => s._2 == minDis).map(_._1)
          .groupBy(_._1)
          .maxBy(_._2.size)
          ._2

        if (lines.size == 1) { // 国内海外边界
          val line1 = lines.head
          val start = line1._1
          val end = line1._2
          // 三点用行列式判断旋转方向
          val angle = (start._1 - lon) * (end._2 - lat) - (end._1 - lon) * (start._2 - lat)
          if ((angle < 0) == line1._4) line1._3
          else -1
        } else if (lines.size == 2) { // 两条线段，如果终点不同，则一定是国内和海外，并且点到线段距离最短点为起点，终点相同，则为国内两个区域边界
          val line1 = lines.head
          val line2 = lines.last

          // 终点相同，为国内两个相邻区域，终点不同，为国界线
          val start = if (line1._2.equals(line2._2)) line1._1 else line2._2
          val end = line1._2

          // 三点用行列式判断旋转方向
          val angle = (start._1 - lon) * (end._2 - lat) - (end._1 - lon) * (start._2 - lat)
          if ((angle < 0) == line1._4) line1._3
          else if (line1._2 == line2._2 && line1._4 != line2._4) line2._3
          else -1
        } else { //多区域顶点 判断
          lines.groupBy(_._3).map(s => {
            val line1 = s._2.filter(_._4).head
            val line2 = s._2.filter(!_._4).head

            var start = line2._2
            var end = line1._2
            val point = line1._1

            val dis1 = LineUtils.lineDis(start._1, start._2, point._1, point._2)
            val dis2 = LineUtils.lineDis(end._1, end._2, point._1, point._2)
            if (dis1 > dis2)
              start = (point._1 + dis2 / dis1 * (start._1 - point._1), point._2 + dis2 / dis1 * (start._2 - point._2))
            else
              end = (point._1 + dis1 / dis2 * (end._1 - point._1), point._2 + dis1 / dis2 * (end._2 - point._2))
            val angle = (start._1 - lon) * (end._2 - lat) - (end._1 - lon) * (start._2 - lat)
            (s._1, angle)
          })
            .minBy(_._2)
            ._1
        }
      } else {
        -1
      }
    }
  }

  def determineAreaByAdmin(lon: Double, lat: Double, admin: Admin, radius: Int): BusinessAreaInfo = {
    val businessAreas = determineAreaByCityId(lon, lat, admin.cityID, radius, CoordinateSystem.GCJ02)
    BusinessAreaInfo(admin, businessAreas)
  }

  def determineAreaByCityId(lon: Double, lat: Double, cityID: Int, radius: Int, coordSys: CoordinateSystem): Seq[BusinessArea] = {
    val gcj02LonLat = GeoUtils.toGCJ02(lon, lat, coordSys)
    if (cityID != -1) cityBusinessArea.getOrElse(cityID, Array.empty)
      .map(area => (GeoUtils.distance(area.center, Location(gcj02LonLat._1, gcj02LonLat._2)), area))
      .filter(_._1 <= radius)
      .sortBy(_._1)(Ordering.Double)
      .map(s => BusinessArea(s._2.name, s._2.areaCode, s._1.toInt))
    else Seq.empty
  }
}
