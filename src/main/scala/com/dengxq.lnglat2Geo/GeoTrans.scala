package com.dengxq.lnglat2Geo

import com.dengxq.lnglat2Geo.build.AdminDataProvider
import com.dengxq.lnglat2Geo.entity.CoordinateSystem.CoordinateSystem
import com.dengxq.lnglat2Geo.entity.DistrictLevel.DistrictLevel
import com.dengxq.lnglat2Geo.entity.{Admin, BusinessArea, BusinessAreaInfo, CoordinateSystem, _}
import com.dengxq.lnglat2Geo.utils.GeoUtils

object GeoTrans {

  /**
    * 按需初始化数据, 离线数据处理可以不加载该方法
    *
    * @param needBoundary    是否需要加载边界数据，用于经纬度转换省市区县
    * @param needStreet      是否需要加载街道数据
    * @param needArea        是否需要加载商圈数据
    * @param needCityLevel   是否需要加载城市级别数据
    * @return
    */
  def init(needBoundary: Boolean = true, needStreet: Boolean = true, needArea: Boolean = false, needCityLevel: Boolean = false): Unit = {
    GeoTransImpl.init(needBoundary, needArea, needStreet, needCityLevel)
  }

  /**
    * 判断经纬度的行政区划
    * @param lon 经度
    * @param lat 纬度
    * @param needStreet 是否需要街道信息
    * @param coordSys 输入经纬度的坐标系
    * @return 行政区划
    */
  def determineAdmin(lon: Double, lat: Double, coordSys: CoordinateSystem, needStreet: Boolean = true): Admin = {
    GeoTransImpl.determineAdmin(lon, lat, needStreet, coordSys)
  }

  /**
    * 给出附近的所有商圈信息
    *
    * @param lon 经度
    * @param lat 纬度
    * @param radius 需要商圈的半径
    * @param coordSys 输入经纬度的坐标系
    * @param needStreet 是否需要返回行政区划的街道信息
    * @return
    */
  def aroundBusinessAreas(lon: Double, lat: Double, radius: Int = 4000, coordSys: CoordinateSystem, needStreet: Boolean = true): BusinessAreaInfo = {
    val gcj02LonLat = GeoUtils.toGCJ02(lon, lat, coordSys)
    val admin = determineAdmin(gcj02LonLat._1, gcj02LonLat._2, CoordinateSystem.GCJ02, needStreet)
    GeoTransImpl.determineAreaByAdmin(gcj02LonLat._1, gcj02LonLat._2, admin, radius)
  }

  /**
    * 给出附近的所有商圈信息
    *
    * @param lon 经度
    * @param lat 纬度
    * @param radius 需要商圈的半径
    * @param coordSys 输入经纬度的坐标系
    * @param cityID 输入城市adcode
    * @return
    */
  def aroundBusinessAreasByCityID(lon: Double, lat: Double, radius: Int = 4000, coordSys: CoordinateSystem, cityID: Int): Seq[BusinessArea] = {
    GeoTransImpl.determineAreaByCityId(lon, lat, cityID, radius, coordSys)
  }

  /**
    * 获取城市级别
    * @param adcodeOrName 城市adcode或者城市名
    * @return 城市级别
    */
  def getCityLevel(adcodeOrName: String): String = {
    GeoTransImpl.getCityLevel(adcodeOrName)
  }

  /**
    * 根据地区code返回规范数据
    * @param adcode 地区code
    * @return
    */
  def normalizeName(adcode: Int): AdminNode = {
    GeoTransImpl.normalizeName(adcode)
  }

  /**
    * 根据地区name返回规范化的地区信息
    * @param adcode 地区code
    * @return 规范化的地区信息
    */
  def normalizeName(name: String, level: DistrictLevel): AdminNode = {
    GeoTransImpl.normalizeName(name, level)
  }

  /**
    * 根据所有信息返回规范化的地区信息
    *
    * @param province 省名 可为空
    * @param city 城市名 可为空
    * @param district 区县名 可为空
    * @param street 街道名 可为空
    * @param isFullMatch 所有输入区域是简称还是全名
    * @return 规范化的地区信息，可能有多个或不存在
    */
  def normalizeName(province: String = "", city: String = "", district: String = "", street: String = "", isFullMatch: Boolean = false): Seq[Admin] = {
    GeoTransImpl.normalizeName(province, city, district, street, isFullMatch)
  }

  /**
    * 获取所有行政区划数据
    *
    * @return 所有行政区划数据，不包含街道
    */
  def adminData() : Map[Int, AdminNode] = {
    GeoTransImpl.adminData
  }

  def countryCode() : Map[String, String] = {
    GeoTransImpl.countryCode
  }

  def districtBoundary(): Map[Int, Array[Array[Long]]] = {
    AdminDataProvider.AdminLoader.loadBoundarySrc
  }
}
