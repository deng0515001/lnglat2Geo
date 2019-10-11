package com.dengxq.lnglat2Geo.entity

import scala.util.Try

trait EnumFunc extends Enumeration {
  def exists(code: Int): Boolean = this.values.exists(_.id == code) //检测是否存在此枚举值
  def exists(code: Value): Boolean = this.values.contains(code) //检测是否存在此枚举值
  def exists(name: String): Boolean = Try {this.withName(name); true}.getOrElse(false)
  def getName(code: Int): String = this.values.find(_.id == code).getOrElse("").toString
  def getName(code: Value): String = this.values.find(_.id == code.id).getOrElse("").toString
}

object EnumImplicit {
  implicit def enum2int[E<:Enumeration#Value](enum: E): Int = enum.id
}
@SerialVersionUID(450698729458760556L)
object CoordinateSystem extends EnumFunc {
  type CoordinateSystem = Value
  val WGS84 = Value(0) // GPS 坐标系
  val GCJ02 = Value(1) // 国测局坐标系(火星坐标系)
  val BD09 = Value(2) // 百度坐标系
  val BJ54 = Value(3) // 北京54坐标系
  val XIAN80 = Value(4) // 西安80坐标系
  val CGCS2000 = Value(5) // 2000国家大地坐标系
  val XYZ = Value(6) // 笛卡尔坐标系
  val MERCATOR  = Value(7) // 墨卡托坐标系
}

@SerialVersionUID(638715523836978671L)
object Azimuth extends EnumFunc {
  type Azimuth = Value
  val North = Value(0)
  val NorthEast = Value(45)
  val East = Value(90)
  val SouthEast = Value(135)
  val South = Value(180)
  val SouthWest = Value(225)
  val West = Value(270)
  val NorthWest = Value(315)
}
@SerialVersionUID(-713774920813648035L)
object DistrictLevel extends EnumFunc {
  type DistrictLevel = Value
  /**国家**/
  val Country = Value("country")
  /**省,自治区**/
  val Province = Value("province")
  /**地级市**/
  val City = Value("city")
  /**区,县,县级市**/
  val District = Value("district")
  /**街道**/
  val Street = Value("street")
}

@SerialVersionUID(568714356783645678L)
object AdminLevel {
  /**海外**/
  val Oversea = "oversea"
  /**国家**/
  val Country = "country"
  /**省,自治区**/
  val Province = "province"
  /**地级市**/
  val City = "city"
  /**省辖市(属县级市) see:https://baike.baidu.com/item/省直辖县级行政单位**/
  val ProvincialCity = "provincialcity"
  /**区,县,县级市**/
  val District = "district"
  /**街道**/
  val Street = "street"
}