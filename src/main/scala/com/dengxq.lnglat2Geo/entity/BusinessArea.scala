package com.dengxq.lnglat2Geo.entity

/**
  * 商圈信息
  * @param name 商圈名称
  * @param center 商圈中心点
  * @param areaCode 商圈ID
  * @param parentId 所在行政区划id
  */
@SerialVersionUID(-6474937368799868168L)
case class BusinessAreaData(name: String, center: Location, areaCode: Int) extends Serializable

@SerialVersionUID(-5899680396800964972L)
case class BusinessAreaGroup(cityAdCode: Int, areas: Array[BusinessAreaData]) extends Serializable

case class BusinessArea(name: String, areaCode: Int, distance:Int)

case class BusinessAreaInfo(admin: Admin, areas: Seq[BusinessArea])