package com.dengxq.lnglat2Geo.entity

@SerialVersionUID(-1l)
case class AdminBoundary2(var code: Int, var mbr: (Double, Double, Double, Double), var boundary: Array[Array[Long]]) extends Serializable
