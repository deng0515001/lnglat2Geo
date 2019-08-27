package com.dengxq.lnglat2Geo.entity

import com.dengxq.lnglat2Geo.entity.DistrictLevel.DistrictLevel

@SerialVersionUID(5953969594271885955L)
case class AdminNode(
                      var id: Int,
                      name: String,
                      shortName:String,
                      center: Location,
                      level: DistrictLevel,
                      parentId: Int,
                      var children: List[Int]
                    ) extends Serializable
